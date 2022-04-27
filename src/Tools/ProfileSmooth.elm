module Tools.ProfileSmooth exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import Point3d exposing (zCoordinate)
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (multiplyBy, zero)
import String.Interpolate
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.ProfileSmoothOptions exposing (..)
import TrackLoaded exposing (TrackLoaded, adjustAltitude)
import UtilsForViews exposing (showDecimal0)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


toolId =
    "profilesmooth"


type Msg
    = LimitGradient
    | SmoothAltitudes
    | SmoothGradients
    | SetMaximumAscent Float
    | SetMaximumDescent Float
    | SetExtent ExtentOption
    | SetWindowSize Int
    | ChooseMethod SmoothMethod
    | SetRedistribution Bool
    | DisplayInfo String String


defaultOptions : Options
defaultOptions =
    { smoothMethod = MethodLimit
    , extent = ExtentIsRange
    , previewData = Nothing
    , maximumAscent = 15.0
    , maximumDescent = 15.0
    , windowSize = 2
    , limitRedistributes = False
    }


actions : Options -> Element.Color -> TrackLoaded msg -> List (ToolAction msg)
actions newOptions previewColour track =
    case newOptions.previewData of
        Just previewTree ->
            let
                ( start, end ) =
                    case track.markerPosition of
                        Just _ ->
                            TrackLoaded.getRangeFromMarkers track

                        Nothing ->
                            ( 0, 0 )

                normalPreview =
                    TrackLoaded.previewFromTree
                        previewTree
                        start
                        (skipCount previewTree - end)
                        10

                ( newTreeForProfilePreview, _ ) =
                    apply newOptions track
            in
            case newTreeForProfilePreview of
                Just newTree ->
                    [ ShowPreview
                        { tag = "limit"
                        , shape = PreviewCircle
                        , colour = previewColour
                        , points = normalPreview
                        }
                    , ShowPreview
                        { tag = "limitProfile"
                        , shape = PreviewProfile newTree
                        , colour = previewColour
                        , points = []
                        }
                    , RenderProfile
                    ]

                Nothing ->
                    [ HidePreview "limit", HidePreview "limitprofile" ]

        Nothing ->
            [ HidePreview "limit", HidePreview "limitprofile" ]


putPreviewInOptions : TrackLoaded msg -> Options -> Options
putPreviewInOptions track options =
    let
        adjustedPoints =
            computeNewPoints options track
    in
    { options
        | previewData =
            DomainModel.treeFromSourcesWithExistingReference
                (DomainModel.gpxPointFromIndex 0 track.trackTree)
                (List.map Tuple.second adjustedPoints)
    }


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        SetExtent extent ->
            let
                newOptions =
                    { options | extent = extent }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        SetMaximumAscent up ->
            let
                newOptions =
                    { options | maximumAscent = up }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        SetMaximumDescent down ->
            let
                newOptions =
                    { options | maximumDescent = down }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        LimitGradient ->
            ( options
            , [ Actions.LimitGradientWithOptions options
              , TrackHasChanged
              ]
            )

        SmoothAltitudes ->
            ( options
            , [ Actions.SmoothAltitudes options
              , TrackHasChanged
              ]
            )

        SmoothGradients ->
            ( options
            , [ Actions.SmoothGradients options
              , TrackHasChanged
              ]
            )

        SetWindowSize size ->
            let
                newOptions =
                    { options | windowSize = size }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ChooseMethod smoothMethod ->
            let
                newOptions =
                    { options | smoothMethod = smoothMethod }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        SetRedistribution flag ->
            let
                newOptions =
                    { options | limitRedistributes = flag }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            ( 0, 0 )

        newCourse =
            computeNewPoints options track
                |> List.map Tuple.second

        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                newCourse
                track.trackTree

        oldPoints =
            DomainModel.extractPointsInRange
                fromStart
                fromEnd
                track.trackTree
    in
    ( newTree
    , oldPoints |> List.map Tuple.second
    )


type SlopeStatus
    = Clamped RoadSection Float
    | NotClamped RoadSection Length.Length


type alias SlopeStuff =
    { roads : List SlopeStatus
    , totalClamped : Length.Length -- altitude shortfall
    , totalOffered : Length.Length -- how much sections have to spare
    }


emptySlopeStuff : SlopeStuff
emptySlopeStuff =
    { roads = [], totalClamped = zero, totalOffered = zero }


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    case options.smoothMethod of
        MethodLimit ->
            if options.limitRedistributes then
                limitGradientsWithRedistribution options track

            else
                simpleLimitGradients options track

        MethodGradients ->
            averageGradientsWithWindow options track

        MethodAltitudes ->
            smoothAltitudesWithWindowAverage options track


limitGradientsWithRedistribution : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
limitGradientsWithRedistribution options track =
    {-
       This method attempts to find other opportunities to make up for the lost
       altitude changes, so as to preserve the atitudes at the end points.
       That, of course, is not always possible.
    -}
    let
        ( fromStart, fromEnd ) =
            -- Range over which to apply the clamp.
            if track.markerPosition == Nothing then
                ( 0, 0 )

            else
                TrackLoaded.getRangeFromMarkers track

        endIndex =
            skipCount track.trackTree - fromEnd

        ( startDistance, startAltitude ) =
            ( distanceFromIndex fromStart track.trackTree
            , earthPointFromIndex fromStart track.trackTree |> Point3d.zCoordinate
            )

        ( endDistance, endAltitude ) =
            ( distanceFromIndex endIndex track.trackTree
            , earthPointFromIndex endIndex track.trackTree |> Point3d.zCoordinate
            )

        averageSlope =
            if
                (endAltitude |> Quantity.equalWithin Length.centimeter startAltitude)
                    || (endDistance |> Quantity.equalWithin Length.centimeter startDistance)
            then
                -- Don't entirely trust Quantity.ratio
                0.0

            else
                Quantity.ratio
                    (endAltitude |> Quantity.minus startAltitude)
                    (endDistance |> Quantity.minus startDistance)

        slopeDiscoveryFn : RoadSection -> SlopeStuff -> SlopeStuff
        slopeDiscoveryFn road slopeStuff =
            let
                altitudeChange : Length.Length
                altitudeChange =
                    zCoordinate road.endPoint
                        |> Quantity.minus (zCoordinate road.startPoint)

                clampedSlope : Float
                clampedSlope =
                    -- Easier to use fractions here.
                    0.01
                        * clamp (0 - options.maximumDescent)
                            options.maximumAscent
                            road.gradientAtStart

                altitudeGap : Length.Length
                altitudeGap =
                    altitudeChange
                        |> Quantity.minus
                            (road.trueLength |> multiplyBy clampedSlope)

                altitudeIfAverageSlope : Length.Length
                altitudeIfAverageSlope =
                    road.trueLength |> multiplyBy averageSlope

                availableToOffer : Length.Length
                availableToOffer =
                    altitudeIfAverageSlope |> Quantity.minus altitudeChange

                thisSectionSummary : SlopeStatus
                thisSectionSummary =
                    if
                        road.gradientAtStart
                            <= options.maximumAscent
                            && road.gradientAtStart
                            >= (0 - options.maximumDescent)
                    then
                        NotClamped road availableToOffer

                    else
                        Clamped road clampedSlope
            in
            { roads = thisSectionSummary :: slopeStuff.roads
            , totalClamped = altitudeGap |> Quantity.plus slopeStuff.totalClamped
            , totalOffered = availableToOffer |> Quantity.plus slopeStuff.totalOffered
            }

        slopeInfo =
            traverseTreeBetweenLimitsToDepth
                fromStart
                (skipCount track.trackTree - fromEnd)
                (always Nothing)
                0
                track.trackTree
                slopeDiscoveryFn
                emptySlopeStuff

        proRataToAllocate =
            if
                (slopeInfo.totalClamped |> Quantity.equalWithin Length.centimeter Quantity.zero)
                    || (slopeInfo.totalOffered |> Quantity.equalWithin Length.centimeter Quantity.zero)
            then
                0.0

            else
                Quantity.ratio slopeInfo.totalOffered slopeInfo.totalClamped

        allocateProRata :
            SlopeStatus
            -> ( Length.Length, List ( EarthPoint, GPXSource ) )
            -> ( Length.Length, List ( EarthPoint, GPXSource ) )
        allocateProRata section ( altitude, outputs ) =
            -- Note that sections are reversed so we are adjusting start altitude
            -- working backwards from the end marker.
            let
                ( earth, gpx ) =
                    case section of
                        Clamped roadSection slope ->
                            -- Apply clamped slope
                            let
                                altitudeChange =
                                    roadSection.trueLength
                                        |> multiplyBy slope

                                newStartAltitude =
                                    altitude |> Quantity.minus altitudeChange

                                newStartPoint =
                                    adjustAltitude newStartAltitude roadSection.startPoint

                                baseGPX =
                                    roadSection.sourceData |> Tuple.first
                            in
                            ( newStartPoint, { baseGPX | altitude = newStartAltitude } )

                        NotClamped roadSection offered ->
                            -- Apply pro-rata
                            let
                                altitudeChange =
                                    roadSection.trueLength
                                        |> multiplyBy (roadSection.gradientAtStart / 100.0)
                                        |> Quantity.plus (offered |> multiplyBy proRataToAllocate)

                                newStartAltitude =
                                    altitude |> Quantity.minus altitudeChange

                                newStartPoint =
                                    adjustAltitude newStartAltitude roadSection.startPoint

                                baseGPX =
                                    roadSection.sourceData |> Tuple.first
                            in
                            ( newStartPoint, { baseGPX | altitude = newStartAltitude } )
            in
            ( gpx.altitude, ( earth, gpx ) :: outputs )

        ( _, adjustedPoints ) =
            slopeInfo.roads |> List.foldl allocateProRata ( endAltitude, [] )
    in
    adjustedPoints


type alias AverageSmoothState =
    { leading : List ( EarthPoint, GPXSource ) -- head = oldest
    , trailing : List ( EarthPoint, GPXSource ) -- same, head = most recent
    , outputs : List ( EarthPoint, GPXSource )
    , index : Int
    }


smoothAltitudesWithWindowAverage : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
smoothAltitudesWithWindowAverage options track =
    {-
       Use a running average of altitude.
    -}
    let
        ( fromStart, fromEnd ) =
            -- Range over which to apply the clamp.
            if track.markerPosition == Nothing then
                ( 0, 0 )

            else
                TrackLoaded.getRangeFromMarkers track

        endIndex =
            skipCount track.trackTree - fromEnd

        startState : AverageSmoothState
        startState =
            { leading = []
            , trailing = []
            , outputs = [ getDualCoords track.trackTree 0 ]
            , index = 0
            }

        slidingWindowSnoother :
            RoadSection
            -> AverageSmoothState
            -> AverageSmoothState
        slidingWindowSnoother road { leading, trailing, outputs, index } =
            let
                extendedLeadingBuffer =
                    leading ++ [ ( road.endPoint, Tuple.second road.sourceData ) ]

                ( newLeading, newTrailing ) =
                    if List.length extendedLeadingBuffer > options.windowSize then
                        -- Move one across to the trailing side
                        ( List.drop 1 extendedLeadingBuffer
                        , List.take 1 extendedLeadingBuffer ++ trailing
                        )

                    else
                        ( extendedLeadingBuffer, trailing )

                newOutputs =
                    -- Start outputting when leading is full (trailing is not empty).
                    case newTrailing of
                        [] ->
                            outputs

                        ( justPassedEarthPoint, justPassedGpx ) :: others ->
                            if
                                index
                                    - options.windowSize
                                    > fromStart
                                    && index
                                    - options.windowSize
                                    < endIndex
                            then
                                let
                                    mergeListsForAltitude =
                                        (newLeading ++ newTrailing)
                                            |> List.map (zCoordinate << Tuple.first)

                                    averageAltitude =
                                        Quantity.sum mergeListsForAltitude
                                            |> Quantity.divideBy (toFloat <| List.length mergeListsForAltitude)

                                    revisedEarthPoint =
                                        adjustAltitude averageAltitude justPassedEarthPoint

                                    revisedGpx =
                                        { justPassedGpx | altitude = averageAltitude }
                                in
                                ( revisedEarthPoint, revisedGpx ) :: outputs

                            else
                                -- Outside range, just pass through
                                ( justPassedEarthPoint, justPassedGpx ) :: outputs
            in
            { leading = newLeading
            , trailing = List.take options.windowSize <| newTrailing
            , outputs = newOutputs
            , index = index + 1
            }

        flusher : AverageSmoothState -> List ( EarthPoint, GPXSource )
        flusher { leading, trailing, outputs, index } =
            -- We have stopped receiving any more at the leading edge, but we must
            -- process what we have acquired.
            case leading of
                [] ->
                    List.reverse outputs

                ( justPassedEarthPoint, justPassedGpx ) :: others ->
                    let
                        mergeListsForAltitude =
                            (leading ++ trailing)
                                |> List.map (zCoordinate << Tuple.first)

                        averageAltitude =
                            Quantity.sum mergeListsForAltitude
                                |> Quantity.divideBy (toFloat <| List.length mergeListsForAltitude)

                        ( revisedEarthPoint, revisedGpx ) =
                            if index > fromStart && index < endIndex then
                                ( adjustAltitude averageAltitude justPassedEarthPoint
                                , { justPassedGpx | altitude = averageAltitude }
                                )

                            else
                                ( justPassedEarthPoint, justPassedGpx )
                    in
                    flusher
                        { leading = List.drop 1 leading
                        , trailing = List.take 1 leading ++ trailing
                        , outputs = ( revisedEarthPoint, revisedGpx ) :: outputs
                        , index = index + 1
                        }

        finalState =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                (always Nothing)
                0
                track.trackTree
                slidingWindowSnoother
                startState
    in
    flusher finalState


type alias GradientSmootherState =
    { leading : List RoadSection -- head = oldest
    , trailing : List RoadSection -- same, head = most recent
    , outputs : List ( EarthPoint, GPXSource )
    , lastAltitude : Length.Length
    , index : Int
    }


averageGradientsWithWindow : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
averageGradientsWithWindow options track =
    {-
       Use a running average of gradients and derive altitudes.
    -}
    let
        ( fromStart, fromEnd ) =
            -- Range over which to apply the clamp.
            if track.markerPosition == Nothing then
                ( 0, 0 )

            else
                TrackLoaded.getRangeFromMarkers track

        endIndex =
            skipCount track.trackTree - fromEnd

        startState : GradientSmootherState
        startState =
            { leading = []
            , trailing = []
            , outputs = [ getDualCoords track.trackTree 0 ]
            , lastAltitude = gpxPointFromIndex 0 track.trackTree |> .altitude
            , index = 0
            }

        slidingWindowSnoother :
            RoadSection
            -> GradientSmootherState
            -> GradientSmootherState
        slidingWindowSnoother road { leading, trailing, outputs, lastAltitude, index } =
            let
                extendedLeadingBuffer =
                    leading ++ [ road ]

                ( newLeading, newTrailing ) =
                    if List.length extendedLeadingBuffer > options.windowSize then
                        -- Move one across to the trailing side
                        ( List.drop 1 extendedLeadingBuffer
                        , List.take 1 extendedLeadingBuffer ++ trailing
                        )

                    else
                        ( extendedLeadingBuffer, trailing )

                ( newOutputs, nextAltitude ) =
                    case newTrailing of
                        [] ->
                            ( outputs, lastAltitude )

                        justPassedRoad :: others ->
                            let
                                mergeListsForGradient =
                                    -- At this stage, newTrailing has the "current" entry.
                                    (newLeading ++ newTrailing)
                                        |> List.map .gradientAtStart

                                averageGradient =
                                    List.sum mergeListsForGradient / (toFloat <| List.length mergeListsForGradient)

                                newAltitude =
                                    justPassedRoad.trueLength
                                        |> multiplyBy (averageGradient / 100.0)
                                        |> Quantity.plus lastAltitude

                                justPassedGpx =
                                    Tuple.second justPassedRoad.sourceData

                                ( revisedEarthPoint, revisedGpx ) =
                                    if
                                        index
                                            - options.windowSize
                                            > fromStart
                                            && index
                                            - options.windowSize
                                            < endIndex
                                    then
                                        ( adjustAltitude newAltitude justPassedRoad.endPoint
                                        , { justPassedGpx | altitude = newAltitude }
                                        )

                                    else
                                        ( justPassedRoad.endPoint, justPassedGpx )
                            in
                            ( ( revisedEarthPoint, revisedGpx ) :: outputs
                            , newAltitude
                            )
            in
            { leading = newLeading
            , trailing = List.take options.windowSize <| newTrailing
            , outputs = newOutputs
            , lastAltitude = nextAltitude
            , index = index + 1
            }

        flusher : GradientSmootherState -> List ( EarthPoint, GPXSource )
        flusher { leading, trailing, outputs, lastAltitude, index } =
            -- We have stopped receiving any more at the leading edge, but we must
            -- process what we have acquired.
            case leading of
                [] ->
                    List.reverse outputs

                justPassedRoad :: others ->
                    if index > fromStart && index < endIndex then
                        let
                            mergeListsForGradient =
                                (leading ++ trailing)
                                    |> List.map .gradientAtStart

                            averageGradient =
                                List.sum mergeListsForGradient / (toFloat <| List.length mergeListsForGradient)

                            newAltitude =
                                justPassedRoad.trueLength
                                    |> multiplyBy (averageGradient / 100.0)
                                    |> Quantity.plus lastAltitude

                            revisedEarthPoint =
                                adjustAltitude newAltitude justPassedRoad.endPoint

                            justPassedGpx =
                                Tuple.second justPassedRoad.sourceData

                            revisedGpx =
                                { justPassedGpx | altitude = newAltitude }
                        in
                        flusher
                            { leading = List.drop 1 leading
                            , trailing = List.take 1 leading ++ trailing
                            , outputs = ( revisedEarthPoint, revisedGpx ) :: outputs
                            , lastAltitude = newAltitude
                            , index = index + 1
                            }

                    else
                        -- Outside markers, passthrough
                        flusher
                            { leading = List.drop 1 leading
                            , trailing = List.take 1 leading ++ trailing
                            , outputs =
                                ( justPassedRoad.endPoint
                                , Tuple.second justPassedRoad.sourceData
                                )
                                    :: outputs
                            , lastAltitude = zCoordinate justPassedRoad.endPoint
                            , index = index + 1
                            }

        finalState =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                (always Nothing)
                0
                track.trackTree
                slidingWindowSnoother
                startState
    in
    flusher finalState


simpleLimitGradients : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
simpleLimitGradients options track =
    {-
       This method simply clamps the gradients and works out the resulting altitudes.
       It's what Vue GPX Smoother does.
       Interestingly, we must continue to the track end even when finished clamping.
       (Or we could make this whole track only.)
       BTW, for a section, I'm going to e lazy and always just process the whole
       track, because we sort of need it for the previews. (We don't, it's debt)
    -}
    let
        ( fromStart, fromEnd ) =
            -- Range over which to apply the clamp.
            if track.markerPosition == Nothing then
                ( 0, 0 )

            else
                TrackLoaded.getRangeFromMarkers track

        endIndex =
            skipCount track.trackTree - fromEnd

        startAltitude =
            gpxPointFromIndex 0 track.trackTree |> .altitude

        clamper :
            RoadSection
            -> ( Int, Length.Length, List ( EarthPoint, GPXSource ) )
            -> ( Int, Length.Length, List ( EarthPoint, GPXSource ) )
        clamper road ( index, lastAltitude, outputs ) =
            let
                newGradient =
                    if index > fromStart && index < endIndex then
                        clamp
                            (0 - options.maximumDescent)
                            options.maximumAscent
                            road.gradientAtStart

                    else
                        road.gradientAtStart

                newEndAltitude =
                    road.trueLength
                        |> Quantity.multiplyBy (newGradient / 100.0)
                        |> Quantity.plus lastAltitude

                newEarthPoint =
                    adjustAltitude newEndAltitude road.endPoint

                currentGpx =
                    Tuple.second road.sourceData

                newGpx =
                    { currentGpx | altitude = newEndAltitude }
            in
            ( index + 1, newEndAltitude, ( newEarthPoint, newGpx ) :: outputs )

        ( _, _, adjustedPoints ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                (always Nothing)
                0
                track.trackTree
                clamper
                ( 0, startAltitude, [ getDualCoords track.trackTree 0 ] )
    in
    List.reverse adjustedPoints


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            let
                newOptions =
                    putPreviewInOptions theTrack options
            in
            ( newOptions
            , actions newOptions colour theTrack
            )

        _ ->
            ( options, [ HidePreview "limit", HidePreview "limitProfile" ] )


view : I18NOptions.Location -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view location options wrapper track =
    let
        i18n =
            I18N.text location toolId

        maxAscentSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMaximumAscent
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "uphill")
                                [ showDecimal0 options.maximumAscent ]
                , min = 10.0
                , max = 25.0
                , step = Just 1.0
                , value = options.maximumAscent
                , thumb = Input.defaultThumb
                }

        maxDescentSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMaximumDescent
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "downhill")
                                [ showDecimal0 options.maximumDescent ]
                , min = 10.0
                , max = 25.0
                , step = Just 1.0
                , value = options.maximumDescent
                , thumb = Input.defaultThumb
                }

        windowSizeSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetWindowSize << round
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "window")
                                [ String.fromInt options.windowSize ]
                , min = 1.0
                , max = 8.0
                , step = Just 1.0
                , value = toFloat options.windowSize
                , thumb = Input.defaultThumb
                }

        limitGradientsMethod =
            column [ spacing 10, centerX ]
                [ el [ centerX ] <| maxAscentSlider
                , el [ centerX ] <| maxDescentSlider
                , extent
                , el [ centerX ] <|
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| LimitGradient
                        , label = paragraph [] [ i18n "apply" ]
                        }
                ]

        smoothAltitudes =
            column [ spacing 10, centerX ]
                [ el [ centerX ] <| windowSizeSlider
                , extent
                , el [ centerX ] <|
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| SmoothAltitudes
                        , label = paragraph [] [ i18n "altitudes" ]
                        }
                ]

        smoothGradients =
            column [ spacing 10, centerX ]
                [ el [ centerX ] <| windowSizeSlider
                , extent
                , el [ centerX ] <|
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| SmoothGradients
                        , label = paragraph [] [ i18n "gradients" ]
                        }
                ]

        modeChoice =
            Input.radio
                [ padding 10
                , spacing 10
                ]
                { onChange = wrapper << ChooseMethod
                , selected = Just options.smoothMethod
                , label = Input.labelHidden "Method"
                , options =
                    [ Input.option MethodLimit (i18n "uselimit")
                    , Input.option MethodAltitudes (i18n "usealts")
                    , Input.option MethodGradients (i18n "usegrad")
                    ]
                }

        extent =
            paragraph [] <|
                if track.markerPosition == Nothing then
                    [ i18n "whole" ]

                else
                    [ i18n "part" ]
    in
    wrappedRow
        [ spacing 6
        , padding 6
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , width fill
        ]
        [ modeChoice
        , case options.smoothMethod of
            MethodLimit ->
                limitGradientsMethod

            MethodAltitudes ->
                smoothAltitudes

            MethodGradients ->
                smoothGradients
        ]
