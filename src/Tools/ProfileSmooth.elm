module Tools.ProfileSmooth exposing (Msg(..), SlopeStatus(..), SlopeStuff, apply, defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length
import Point3d exposing (zCoordinate)
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (multiplyBy, zero)
import String.Interpolate
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.ProfileSmoothOptions exposing (..)
import TrackLoaded exposing (TrackLoaded, adjustAltitude)
import UtilsForViews exposing (showDecimal0)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder)


toolId =
    "profilesmooth"


type Msg
    = ApplyPreview
    | SetMaximumAscent Float
    | SetMaximumDescent Float
    | SetWindowSize Int
    | SetBumpiness Float
    | ChooseMethod SmoothMethod


defaultOptions : Options
defaultOptions =
    { smoothMethod = MethodLimit
    , extent = ExtentIsRange
    , previewData = Nothing
    , maximumAscent = 15.0
    , maximumDescent = 15.0
    , windowSize = 2
    , limitRedistributes = False
    , bumpiness = 0.0
    }


previewActions : Options -> Element.Color -> TrackLoaded msg -> List (ToolAction msg)
previewActions newOptions previewColour track =
    let
        ( start, end ) =
            case track.markerPosition of
                Just _ ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )
    in
    case newOptions.previewData of
        Just previewTree ->
            let
                normalPreview =
                    TrackLoaded.previewFromTree
                        previewTree
                        start
                        (skipCount previewTree - end)
                        10
            in
            [ ShowPreview
                { tag = "limit"
                , shape = PreviewCircle
                , colour = previewColour
                , points = normalPreview
                }
            , ShowPreview
                { tag = "limitProfile"
                , shape = PreviewProfile previewTree
                , colour = previewColour
                , points = []
                }
            ]

        Nothing ->
            [ HidePreview "limit", HidePreview "limitprofile" ]


previewWithNewOptions : TrackLoaded msg -> Options -> Options
previewWithNewOptions track options =
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
        SetMaximumAscent up ->
            let
                newOptions =
                    { options | maximumAscent = up }
                        |> previewWithNewOptions track
            in
            ( newOptions
            , previewActions newOptions previewColour track
            )

        SetMaximumDescent down ->
            let
                newOptions =
                    { options | maximumDescent = down }
                        |> previewWithNewOptions track
            in
            ( newOptions
            , previewActions newOptions previewColour track
            )

        SetBumpiness bumpiness ->
            let
                newOptions =
                    { options | bumpiness = bumpiness }
                        |> previewWithNewOptions track
            in
            ( newOptions
            , previewActions newOptions previewColour track
            )

        ApplyPreview ->
            let
                undoInfo =
                    TrackLoaded.undoInfoWithWholeTrackDefault
                        (Actions.ApplySmoothProfile options)
                        track
            in
            ( options
            , [ WithUndo undoInfo
              , undoInfo.action
              , TrackHasChanged
              ]
            )

        SetWindowSize size ->
            let
                newOptions =
                    { options | windowSize = size }
                        |> previewWithNewOptions track
            in
            ( newOptions
            , previewActions newOptions previewColour track
            )

        ChooseMethod smoothMethod ->
            let
                newOptions =
                    { options | smoothMethod = smoothMethod }
                        |> previewWithNewOptions track
            in
            ( newOptions
            , previewActions newOptions previewColour track
            )


apply : Options -> TrackLoaded msg -> Maybe PeteTree
apply options track =
    let
        ( fromStart, fromEnd ) =
            case track.markerPosition of
                Just _ ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )

        newCourse =
            computeNewPoints options track
                |> List.map Tuple.second

        newTree =
            DomainModel.replaceRange
                0
                0
                track.referenceLonLat
                newCourse
                track.trackTree
    in
    newTree


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

        MethodUniform ->
            useUniformGradient options.bumpiness track


useUniformGradient : Float -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
useUniformGradient bumpiness track =
    -- This is the original v1 bump smoother. Astonished that people want this.
    -- This implementation uses a fold over the whole track, really just to be
    -- consistent with others here and because it's not really less efficient
    -- than multiple modifications in situ, each of which essentially builds a new tree.
    let
        ( startIndex, fromEnd ) =
            -- Note that this option is permitted only when there's a range.
            TrackLoaded.getRangeFromMarkers track

        endIndex =
            skipCount track.trackTree - fromEnd

        ( startDistance, endDistance ) =
            ( distanceFromIndex startIndex track.trackTree
            , distanceFromIndex endIndex track.trackTree
            )

        ( startAltitude, endAltitude ) =
            ( gpxPointFromIndex startIndex track.trackTree |> .altitude
            , gpxPointFromIndex endIndex track.trackTree |> .altitude
            )

        rangeLength =
            endDistance |> Quantity.minus startDistance

        uniformSmoother : RoadSection -> ( Int, List ( EarthPoint, GPXSource ) ) -> ( Int, List ( EarthPoint, GPXSource ) )
        uniformSmoother road ( index, outputs ) =
            let
                newPoint =
                    if index > startIndex && index < endIndex then
                        let
                            oldGPX =
                                Tuple.second road.sourceData

                            distanceHere =
                                distanceFromIndex index track.trackTree

                            ( distanceFromStart, _ ) =
                                ( distanceHere |> Quantity.minus startDistance
                                , endDistance |> Quantity.minus distanceHere
                                )

                            proportionFromStart =
                                Quantity.ratio distanceFromStart rangeLength

                            altitudeIfUniform =
                                Quantity.plus
                                    (endAltitude |> Quantity.multiplyBy proportionFromStart)
                                    (startAltitude |> Quantity.multiplyBy (1.0 - proportionFromStart))

                            newAltitude =
                                Quantity.plus
                                    (altitudeIfUniform |> Quantity.multiplyBy (1.0 - bumpiness))
                                    (oldGPX.altitude |> Quantity.multiplyBy bumpiness)
                        in
                        ( { space =
                                Point3d.xyz
                                    (Point3d.xCoordinate road.endPoint.space)
                                    (Point3d.yCoordinate road.endPoint.space)
                                    newAltitude
                          , time = road.endPoint.time
                          }
                        , { oldGPX | altitude = newAltitude }
                        )

                    else
                        ( { space = road.endPoint.space
                          , time = road.endPoint.time
                          }
                        , Tuple.second road.sourceData
                        )
            in
            ( index + 1, newPoint :: outputs )
    in
    if track.markerPosition == Nothing then
        []

    else
        List.reverse <|
            Tuple.second <|
                DomainModel.traverseTreeBetweenLimitsToDepth
                    0
                    (skipCount track.trackTree)
                    (always Nothing)
                    0
                    track.trackTree
                    uniformSmoother
                    ( 1, [ getDualCoords track.trackTree 0 ] )


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
            , earthPointFromIndex fromStart track.trackTree |> .space |> Point3d.zCoordinate
            )

        ( endDistance, endAltitude ) =
            ( distanceFromIndex endIndex track.trackTree
            , earthPointFromIndex endIndex track.trackTree |> .space |> Point3d.zCoordinate
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
                    zCoordinate road.endPoint.space
                        |> Quantity.minus (zCoordinate road.startPoint.space)

                clampedSlope : Float
                clampedSlope =
                    -- Easier to use fractions here.
                    0.01
                        * clamp -options.maximumDescent
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
                            >= -options.maximumDescent
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

                        ( justPassedEarthPoint, justPassedGpx ) :: _ ->
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
                                            |> List.map (zCoordinate << .space << Tuple.first)

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

                ( justPassedEarthPoint, justPassedGpx ) :: _ ->
                    let
                        ( revisedEarthPoint, revisedGpx ) =
                            if index > fromStart && index < endIndex then
                                let
                                    mergeListsForAltitude =
                                        (leading ++ trailing)
                                            |> List.map (zCoordinate << .space << Tuple.first)

                                    averageAltitude =
                                        Quantity.sum mergeListsForAltitude
                                            |> Quantity.divideBy (toFloat <| List.length mergeListsForAltitude)
                                in
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

        slidingWindowSmoother :
            RoadSection
            -> GradientSmootherState
            -> GradientSmootherState
        slidingWindowSmoother road { leading, trailing, outputs, lastAltitude, index } =
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

                        justPassedRoad :: _ ->
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

                justPassedRoad :: _ ->
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
                            , lastAltitude = zCoordinate justPassedRoad.endPoint.space
                            , index = index + 1
                            }

        finalState =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                (always Nothing)
                0
                track.trackTree
                slidingWindowSmoother
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
                            -options.maximumDescent
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
                    previewWithNewOptions theTrack options
            in
            ( newOptions
            , previewActions newOptions colour theTrack
            )

        _ ->
            ( options, [ HidePreview "limit", HidePreview "limitProfile" ] )


view : I18NOptions.Location -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view location options wrapper track =
    let
        i18n =
            I18N.text location toolId

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
                    , Input.option MethodUniform (i18n "useuniform")
                    ]
                }

        extent =
            paragraph [] <|
                if track.markerPosition == Nothing then
                    [ i18n "whole" ]

                else
                    [ i18n "part" ]

        applyButton tag =
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| ApplyPreview
                , label = paragraph [] [ i18n tag ]
                }
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
                let
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
                in
                column [ spacing 10, centerX ]
                    [ el [ centerX ] <| maxAscentSlider
                    , el [ centerX ] <| maxDescentSlider
                    , extent
                    , applyButton "apply"
                    ]

            MethodAltitudes ->
                column [ spacing 10, centerX ]
                    [ el [ centerX ] <| windowSizeSlider
                    , extent
                    , el [ centerX ] <|
                        applyButton "altitudes"
                    ]

            MethodGradients ->
                column [ spacing 10, centerX ]
                    [ el [ centerX ] <| windowSizeSlider
                    , extent
                    , el [ centerX ] <|
                        applyButton "gradients"
                    ]

            MethodUniform ->
                let
                    bumpinessSlider =
                        Input.slider
                            commonShortHorizontalSliderStyles
                            { onChange = wrapper << SetBumpiness
                            , label =
                                Input.labelBelow [] <|
                                    text <|
                                        String.Interpolate.interpolate
                                            (I18N.localisedString location toolId "bumpiness")
                                            [ showDecimal0 <| 100.0 * options.bumpiness ]
                            , min = 0.0
                            , max = 1.0
                            , step = Just 0.05
                            , value = options.bumpiness
                            , thumb = Input.defaultThumb
                            }
                in
                column [ spacing 10, centerX ]
                    [ el [ centerX ] <| bumpinessSlider
                    , paragraph [] <|
                        if track.markerPosition == Nothing then
                            [ i18n "needpart" ]

                        else
                            [ i18n "part" ]
                    , applyButton "uniform"
                    ]
        ]
