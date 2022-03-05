module Tools.ProfileSmooth exposing (..)

import Actions exposing (ToolAction(..))
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import Point3d exposing (zCoordinate)
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (multiplyBy, zero)
import Tools.ProfileSmoothOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


type Msg
    = LimitGradient
    | SetMaximumAscent Float
    | SetMaximumDescent Float
    | SetExtent ExtentOption
    | SetProcessNoise Float
    | SetMeasurementNoise Float
    | SetDeltaSlope Bool
    | SetWindowSize Int
    | ChooseMethod SmoothMethod
    | SetRedistribution Bool


defaultOptions : Options
defaultOptions =
    { smoothMethod = MethodLimit
    , extent = ExtentIsRange
    , previewData = Nothing
    , processNoise = 0.01
    , measurementNoise = 3.0
    , useDeltaSlope = False
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
                normalPreview =
                    TrackLoaded.previewFromTree
                        previewTree
                        0
                        (skipCount track.trackTree)
                        10

                ( newTreeForProfilePreview, _ ) =
                    apply newOptions track
            in
            [ ShowPreview
                { tag = "limit"
                , shape = PreviewCircle
                , colour = previewColour
                , points = normalPreview
                }
            , case newTreeForProfilePreview of
                Just newTree ->
                    ShowPreview
                        { tag = "limitProfile"
                        , shape = PreviewProfile newTree
                        , colour = previewColour
                        , points = []
                        }

                Nothing ->
                    NoAction
            , RenderProfile
            ]

        Nothing ->
            [ HidePreview "limit" ]


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

        SetProcessNoise noise ->
            let
                newOptions =
                    { options | processNoise = noise }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        SetMeasurementNoise noise ->
            let
                newOptions =
                    { options | measurementNoise = noise }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        SetDeltaSlope delta ->
            let
                newOptions =
                    { options | useDeltaSlope = delta }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
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

        MethodGaussian ->
            []


limitGradientsWithRedistribution : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
limitGradientsWithRedistribution options track =
    {-
       This method attempts to find other opportunities to make up for the lost
       altitude changes, so as to preserve the atitudes at the end points.
       That, of course, is not always possible.
    -}
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

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
            --TODO: Check if we should drop the startmost point.
            slopeInfo.roads |> List.foldl allocateProRata ( endAltitude, [] )
    in
    adjustedPoints


adjustAltitude : Length.Length -> EarthPoint -> EarthPoint
adjustAltitude alt pt =
    Point3d.xyz
        (Point3d.xCoordinate pt)
        (Point3d.yCoordinate pt)
        alt


type alias AverageSmoothState =
    { leading : List ( EarthPoint, GPXSource ) -- head = oldest
    , trailing : List ( EarthPoint, GPXSource ) -- same, head = most recent
    , outputs : List ( EarthPoint, GPXSource )
    }


smoothAltitudesWithWindowAverage : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
smoothAltitudesWithWindowAverage options track =
    {-
       Use a running average of altitude.
    -}
    let
        startState : AverageSmoothState
        startState =
            { leading = []
            , trailing = []
            , outputs = [ getDualCoords track.trackTree 0 ]
            }

        slidingWindowSnoother :
            RoadSection
            -> AverageSmoothState
            -> AverageSmoothState
        slidingWindowSnoother road { leading, trailing, outputs } =
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
            in
            { leading = newLeading
            , trailing = List.take options.windowSize <| newTrailing
            , outputs = newOutputs
            }

        flusher : AverageSmoothState -> List ( EarthPoint, GPXSource )
        flusher { leading, trailing, outputs } =
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

                        revisedEarthPoint =
                            adjustAltitude averageAltitude justPassedEarthPoint

                        revisedGpx =
                            { justPassedGpx | altitude = averageAltitude }
                    in
                    flusher
                        { leading = List.drop 1 leading
                        , trailing = List.take 1 leading ++ trailing
                        , outputs = ( revisedEarthPoint, revisedGpx ) :: outputs
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
    }


averageGradientsWithWindow : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
averageGradientsWithWindow options track =
    {-
       Use a running average of gradients and derive altitudes.
    -}
    let
        startState : GradientSmootherState
        startState =
            { leading = []
            , trailing = []
            , outputs = [ getDualCoords track.trackTree 0 ]
            , lastAltitude = gpxPointFromIndex 0 track.trackTree |> .altitude
            }

        slidingWindowSnoother :
            RoadSection
            -> GradientSmootherState
            -> GradientSmootherState
        slidingWindowSnoother road { leading, trailing, outputs, lastAltitude } =
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
                    -- Start outputting when leading is full (trailing is not empty).
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

                                revisedEarthPoint =
                                    adjustAltitude newAltitude justPassedRoad.endPoint

                                justPassedGpx =
                                    Tuple.second justPassedRoad.sourceData

                                revisedGpx =
                                    { justPassedGpx | altitude = newAltitude }
                            in
                            ( ( revisedEarthPoint, revisedGpx ) :: outputs
                            , newAltitude
                            )
            in
            { leading = newLeading
            , trailing = List.take options.windowSize <| newTrailing
            , outputs = newOutputs
            , lastAltitude = nextAltitude
            }

        flusher : GradientSmootherState -> List ( EarthPoint, GPXSource )
        flusher { leading, trailing, outputs, lastAltitude } =
            -- We have stopped receiving any more at the leading edge, but we must
            -- process what we have acquired.
            case leading of
                [] ->
                    List.reverse outputs

                justPassedRoad :: others ->
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
    -}
    let
        startAltitude =
            gpxPointFromIndex 0 track.trackTree |> .altitude

        clamper :
            RoadSection
            -> ( Length.Length, List ( EarthPoint, GPXSource ) )
            -> ( Length.Length, List ( EarthPoint, GPXSource ) )
        clamper road ( lastAltitude, outputs ) =
            let
                newGradient =
                    clamp
                        (0 - options.maximumDescent)
                        options.maximumAscent
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
            ( newEndAltitude, ( newEarthPoint, newGpx ) :: outputs )

        ( _, adjustedPoints ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                (always Nothing)
                0
                track.trackTree
                clamper
                ( startAltitude, [ getDualCoords track.trackTree 0 ] )
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


view : Options -> (Msg -> msg) -> Element msg
view options wrapper =
    let
        maxAscentSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMaximumAscent
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Uphill: "
                                ++ showDecimal0 options.maximumAscent
                                ++ "%"
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
                            "Downhill: "
                                ++ showDecimal0 options.maximumDescent
                                ++ "%"
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
                            "Points either side: "
                                ++ String.fromInt options.windowSize
                , min = 1.0
                , max = 8.0
                , step = Just 1.0
                , value = toFloat options.windowSize
                , thumb = Input.defaultThumb
                }

        extent =
            Input.radioRow
                [ padding 10
                , spacing 5
                ]
                { onChange = wrapper << SetExtent
                , selected = Just options.extent
                , label = Input.labelHidden "Style"
                , options =
                    [ Input.option ExtentIsRange (text "Selected range")
                    , Input.option ExtentIsTrack (text "Whole track")
                    ]
                }

        limitGradientsMethod =
            column [ spacing 10 ]
                [ el [ centerX ] <| maxAscentSlider
                , el [ centerX ] <| maxDescentSlider

                --, el [ centerX ] <|
                --    Input.checkbox []
                --        { onChange = wrapper << SetRedistribution
                --        , icon = Input.defaultCheckbox
                --        , checked = options.limitRedistributes
                --        , label = Input.labelRight [] (text "Try to preserve altitudes")
                --        }
                , el [ centerX ] <|
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| LimitGradient
                        , label =
                            text <|
                                "Apply limits"
                        }
                ]

        smoothAltitudes =
            column [ spacing 10 ]
                [ el [ centerX ] <| windowSizeSlider
                , el [ centerX ] <|
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| LimitGradient
                        , label =
                            paragraph []
                                [ text "Smooth by averaging altitudes with nearby points" ]
                        }
                ]

        smoothGradiente =
            column [ spacing 10 ]
                [ el [ centerX ] <| windowSizeSlider
                , el [ centerX ] <|
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| LimitGradient
                        , label =
                            paragraph []
                                [ text "Smooth by averaging gradients with nearby points" ]
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
                    [ Input.option MethodLimit (text "Limit gradients")
                    , Input.option MethodAltitudes (text "Smooth altitudes")
                    , Input.option MethodGradients (text "Smooth gradients")
                    , Input.option MethodGaussian (text "Gaussian")
                    ]
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
                limitGradientsMethod

            MethodAltitudes ->
                smoothAltitudes

            MethodGradients ->
                smoothGradiente

            MethodGaussian ->
                none
        ]
