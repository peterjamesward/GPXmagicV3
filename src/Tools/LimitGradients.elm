module Tools.LimitGradients exposing (..)

import Actions exposing (PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, distanceFromIndex, earthPointFromIndex, skipCount, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import Point3d exposing (zCoordinate)
import Quantity exposing (multiplyBy, zero)
import Tools.LimitGradientOptions exposing (ExtentOption(..), Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


type Msg
    = LimitGradient
    | SetMaximumAscent Float
    | SetMaximumDescent Float
    | SetExtent ExtentOption


defaultOptions : Options
defaultOptions =
    { maximumAscent = 15.0
    , maximumDescent = 15.0
    , extent = ExtentIsRange
    , previewData = Nothing
    }


actions : Options -> Element.Color -> TrackLoaded msg -> List (ToolAction msg)
actions newOptions previewColour track =
    if newOptions.extent == ExtentIsRange then
        case newOptions.previewData of
            Just previewTree ->
                [ ShowPreview
                    { tag = "limit"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = DomainModel.extractPointsInRange 0 0 previewTree
                    }
                , RenderProfile
                ]

            Nothing ->
                [ HidePreview "limit" ]

    else
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
            <|
                List.map Tuple.second adjustedPoints
    }


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( msg, hasTrack ) of
        ( SetExtent extent, Just track ) ->
            let
                newOptions =
                    { options | extent = extent }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ( SetMaximumAscent up, Just track ) ->
            let
                newOptions =
                    { options | maximumAscent = up }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ( SetMaximumDescent down, Just track ) ->
            let
                newOptions =
                    { options | maximumDescent = down }
                        |> putPreviewInOptions track
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ( LimitGradient, Just track ) ->
            ( options
            , [ Actions.LimitGradientWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
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

        adjustAltitude : Length.Length -> EarthPoint -> EarthPoint
        adjustAltitude alt pt =
            Point3d.xyz
                (Point3d.xCoordinate pt)
                (Point3d.yCoordinate pt)
                alt

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
            ( options, [ HidePreview "limit" ] )


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

        extent =
            Input.radioRow
                [ padding 10
                , spacing 5
                ]
                { onChange = wrapper << SetExtent
                , selected = Just options.extent
                , label = Input.labelHidden "Style"
                , options =
                    [ Input.option ExtentIsRange (text "Selected range\n(preview)")
                    , Input.option ExtentIsTrack (text "Whole track\n(no preview)")
                    ]
                }
    in
    column
        [ spacing 6
        , padding 6
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , width fill
        ]
        [ el [ centerX ] <| maxAscentSlider
        , el [ centerX ] <| maxDescentSlider
        , el [ centerX ] <| extent
        , el [ centerX ] <|
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| LimitGradient
                , label =
                    text <|
                        "Apply limits"
                }
        ]
