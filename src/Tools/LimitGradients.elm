module Tools.LimitGradients exposing (..)

import Actions exposing (PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters, meters)
import Point3d
import Quantity
import Tools.LimitGradientOptions exposing (ExtentOption(..), Options)
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, prettyButtonStyles)


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
    }


actions newOptions previewColour track =
    if newOptions.extent == ExtentIsRange then
        [ ShowPreview
            { tag = "limit"
            , shape = PreviewCircle
            , colour = previewColour
            , points = computeNewPoints newOptions track
            }
        ]

    else
        [ HidePreview "limit" ]


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
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ( SetMaximumAscent up, Just track ) ->
            let
                newOptions =
                    { options | maximumAscent = up }
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ( SetMaximumDescent down, Just track ) ->
            let
                newOptions =
                    { options | maximumDescent = down }
            in
            ( newOptions
            , actions newOptions previewColour track
            )

        ( LimitGradient, Just track ) ->
            ( options
            , []
            )


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

        unclampedXYDeltas : List ( Length.Length, Length.Length )
        unclampedXYDeltas =
            -- Yields X and Y deltas looking forward from each track point.
            List.map2
                (\pt1 pt2 ->
                    ( pt1.length
                    , Point3d.zCoordinate pt2.xyz |> Quantity.minus (Point3d.zCoordinate pt1.xyz)
                    )
                )
                targetZone
                (List.drop 1 targetZone)

        clampedXYDeltas : List ( Length.Length, Length.Length )
        clampedXYDeltas =
            -- What the deltas would be with the ascent and descent limits applied.
            List.map
                (\( x, y ) ->
                    ( x
                    , Quantity.clamp
                        (x |> Quantity.multiplyBy (negate settings.maximumDescent / 100.0))
                        (x |> Quantity.multiplyBy (settings.maximumAscent / 100.0))
                        y
                    )
                )
                unclampedXYDeltas

        targetElevationChange =
            -- Current change of elevation, derived directly by summation.
            Quantity.sum <| List.map Tuple.second unclampedXYDeltas

        clampedElevationChange =
            -- What the change would be with the limits in place.
            Quantity.sum <| List.map Tuple.second clampedXYDeltas

        elevationCorrection =
            -- What overall impact do the limits have?
            targetElevationChange |> Quantity.minus clampedElevationChange

        offeredCorrections =
            -- "Ask" each segment how much leeway they have from the limit (up or down)
            if elevationCorrection |> Quantity.greaterThan Quantity.zero then
                -- We need to gain height overall.
                List.map
                    (\( x, y ) ->
                        (x |> Quantity.multiplyBy (settings.maximumAscent / 100.0))
                            |> Quantity.minus y
                    )
                    clampedXYDeltas

            else if elevationCorrection |> Quantity.lessThan Quantity.zero then
                -- We need to lose height overall.
                List.map
                    (\( x, y ) ->
                        (x |> Quantity.multiplyBy (settings.maximumDescent / 100.0))
                            |> Quantity.minus y
                    )
                    clampedXYDeltas

            else
                List.map (always Quantity.zero) clampedXYDeltas

        totalOffered =
            -- How much do we have to play with?
            Quantity.sum offeredCorrections

        proprtionNeeded =
            -- Assuming less than one for now, or button should have been disabled.
            if Quantity.abs elevationCorrection |> Quantity.lessThan (meters 0.1) then
                -- 10 cm is near enough.
                0

            else
                -- How much of what is available is needed?
                Quantity.ratio elevationCorrection totalOffered
                    |> clamp 0.0 1.0

        proRataCorrections =
            -- What shall we ask from each segment, on this basis?
            List.map
                (Quantity.multiplyBy proprtionNeeded)
                offeredCorrections

        finalYDeltas =
            -- What does that make the deltas?
            List.map2
                (\( x, y ) adjust -> y |> Quantity.plus adjust)
                clampedXYDeltas
                proRataCorrections

        resultingElevations =
            -- And from that, the running cumulative elevations?
            List.Extra.scanl
                Quantity.plus
                (Point3d.zCoordinate referenceNode.xyz)
                finalYDeltas

        undoRedoInfo : UndoRedoInfo
        undoRedoInfo =
            { regionStart = startIndex
            , regionEnd = endIndex
            , originalAltitudes = targetZone |> List.map (.xyz >> Point3d.zCoordinate)
            , revisedAltitudes = resultingElevations
            }
    in
    []



view : Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view options wrapper track =
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

        markedNode =
            Maybe.withDefault track.currentNode track.markedNode

        startPoint =
            if track.currentNode.index <= markedNode.index then
                track.currentNode

            else
                markedNode

        endPoint =
            if track.currentNode.index < markedNode.index then
                markedNode

            else
                track.currentNode
    in
    wrappedRow [ spacing 10, padding 10 ]
        [ maxAscentSlider
        , maxDescentSlider
        , button
            prettyButtonStyles
            { onPress = Just <| wrapper <| LimitGradient
            , label =
                text <|
                    "Apply limits"
            }
        ]
