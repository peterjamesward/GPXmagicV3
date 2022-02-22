module Tools.MoveAndStretch exposing (..)

import Actions exposing (PreviewShape(..), ToolAction(..))
import Axis3d
import Color
import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import Quantity
import Svg
import Svg.Attributes as SA
import Tools.CurveFormer exposing (highlightPoints)
import Tools.MoveAndStretchOptions exposing (Mode(..), Options, Point)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { vector = Vector2d.zero
    , lastVector = Vector2d.zero
    , dragging = Nothing
    , preview = []
    , mode = Translate
    , heightSliderSetting = 0.0 -- Note this is not the length.
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | DraggerModeToggle Bool
    | DraggerReset
    | DraggerMarker Int
    | DraggerApply
    | StretchHeight Float


radius =
    100


heightOffset sliderValue =
    -- Using cube here gives large range, preserves sign, more control in the centre.
    let
        clamped =
            -- Just to be sure.
            clamp -1.0 1.0 sliderValue
    in
    Length.meters <| 100.0 * clamped * clamped * clamped


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


settingNotZero : Options -> Bool
settingNotZero model =
    Vector2d.direction model.vector
        /= Nothing
        || model.heightSliderSetting
        /= 0.0


twoWayDragControl : Options -> (Msg -> msg) -> Element msg
twoWayDragControl model wrapper =
    let
        clickableContainer =
            el
                [ htmlAttribute <| Pointer.onDown (.pointer >> .offsetPos >> point >> DraggerGrab >> wrapper)
                , htmlAttribute <| Pointer.onMove (.pointer >> .offsetPos >> point >> DraggerMove >> wrapper)
                , htmlAttribute <| Pointer.onUp (.pointer >> .offsetPos >> point >> DraggerRelease >> wrapper)
                , htmlAttribute <| Html.Attributes.style "touch-action" "none"
                , Element.pointer
                , Element.alignLeft
                ]
                << html
                << Svg.svg
                    [ SA.viewBox "-150 -150 300 300"
                    , SA.width "140px"
                    , SA.height "140px"
                    ]

        ( x, y ) =
            Vector2d.components model.vector

        ( xPoint, yPoint ) =
            ( String.fromFloat <| Length.inMeters x
            , String.fromFloat <| Length.inMeters y
            )
    in
    clickableContainer <|
        [ Svg.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| String.fromInt radius
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SA.fill "darkslategrey"
            ]
            []
        , Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 xPoint
            , SA.y2 yPoint
            , SA.stroke "orange"
            , SA.strokeWidth "10"
            , SA.strokeLinecap "round"
            ]
            []
        ]


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, previewActions options colour theTrack )

        _ ->
            ( options, [ HidePreview "stretch", HidePreview "stretchMark" ] )


previewActions newOptions colour track =
    [ ShowPreview
        { tag = "stretch"
        , shape = PreviewLine
        , colour = colour
        , points = newOptions.preview
        }
    , case newOptions.mode of
        Stretch mark ->
            ShowPreview
                { tag = "stretchMark"
                , shape =
                    PreviewToolSupplied <|
                        highlightPoints Color.white [ mark ] track
                , colour = colour
                , points = []
                }

        Translate ->
            Actions.NoAction
    ]


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        currentPoints =
            List.map Tuple.first <|
                DomainModel.extractPointsInRange fromStart fromEnd track.trackTree

        newPoints =
            case options.mode of
                Translate ->
                    movePoints options currentPoints

                Stretch drag ->
                    stretchPoints options drag track
    in
    newPoints
        |> List.map
            (\earth ->
                ( earth
                , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                )
            )


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        gpxPoints =
            options.preview |> List.map Tuple.second

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                gpxPoints
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


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> Color
    -> TrackLoaded msg
    -> ( Options, List (Actions.ToolAction msg) )
update message options wrapper previewColour track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track
    in
    case message of
        DraggerGrab offset ->
            ( { options | dragging = Just offset }, [] )

        DraggerMove offset ->
            case options.dragging of
                Nothing ->
                    ( options, [] )

                Just dragStart ->
                    let
                        newVector =
                            options.lastVector |> Vector2d.plus (Vector2d.from dragStart offset)

                        newOptions =
                            { options | vector = newVector }

                        preview =
                            computeNewPoints newOptions track

                        optionsWithPreview =
                            { newOptions | preview = preview }
                    in
                    ( optionsWithPreview
                    , previewActions optionsWithPreview previewColour track
                    )

        DraggerRelease _ ->
            ( { options
                | dragging = Nothing
                , lastVector = options.vector
              }
            , []
            )

        DraggerModeToggle bool ->
            let
                newOptions =
                    { options
                        | mode =
                            case options.mode of
                                Translate ->
                                    Stretch fromStart

                                Stretch _ ->
                                    Translate
                    }

                preview =
                    computeNewPoints newOptions track

                optionsWithPreview =
                    { newOptions | preview = preview }
            in
            ( optionsWithPreview
            , previewActions optionsWithPreview previewColour track
            )

        DraggerReset ->
            let
                newOptions =
                    { options
                        | dragging = Nothing
                        , vector = Vector2d.zero
                        , heightSliderSetting = 0.0
                        , preview = []
                    }
            in
            ( newOptions, [ HidePreview "stretch", HidePreview "stretchMark" ] )

        DraggerMarker idx ->
            let
                newOptions =
                    { options | mode = Stretch idx }

                preview =
                    computeNewPoints newOptions track

                optionsWithPreview =
                    { newOptions | preview = preview }
            in
            ( optionsWithPreview
            , previewActions optionsWithPreview previewColour track
            )

        DraggerApply ->
            ( options
            , [ MoveAndStretchWithOptions options
              , TrackHasChanged
              , HidePreview "stretch"
              , HidePreview "stretchMark"
              ]
            )

        StretchHeight x ->
            let
                newOptions =
                    { options | heightSliderSetting = x }

                preview =
                    computeNewPoints newOptions track

                optionsWithPreview =
                    { newOptions | preview = preview }
            in
            ( optionsWithPreview
            , previewActions optionsWithPreview previewColour track
            )


view : Bool -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view imperial options wrapper track =
    let
        ( nearEnd, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        farEnd =
            DomainModel.skipCount track.trackTree - fromEnd

        canApply =
            case options.mode of
                Translate ->
                    nearEnd < farEnd

                Stretch drag ->
                    nearEnd < drag && drag < farEnd

        heightSlider =
            Input.slider commonShortVerticalSliderStyles
                { onChange = wrapper << StretchHeight
                , label = Input.labelHidden "Height"
                , min = -1.0
                , max = 1.0
                , step = Nothing
                , value = options.heightSliderSetting
                , thumb =
                    Input.defaultThumb
                }

        showSliderInStretchMode =
            case options.mode of
                Stretch drag ->
                    Input.slider commonShortHorizontalSliderStyles
                        { onChange = wrapper << DraggerMarker << round
                        , label =
                            Input.labelBelow []
                                (text "Choose point to drag")
                        , min = toFloat <| nearEnd + 1
                        , max = toFloat <| farEnd - 1
                        , step = Just 1.0
                        , value = drag |> toFloat
                        , thumb = Input.defaultThumb
                        }

                Translate ->
                    none

        showActionButtons =
            row [ spacing 5 ]
                [ Input.button neatToolsBorder
                    { label = text "Zero", onPress = Just <| wrapper DraggerReset }
                , if canApply then
                    Input.button neatToolsBorder
                        { label = text "Apply"
                        , onPress = Just <| wrapper DraggerApply
                        }

                  else
                    Input.button neatToolsBorder
                        { label = text "Not valid"
                        , onPress = Nothing
                        }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = Input.defaultCheckbox
                , checked = options.mode /= Translate
                , label = Input.labelRight [ centerY ] (text "Stretch")
                }
    in
    -- Try with linear vector, switch to log or something else if needed.
    row
        [ padding 5
        , spacing 5
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ twoWayDragControl options wrapper
        , column
            [ Element.alignLeft
            , Element.width Element.fill
            , spacing 10
            ]
            [ el [ centerX ] showModeSelection
            , el [ centerX ] showSliderInStretchMode
            , el [ centerX ] showActionButtons
            , el [ centerX ] <|
                text <|
                    "Height "
                        ++ showShortMeasure imperial (heightOffset options.heightSliderSetting)
            ]
        , heightSlider
        ]


movePoints : Options -> List EarthPoint -> List EarthPoint
movePoints options region =
    -- This used by preview and action.
    let
        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShift =
            heightOffset options.heightSliderSetting

        translation =
            -- Negate y because SVG coordinates go downards.
            Point3d.translateBy (Vector3d.xyz xShift (Quantity.negate yShift) zShift)
    in
    List.map translation region


stretchPoints : Options -> Int -> TrackLoaded msg -> List EarthPoint
stretchPoints options drag track =
    -- This used by preview and action.
    -- Here we move points either side of the stretch marker.
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        toEnd =
            DomainModel.skipCount track.trackTree - fromEnd

        stretcher =
            DomainModel.earthPointFromIndex drag track.trackTree

        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShiftMax =
            Vector3d.xyz
                Quantity.zero
                Quantity.zero
                (heightOffset options.heightSliderSetting)

        horizontalTranslation =
            -- Negate y because SVG coordinates go downards.
            Vector3d.xyz xShift (Quantity.negate yShift) (meters 0)

        ( startAnchor, endAnchor ) =
            ( DomainModel.earthPointFromIndex fromStart track.trackTree
            , DomainModel.earthPointFromIndex toEnd track.trackTree
            )

        ( firstPart, secondPart ) =
            ( List.map Tuple.first <|
                DomainModel.extractPointsInRange
                    (fromStart + 1)
                    (DomainModel.skipCount track.trackTree - drag)
                    track.trackTree
            , List.map Tuple.first <|
                DomainModel.extractPointsInRange
                    (drag + 1)
                    (fromEnd + 1)
                    track.trackTree
            )

        ( firstPartAxis, secondPartAxis ) =
            ( Axis3d.throughPoints startAnchor stretcher
            , Axis3d.throughPoints endAnchor stretcher
            )

        ( firstPartDistance, secondPartDistance ) =
            ( Point3d.distanceFrom startAnchor stretcher
            , Point3d.distanceFrom endAnchor stretcher
            )

        distanceAlong maybeAxis p =
            case maybeAxis of
                Just axis ->
                    p |> Point3d.signedDistanceAlong axis

                Nothing ->
                    Quantity.zero

        ( adjustedFirstPoints, adjustedSecondPoints ) =
            ( List.map adjustRelativeToStart firstPart
            , List.map adjustRelativeToEnd secondPart
            )

        adjustRelativeToStart pt =
            let
                proportion =
                    Quantity.ratio
                        (pt |> distanceAlong firstPartAxis)
                        firstPartDistance
            in
            pt
                |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportion)

        adjustRelativeToEnd pt =
            let
                proportion =
                    Quantity.ratio
                        (pt |> distanceAlong secondPartAxis)
                        secondPartDistance
            in
            pt
                |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportion)
    in
    adjustedFirstPoints ++ adjustedSecondPoints
