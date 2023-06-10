module Tools.MoveAndStretch exposing (Msg(..), apply, defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import Axis3d
import Color
import CommonToolStyles
import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (Meters, meters)
import Point2d
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import String.Interpolate
import Svg
import Svg.Attributes as SA
import SystemSettings exposing (SystemSettings)
import Tools.CurveFormer exposing (highlightPoints)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.MoveAndStretchOptions exposing (Mode(..), Options, Point)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


toolId =
    "stretch"


defaultOptions : Options
defaultOptions =
    { vector = Vector2d.zero
    , lastVector = Vector2d.zero
    , dragging = Nothing
    , preview = []
    , mode = Translate
    , heightSliderSetting = Quantity.zero -- Note this is not the length.
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
    | NudgeButton (Quantity Float Meters)


radius =
    100


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


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


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newPoints =
            case options.mode of
                Translate ->
                    let
                        currentPoints =
                            List.map Tuple.first <|
                                DomainModel.extractPointsInRange fromStart fromEnd track.trackTree
                    in
                    movePoints options currentPoints

                Stretch drag ->
                    stretchPoints options drag track
    in
    TrackLoaded.asPreviewPoints track newPoints


apply : Options -> TrackLoaded msg -> TrackLoaded msg
apply options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        gpxPoints =
            options.preview |> List.map .gpx

        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                gpxPoints
                track.trackTree
    in
    case newTree of
        Just isTree ->
            let
                pointerReposition =
                    --Let's reposition by distance, not uncommon.
                    --TODO: Arguably, position from the relevant track end would be better.
                    DomainModel.preserveDistanceFromStart track.trackTree isTree

                ( newOrange, newPurple ) =
                    ( pointerReposition track.currentPosition
                    , Maybe.map pointerReposition track.markerPosition
                    )
            in
            { track
                | trackTree = Maybe.withDefault track.trackTree newTree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> Color
    -> TrackLoaded msg
    -> ( Options, List (Actions.ToolAction msg) )
update message options wrapper previewColour track =
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

        DraggerModeToggle _ ->
            let
                newOptions =
                    { options
                        | mode =
                            case options.mode of
                                Translate ->
                                    let
                                        ( fromStart, _ ) =
                                            TrackLoaded.getRangeFromMarkers track
                                    in
                                    Stretch (fromStart + 1)

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
                        , heightSliderSetting = Quantity.zero
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
            , [ WithUndo (MoveAndStretchWithOptions options)
              , MoveAndStretchWithOptions options
              , TrackHasChanged
              , HidePreview "stretch"
              , HidePreview "stretchMark"
              ]
            )

        StretchHeight x ->
            let
                newOptions =
                    { options | heightSliderSetting = Length.meters x }

                preview =
                    computeNewPoints newOptions track

                optionsWithPreview =
                    { newOptions | preview = preview }
            in
            ( optionsWithPreview
            , previewActions optionsWithPreview previewColour track
            )

        NudgeButton value ->
            let
                newOptions =
                    if Quantity.equalWithin Length.millimeter Quantity.zero value then
                        { options | heightSliderSetting = Quantity.zero }

                    else
                        { options | heightSliderSetting = options.heightSliderSetting |> Quantity.plus value }

                preview =
                    computeNewPoints newOptions track

                optionsWithPreview =
                    { newOptions | preview = preview }
            in
            ( optionsWithPreview
            , previewActions optionsWithPreview previewColour track
            )


view : SystemSettings -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view settings options wrapper track =
    let
        i18n =
            I18N.text settings.location toolId

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

        verticalButton label increment =
            Input.button
                (width fill :: neatToolsBorder)
                { onPress = Just <| wrapper <| NudgeButton increment
                , label = i18n label
                }

        verticalNudgeButtons =
            column [ alignRight ] <|
                if settings.imperial then
                    [ verticalButton "+1yd" <| Length.yard
                    , verticalButton "+1ft" <| Length.foot
                    , verticalButton "+1in" <| Length.inch
                    , verticalButton "0" <| Quantity.zero
                    , verticalButton "-1in" <| Quantity.negate Length.inch
                    , verticalButton "-1ft" <| Quantity.negate Length.foot
                    , verticalButton "-1yd" <| Quantity.negate Length.yard
                    ]

                else
                    [ verticalButton "+1m" <| Length.meter
                    , verticalButton "+10cm" <| Length.centimeters 10
                    , verticalButton "+1cm" <| Length.centimeter
                    , verticalButton "0" <| Quantity.zero
                    , verticalButton "-1cm" <| Quantity.negate Length.centimeter
                    , verticalButton "-10cm" <| Quantity.negate <| Length.centimeters 10
                    , verticalButton "-1m" <| Quantity.negate Length.meter
                    ]

        --heightSlider =
        --    Input.slider commonShortVerticalSliderStyles
        --        { onChange = wrapper << StretchHeight
        --        , label = Input.labelHidden "Height"
        --        , min = -1.0
        --        , max = 1.0
        --        , step = Nothing
        --        , value = options.heightSliderSetting
        --        , thumb =
        --            Input.defaultThumb
        --        }
        showSliderInStretchMode =
            case options.mode of
                Stretch drag ->
                    let
                        sliderText =
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString settings.location toolId "white")
                                    [ String.fromInt drag ]

                        fineButtons =
                            [ Input.button neatToolsBorder
                                { label = text "<"
                                , onPress =
                                    Just <|
                                        wrapper <|
                                            DraggerMarker <|
                                                max (nearEnd + 1) <|
                                                    (drag - 1)
                                }
                            , Input.button neatToolsBorder
                                { label = text ">"
                                , onPress =
                                    Just <|
                                        wrapper <|
                                            DraggerMarker <|
                                                min (farEnd - 1) <|
                                                    (drag + 1)
                                }
                            ]
                    in
                    Input.slider commonShortHorizontalSliderStyles
                        { onChange = wrapper << DraggerMarker << round
                        , label =
                            Input.labelBelow [] <|
                                row [ spacing 5 ] (sliderText :: fineButtons)
                        , min = toFloat nearEnd + 1
                        , max = toFloat farEnd - 1
                        , step = Just 1.0
                        , value = toFloat drag
                        , thumb = Input.defaultThumb
                        }

                Translate ->
                    none

        showActionButtons =
            row [ spacing 5 ]
                [ Input.button neatToolsBorder
                    { label = i18n "Zero", onPress = Just <| wrapper DraggerReset }
                , if canApply then
                    Input.button neatToolsBorder
                        { label = i18n "apply"
                        , onPress = Just <| wrapper DraggerApply
                        }

                  else
                    Input.button neatToolsBorder
                        { label = i18n "invalid"
                        , onPress = Nothing
                        }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = Input.defaultCheckbox
                , checked = options.mode /= Translate
                , label = Input.labelRight [ centerY ] (i18n "Stretch")
                }
    in
    -- Try with linear vector, switch to log or something else if needed.
    row
        (CommonToolStyles.toolContentBoxStyle settings)
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
                    String.Interpolate.interpolate
                        (I18N.localisedString settings.location toolId "height")
                        [ showShortMeasure settings.imperial options.heightSliderSetting ]
            ]
        , verticalNudgeButtons

        --, heightSlider
        ]


movePoints : Options -> List EarthPoint -> List EarthPoint
movePoints options region =
    -- This used by preview and action.
    let
        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShift =
            options.heightSliderSetting

        translation pt =
            -- Negate y because SVG coordinates go downwards.
            { space =
                pt.space
                    |> Point3d.translateBy (Vector3d.xyz xShift (Quantity.negate yShift) zShift)
            , time = pt.time
            }
    in
    List.map translation region


stretchPoints : Options -> Int -> TrackLoaded msg -> List EarthPoint
stretchPoints options drag track =
    -- This used by preview and action.
    -- Here we move points either side of the stretch marker.
    -- Note we move by relative straight line distance not track distance.
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        farEnd =
            DomainModel.skipCount track.trackTree - fromEnd

        stretcher =
            .space <|
                DomainModel.earthPointFromIndex drag track.trackTree

        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShiftMax =
            Vector3d.xyz
                Quantity.zero
                Quantity.zero
                options.heightSliderSetting

        horizontalTranslation =
            -- Negate y because SVG coordinates go downwards.
            Vector3d.xyz xShift (Quantity.negate yShift) (meters 0)

        ( startAnchor, endAnchor ) =
            ( DomainModel.earthPointFromIndex fromStart track.trackTree
            , DomainModel.earthPointFromIndex farEnd track.trackTree
            )

        ( firstPart, secondPart ) =
            ( List.range fromStart (drag - 1)
            , List.range drag farEnd
            )

        ( firstPartAxis, secondPartAxis ) =
            ( Axis3d.throughPoints startAnchor.space stretcher
            , Axis3d.throughPoints endAnchor.space stretcher
            )

        ( firstPartDistance, secondPartDistance ) =
            ( Point3d.distanceFrom startAnchor.space stretcher
            , Point3d.distanceFrom endAnchor.space stretcher
            )

        ( trackDistanceToNearAnchor, trackDistanceToStretcher, trackDistanceToFarAnchor ) =
            ( DomainModel.distanceFromIndex fromStart track.trackTree
            , DomainModel.distanceFromIndex drag track.trackTree
            , DomainModel.distanceFromIndex farEnd track.trackTree
            )

        ( firstPartTrackDistance, secondPartTrackDistance ) =
            ( trackDistanceToStretcher |> Quantity.minus trackDistanceToNearAnchor
            , trackDistanceToFarAnchor |> Quantity.minus trackDistanceToStretcher
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

        adjustRelativeToStart ptIndex =
            let
                pt =
                    DomainModel.earthPointFromIndex ptIndex track.trackTree

                trackDistance =
                    DomainModel.distanceFromIndex ptIndex track.trackTree

                proportion =
                    Quantity.ratio
                        (pt.space |> distanceAlong firstPartAxis)
                        firstPartDistance

                proportionalDistance =
                    Quantity.ratio
                        (trackDistance |> Quantity.minus trackDistanceToNearAnchor)
                        firstPartTrackDistance
            in
            { space =
                pt.space
                    |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                    |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportionalDistance)
            , time = pt.time
            }

        adjustRelativeToEnd ptIndex =
            let
                pt =
                    DomainModel.earthPointFromIndex ptIndex track.trackTree

                trackDistance =
                    DomainModel.distanceFromIndex ptIndex track.trackTree

                proportion =
                    Quantity.ratio
                        (pt.space |> distanceAlong secondPartAxis)
                        secondPartDistance

                proportionalDistance =
                    Quantity.ratio
                        (trackDistanceToFarAnchor |> Quantity.minus trackDistance)
                        secondPartTrackDistance
            in
            { space =
                pt.space
                    |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                    |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportionalDistance)
            , time = pt.time
            }
    in
    adjustedFirstPoints ++ adjustedSecondPoints
