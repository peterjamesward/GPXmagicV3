module View3dCommonElements exposing (Context, DragAction(..), Msg(..), common3dSceneAttributes, placesOverlay, zoomButtons)

import Angle exposing (Angle)
import Axis2d
import Camera3d exposing (Camera3d)
import Circle2d
import CommonToolStyles
import Dict
import Direction2d exposing (Direction2d)
import DomainModel exposing (EarthPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Frame2d
import Geometry.Svg as Svg
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Point2d
import Point3d.Projection as Point3d
import Quantity exposing (Quantity)
import Rectangle2d
import Svg
import Svg.Attributes
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.DisplaySettingsOptions
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (stopProp, useIcon)


type Msg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOp
    | ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | ToggleFollowOrange
    | SetEmphasis Int
    | MouseMove Mouse.Event
    | MapMsg MapViewer.Msg
    | ToggleShowMap


type DragAction
    = DragNone
    | DragRotate
    | DragPan


type alias Context =
    { cameraAzimuth : Direction2d LocalCoords --Camera relative to plane normal at focus point
    , cameraElevation : Angle -- Above local horizon plane
    , cameraDistance : Quantity Float Length.Meters
    , fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , map : MapViewer.Model
    , showMap : Bool
    }


onContextMenu : a -> Element.Attribute a
onContextMenu msg =
    HE.custom "contextmenu"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


common3dSceneAttributes msgWrapper context =
    [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
    , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
    , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
    , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> msgWrapper)
    , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
    , onContextMenu (msgWrapper ImageNoOp)
    , width fill
    , height fill
    , pointer
    , Border.width 0
    , Border.color FlatColors.ChinesePalette.peace
    ]


zoomButtons : SystemSettings -> (Msg -> msg) -> Context -> Element msg
zoomButtons settings msgWrapper context =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 10
        , Font.size 40
        , padding 6
        , spacing 8
        , Border.width 1
        , Border.rounded 4
        , Border.color FlatColors.AussiePalette.blurple
        , Background.color (CommonToolStyles.themeBackground settings.colourTheme)
        , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp >> msgWrapper)
        ]
        [ Input.button []
            { onPress = Just <| msgWrapper ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        , Input.button
            (if context.followSelectedPoint then
                [ tooltip onLeft (localisedTooltip settings.location "panes" "locked") ]

             else
                [ tooltip onLeft (localisedTooltip settings.location "panes" "unlocked") ]
            )
            { onPress = Just <| msgWrapper ToggleFollowOrange
            , label =
                if context.followSelectedPoint then
                    useIcon FeatherIcons.lock

                else
                    useIcon FeatherIcons.unlock
            }
        , Input.button
            [ ToolTip.tooltip
                onLeft
                (ToolTip.myTooltip <|
                    if context.showMap then
                        "Hide map"

                    else
                        "Show map"
                )
            ]
            { onPress = Just <| msgWrapper ToggleShowMap
            , label =
                if context.showMap then
                    useIcon FeatherIcons.square

                else
                    useIcon FeatherIcons.map
            }
        ]


placesOverlay :
    Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> Camera3d Meters LocalCoords
    -> Element msg
placesOverlay display ( givenWidth, givenHeight ) track camera =
    let
        ( svgWidth, svgHeight ) =
            ( String.fromInt <| Pixels.inPixels givenWidth
            , String.fromInt <| Pixels.inPixels givenHeight
            )

        screenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy
                    (Quantity.toFloatQuantity givenWidth)
                    (Quantity.toFloatQuantity givenHeight)
                )

        nodes2d =
            track.landUseData.places
                |> Dict.filter
                    (\_ place ->
                        place.space
                            |> Point3d.depth camera
                            |> Quantity.greaterThanZero
                    )
                |> Dict.map
                    (\_ place ->
                        place.space |> Point3d.toScreenSpace camera screenRectangle
                    )
                |> Dict.filter
                    (\_ screenPoint ->
                        Rectangle2d.contains screenPoint screenRectangle
                    )

        textAttributes atPoint =
            [ Svg.Attributes.fill "white"
            , Svg.Attributes.fontFamily "sans-serif"
            , Svg.Attributes.fontSize "12px"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate atPoint) + 10))
            , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate atPoint)))
            ]

        -- Create an SVG label at each place
        placeNames =
            nodes2d
                |> Dict.map
                    (\name place ->
                        Svg.g []
                            [ Svg.circle2d
                                [ Svg.Attributes.stroke "white"
                                , Svg.Attributes.strokeWidth "1"
                                , Svg.Attributes.fill "none"
                                ]
                                (Circle2d.withRadius (Pixels.float 3) place)
                            , Svg.text_
                                (textAttributes place)
                                [ Svg.text name ]
                                -- Hack: flip the text upside down since our later
                                -- 'Svg.relativeTo topLeftFrame' call will flip it
                                -- back right side up
                                |> Svg.mirrorAcross (Axis2d.through place Direction2d.x)
                            ]
                    )
                |> Dict.values
                |> Svg.g []
    in
    if
        Dict.isEmpty track.landUseData.places
            || not display.placeNames
    then
        none

    else
        let
            topLeftFrame =
                Frame2d.atPoint
                    (Point2d.xy Quantity.zero (Quantity.toFloatQuantity givenHeight))
                    |> Frame2d.reverseY
        in
        html <|
            Svg.svg
                [ Svg.Attributes.width svgWidth
                , Svg.Attributes.height svgHeight
                ]
                [ Svg.relativeTo topLeftFrame placeNames ]
