module ViewMap exposing
    ( Msg(..)
    , defaultStyleUrl
    , initialiseContext
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import CommonToolStyles
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import Html.Attributes exposing (id, style)
import Pixels exposing (Pixels, inPixels)
import Quantity exposing (Quantity)
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import ViewMapContext exposing (MapContext, MapStyle(..))
import ViewPureStyles exposing (useIcon)


type Msg
    = ToggleFollowOrange
    | ToggleDraggable
    | ToggleMapStyleMenu
    | ChooseMapStyle MapStyle


mapUrl : MapStyle -> String
mapUrl style =
    case style of
        MapBasic ->
            "mapbox://styles/peterjamesward/ckj0benrl8i1k19rp4m1t3pkz"

        MapStreets ->
            "mapbox://styles/mapbox/streets-v12"

        MapSatellite ->
            "mapbox://styles/mapbox/satellite-v9"

        MapSatelliteStreets ->
            "mapbox://styles/mapbox/satellite-streets-v12"

        MapOutdoors ->
            "mapbox://styles/mapbox/outdoors-v12"

        MapLight ->
            "mapbox://styles/mapbox/light-v11"


defaultStyle =
    MapOutdoors


defaultStyleUrl =
    mapUrl defaultStyle


initialiseContext : Maybe MapContext -> MapContext
initialiseContext currentContext =
    case currentContext of
        Just context ->
            { context
                | mapClickDebounce = False
                , lastMapClick = ( 0, 0 )
            }

        Nothing ->
            { mapClickDebounce = False
            , lastMapClick = ( 0, 0 )
            , followOrange = False
            , draggable = False
            , mapStyleMenuOpen = False
            , mapStyle = defaultStyle
            }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> MapContext
    -> ( MapContext, List (ToolAction msg) )
update msg msgWrapper track area context =
    case msg of
        ToggleFollowOrange ->
            ( { context | followOrange = not context.followOrange }
            , []
            )

        ToggleMapStyleMenu ->
            ( { context | mapStyleMenuOpen = not context.mapStyleMenuOpen }
            , []
            )

        ChooseMapStyle style ->
            ( { context | mapStyle = style }
            , [ SetMapStyle <| mapUrl style ]
            )

        ToggleDraggable ->
            let
                newOptions =
                    { context | draggable = not context.draggable }
            in
            ( newOptions
            , [ MakeMapPointsDraggable newOptions.draggable
              , TrackHasChanged
              ]
            )


view :
    SystemSettings
    -> Maybe (TrackLoaded msg)
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe MapContext
    -> (Msg -> msg)
    -> Element msg
view settings track ( viewWidth, viewHeight ) mContext msgWrapper =
    let
        handyMapControls context =
            column
                [ alignTop
                , alignRight
                , moveDown 100
                , moveLeft 10
                , Font.size 40
                , padding 6
                , spacing 8
                , Border.width 1
                , Border.rounded 4
                , Border.color FlatColors.AussiePalette.blurple
                , Background.color (CommonToolStyles.themeBackground SystemSettings.LightTheme)
                ]
                [ Input.button
                    [ tooltip onLeft <|
                        if context.followOrange then
                            localisedTooltip settings.location "panes" "locked"

                        else
                            localisedTooltip settings.location "panes" "unlocked"
                    ]
                    { onPress = Just <| msgWrapper ToggleFollowOrange
                    , label =
                        if context.followOrange then
                            useIcon FeatherIcons.lock

                        else
                            useIcon FeatherIcons.unlock
                    }
                , Input.button
                    [ tooltip onLeft <|
                        if context.draggable then
                            localisedTooltip settings.location "panes" "drag"

                        else
                            localisedTooltip settings.location "panes" "nodrag"
                    ]
                    { onPress = Just <| msgWrapper ToggleDraggable
                    , label =
                        if context.draggable then
                            useIcon FeatherIcons.move

                        else
                            useIcon FeatherIcons.x
                    }
                , Input.button
                    [ tooltip onLeft (localisedTooltip settings.location "panes" "mapstyle")
                    , inFront <| el [ alignRight ] <| mapStyleChoices context
                    ]
                    { onPress = Just <| msgWrapper ToggleMapStyleMenu
                    , label = useIcon FeatherIcons.layers
                    }
                ]

        mapStyleChoices : MapContext -> Element msg
        mapStyleChoices context =
            if context.mapStyleMenuOpen then
                Input.radio
                    [ centerX
                    , spacing 5
                    , padding 5
                    , Font.size 12
                    , alignRight
                    , moveLeft 40
                    , Background.color FlatColors.FlatUIPalette.silver
                    , Border.rounded 8
                    ]
                    { onChange = msgWrapper << ChooseMapStyle
                    , options =
                        [ Input.option MapStreets (text "Streets")
                        , Input.option MapOutdoors (text "Outdoors")
                        , Input.option MapBasic (text "Basic")
                        , Input.option MapSatellite (text "Satellite")
                        , Input.option MapSatelliteStreets (text "Satellite streets")
                        , Input.option MapLight (text "Light")
                        ]
                    , selected = Just context.mapStyle
                    , label = labelHidden "map styles"
                    }

            else
                none

        ( w, h ) =
            ( inPixels viewWidth, inPixels viewHeight )
    in
    case mContext of
        Just context ->
            column
                [ inFront <|
                    if track /= Nothing then
                        handyMapControls context

                    else
                        none
                ]
                [ el
                    [ width <| px <| w
                    , height <| px <| h
                    , alignLeft
                    , alignTop
                    , Border.width 2
                    , Border.color FlatColors.ChinesePalette.peace
                    , htmlAttribute (id "map")
                    , htmlAttribute (style "width" <| String.fromInt w)
                    , htmlAttribute (style "height" <| String.fromInt h)
                    ]
                    none
                ]

        Nothing ->
            -- Keep the DOM hierarchy consistent.
            column []
                [ el
                    [ htmlAttribute (id "map")
                    , width <| px <| inPixels viewWidth
                    , height <| px <| inPixels viewHeight
                    , htmlAttribute (style "width" <| String.fromInt w)
                    , htmlAttribute (style "height" <| String.fromInt h)
                    ]
                    none
                ]
