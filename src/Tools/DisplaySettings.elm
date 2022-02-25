module Tools.DisplaySettings exposing (..)

import Actions exposing (ToolAction(..))
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Json.Decode as D
import Tools.DisplaySettingsOptions exposing (..)


defaultOptions : Options
defaultOptions =
    { roadSurface = True
    , curtainStyle = PastelCurtain
    , centreLine = False
    , groundPlane = True
    }


type Msg
    = SetRoadSurface Bool
    | SetCurtainStyle CurtainStyle
    | SetCentreLine Bool
    | SetGroundPlane Bool


restoreSettings : D.Value -> Options -> Options
restoreSettings json current =
    decode json current


update :
    Msg
    -> Options
    -> ( Options, List (ToolAction msg) )
update msg options =
    let
        actions newOptions =
            [ StoreLocally "visuals" <| encode newOptions ]
    in
    case msg of
        SetCentreLine state ->
            let
                newOptions =
                    { options | centreLine = state }
            in
            ( newOptions, actions newOptions )

        SetGroundPlane state ->
            let
                newOptions =
                    { options | groundPlane = state }
            in
            ( newOptions, actions newOptions )

        SetRoadSurface state ->
            let
                newOptions =
                    { options | roadSurface = state }
            in
            ( newOptions, actions newOptions )

        SetCurtainStyle curtainStyle ->
            let
                newOptions =
                    { options | curtainStyle = curtainStyle }
            in
            ( newOptions, actions newOptions )


view : (Msg -> msg) -> Options -> Element msg
view wrap options =
    let
        curtainChoice =
            Input.radio
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetCurtainStyle
                , selected = Just options.curtainStyle
                , label = Input.labelHidden "Curtain"
                , options =
                    [ Input.option NoCurtain (text "No curtain")
                    , Input.option PlainCurtain (text "Plain")
                    , Input.option PastelCurtain (text "Coloured")
                    ]
                }
    in
    column
        [ spacing 5
        , padding 5
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ el [ centerX ] curtainChoice
        , Input.checkbox
            [ padding 5
            , spacing 5
            ]
            { onChange = wrap << SetRoadSurface
            , checked = options.roadSurface
            , label = Input.labelLeft [] <| text "Road surface"
            , icon = Input.defaultCheckbox
            }
        , Input.checkbox
            [ padding 5
            , spacing 5
            ]
            { onChange = wrap << SetGroundPlane
            , checked = options.groundPlane
            , label = Input.labelLeft [] <| text "Ground"
            , icon = Input.defaultCheckbox
            }
        , Input.checkbox
            [ padding 5
            , spacing 5
            ]
            { onChange = wrap << SetCentreLine
            , checked = options.centreLine
            , label = Input.labelLeft [] <| text "Centre line"
            , icon = Input.defaultCheckbox
            }
        ]
