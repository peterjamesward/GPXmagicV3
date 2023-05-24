module Tools.DisplaySettings exposing (Msg(..), defaultOptions, restoreSettings, toolId, update, view)

import Actions exposing (ToolAction(..))
import CommonToolStyles
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FlatColors.ChinesePalette
import Json.Decode as D
import LandUseDataTypes
import ProfilePort
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions exposing (..)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, infoButton)


toolId =
    "display"


defaultOptions : Options
defaultOptions =
    { roadSurface = True
    , curtainStyle = PastelCurtain
    , centreLine = False
    , groundPlane = True
    , terrainFineness = 0.0
    , placeNames = False
    , showConstraintsAtLevel = Nothing
    , previewSize = 4
    }


type Msg
    = SetRoadSurface Bool
    | SetCurtainStyle CurtainStyle
    | SetCentreLine Bool
    | SetGroundPlane Bool
    | SetTerrainFineness Float
    | DisplayInfo String String
    | SetPreviewSize Int


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
            [ StoreLocally "visuals" <| encode newOptions
            , ReRender
            ]
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

        SetTerrainFineness terrain ->
            let
                newOptions =
                    { options | terrainFineness = terrain }
            in
            ( newOptions, actions newOptions )

        SetPreviewSize size ->
            let
                newOptions =
                    { options | previewSize = size }
            in
            ( newOptions, actions newOptions )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


view : SystemSettings -> (Msg -> msg) -> Options -> Element msg
view settings wrap options =
    let
        groupStyle =
            [ padding 5
            , spacing 5
            , Border.width 1
            , Border.rounded 4
            ]

        i18n =
            I18N.text settings.location toolId

        curtainChoice =
            Input.radio groupStyle
                { onChange = wrap << SetCurtainStyle
                , selected = Just options.curtainStyle
                , label = Input.labelBelow [] (i18n "Curtain")
                , options =
                    [ Input.option NoCurtain (i18n "None")
                    , Input.option PlainCurtain (i18n "Plain")
                    , Input.option PastelCurtain (i18n "Coloured")
                    ]
                }
    in
    wrappedRow
        (CommonToolStyles.toolContentBoxStyle settings)
        [ curtainChoice
        , column groupStyle
            [ Input.checkbox
                []
                { onChange = wrap << SetRoadSurface
                , checked = options.roadSurface
                , label = Input.labelRight [] <| i18n "road"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                []
                { onChange = wrap << SetGroundPlane
                , checked = options.groundPlane
                , label = Input.labelRight [] <| text "Ground"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                []
                { onChange = wrap << SetCentreLine
                , checked = options.centreLine
                , label = Input.labelRight [] <| text "Centre line"
                , icon = Input.defaultCheckbox
                }
            ]
        , column [ padding 5, spacing 10 ]
            [ Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetTerrainFineness
                , label =
                    Input.labelBelow [] <|
                        if options.terrainFineness == 0.0 then
                            i18n "noterrain"

                        else
                            i18n "quality"
                , min = 0.0
                , max = 3.0
                , step = Nothing
                , value = options.terrainFineness
                , thumb = Input.defaultThumb
                }
            , Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetPreviewSize << round
                , label =
                    Input.labelBelow [] (text "Preview size")
                , min = 1.0
                , max = 7.0
                , step = Just 1
                , value = toFloat options.previewSize
                , thumb = Input.defaultThumb
                }
            ]
        ]
