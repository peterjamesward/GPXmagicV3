module Tools.DisplaySettings exposing (Msg(..), defaultOptions, restoreSettings, toolId, update, view)

import Actions exposing (ToolAction(..))
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import Json.Decode as D
import LandUseDataTypes
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
    , landUse = LandUseDataTypes.LandUseHidden
    , placeNames = False
    , showConstraintsAtLevel = Nothing
    }


type Msg
    = SetRoadSurface Bool
    | SetCurtainStyle CurtainStyle
    | SetCentreLine Bool
    | SetGroundPlane Bool
    | SetLandUse LandUseDataTypes.LandUseDisplay
    | SetTerrainFineness Float
    | DisplayInfo String String
    | SetPlaceNames Bool


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

        SetPlaceNames state ->
            let
                newOptions =
                    { options | placeNames = state }
            in
            ( newOptions, actions newOptions )

        SetLandUse state ->
            let
                newOptions =
                    { options | landUse = state }
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

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


view : I18NOptions.Location -> (Msg -> msg) -> Options -> Element msg
view location wrap options =
    let
        i18n =
            I18N.text location toolId

        curtainChoice =
            Input.radio
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetCurtainStyle
                , selected = Just options.curtainStyle
                , label = Input.labelBelow [] (i18n "Curtain")
                , options =
                    [ Input.option NoCurtain (i18n "None")
                    , Input.option PlainCurtain (i18n "Plain")
                    , Input.option PastelCurtain (i18n "Coloured")
                    ]
                }

        landUseChoice =
            Input.radio
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetLandUse
                , selected = Just options.landUse
                , label =
                    Input.labelBelow [] <|
                        row [ spacing 4 ]
                            [ i18n "Land Use"
                            , infoButton (wrap <| DisplayInfo "display" "landuse")
                            ]
                , options =
                    [ Input.option LandUseDataTypes.LandUseHidden (i18n "None")
                    , Input.option LandUseDataTypes.LandUsePlanar (i18n "Flat")
                    , Input.option LandUseDataTypes.LandUseSloped (i18n "3D")
                    ]
                }
    in
    wrappedRow
        [ spacing 5
        , padding 5
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ curtainChoice
        , column []
            [ Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetRoadSurface
                , checked = options.roadSurface
                , label = Input.labelRight [] <| i18n "road"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetGroundPlane
                , checked = options.groundPlane
                , label = Input.labelRight [] <| text "Ground"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetCentreLine
                , checked = options.centreLine
                , label = Input.labelRight [] <| text "Centre line"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetPlaceNames
                , checked = options.placeNames
                , label = Input.labelRight [] <| text "Place names"
                , icon = Input.defaultCheckbox
                }
            ]
        , landUseChoice
        , Input.slider commonShortHorizontalSliderStyles
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
        ]
