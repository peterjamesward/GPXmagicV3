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
import MapPortController
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
    , landUse = LandUseDataTypes.LandUseHidden
    , placeNames = False
    , showConstraintsAtLevel = Nothing
    , mapProjection = "globe"
    , mapAllowTilt = True
    , mapAllowRotate = True
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
    | AllowMapTilt Bool
    | UseGlobeProjection Bool
    | AllowMapRotate Bool


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

        AllowMapTilt allowed ->
            let
                newOptions =
                    { options | mapAllowTilt = allowed }
            in
            ( newOptions
            , Actions.SetMapAllowTilt allowed
                :: actions newOptions
            )

        AllowMapRotate allowed ->
            let
                newOptions =
                    { options | mapAllowRotate = allowed }
            in
            ( newOptions
            , Actions.SetMapAllowRotate allowed
                :: actions newOptions
            )

        UseGlobeProjection useGlobe ->
            let
                newOptions =
                    { options
                        | mapProjection =
                            case useGlobe of
                                False ->
                                    "mercator"

                                True ->
                                    "globe"
                    }
            in
            ( newOptions
            , Actions.SetMapProjection newOptions.mapProjection
                :: actions newOptions
            )


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

        landUseChoice =
            Input.radio groupStyle
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
        (CommonToolStyles.toolContentBoxStyle settings)
        [ curtainChoice
        , landUseChoice
        , column groupStyle
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
        , column groupStyle
            [ Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << UseGlobeProjection
                , checked = options.mapProjection == "globe"
                , label = Input.labelRight [] <| text "Use Globe map"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << AllowMapTilt
                , checked = options.mapAllowTilt
                , label = Input.labelRight [] <| text "Map can tilt"
                , icon = Input.defaultCheckbox
                }
            , Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << AllowMapRotate
                , checked = options.mapAllowRotate
                , label = Input.labelRight [] <| text "Map can rotate"
                , icon = Input.defaultCheckbox
                }
            ]
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
