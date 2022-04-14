module Tools.DisplaySettings exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Json.Decode as D
import LandUseDataTypes
import Tools.DisplaySettingsOptions exposing (..)
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
    | SetConstraintLevel Int


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

        SetConstraintLevel level ->
            let
                newOptions =
                    { options
                        | showConstraintsAtLevel =
                            if level > 0 then
                                Just level

                            else
                                Nothing
                    }
            in
            ( newOptions, actions newOptions )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


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
                , label = Input.labelBelow [] (text "Curtain")
                , options =
                    [ Input.option NoCurtain (text "None")
                    , Input.option PlainCurtain (text "Plain")
                    , Input.option PastelCurtain (text "Coloured")
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
                            [ text "Land Use"
                            , infoButton (wrap <| DisplayInfo "display" "landuse")
                            ]
                , options =
                    [ Input.option LandUseDataTypes.LandUseHidden (text "None")
                    , Input.option LandUseDataTypes.LandUsePlanar (text "Flat")
                    , Input.option LandUseDataTypes.LandUseSloped (text "3D")
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
                , label = Input.labelRight [] <| text "Road surface"
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
                        text "Terrain off"

                    else
                        text "Terrain quality"
            , min = 0.0
            , max = 3.0
            , step = Nothing
            , value = options.terrainFineness
            , thumb = Input.defaultThumb
            }
        ]
