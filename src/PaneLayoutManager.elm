module PaneLayoutManager exposing (..)

import Actions exposing (..)
import Dict exposing (Dict)
import DomainModel exposing (skipCount)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (..)
import FeatherIcons
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Json.Encode as E
import List.Extra
import LocalCoords exposing (LocalCoords)
import MapPortController
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import SceneBuilder3D
import Tools.DisplaySettingsOptions
import ToolsController
import TrackLoaded exposing (TrackLoaded)
import ViewMap
import ViewPlan
import ViewProfileCharts
import ViewPureStyles exposing (..)
import ViewThirdPerson exposing (stopProp)


type ViewMode
    = ViewInfo
    | ViewThird
    | ViewFirst
    | ViewPlan
    | ViewProfile
    | ViewMap


type ViewContext
    = ThirdPersonContext ViewThirdPerson.Context
    | MapContext ViewMap.Context
    | InfoContext
    | ProfileContext


type PaneType
    = PaneWithMap
    | PaneNoMap


type PaneLayout
    = PanesOne
    | PanesLeftRight
    | PanesUpperLower
    | PanesOnePlusTwo
    | PanesGrid


type PaneId
    = Pane1
    | Pane2
    | Pane3
    | Pane4


type alias PaneContext =
    { paneId : PaneId
    , activeView : ViewMode
    , thirdPersonContext : Maybe ViewThirdPerson.Context
    , mapContext : Maybe ViewMap.Context
    , profileContext : Maybe ViewProfileCharts.Context
    , planContext : Maybe ViewPlan.Context
    }


type alias Options =
    { paneLayout : PaneLayout
    , popupVisible : Bool
    , pane1 : PaneContext
    , pane2 : PaneContext
    , pane3 : PaneContext
    , pane4 : PaneContext
    , sliderState : SliderState
    , scene3d : List (Entity LocalCoords)
    , mapState : MapPortController.MapState
    }


type SliderState
    = SliderIdle
    | SliderMoved
    | SliderWaitingForTimeout


defaultPaneContext : PaneContext
defaultPaneContext =
    { paneId = Pane1
    , activeView = ViewInfo
    , thirdPersonContext = Nothing
    , mapContext = Nothing
    , profileContext = Nothing
    , planContext = Nothing
    }


defaultOptions : Options
defaultOptions =
    { paneLayout = PanesOne
    , popupVisible = False
    , pane1 = defaultPaneContext
    , pane2 = { defaultPaneContext | paneId = Pane2 }
    , pane3 = { defaultPaneContext | paneId = Pane3 }
    , pane4 = { defaultPaneContext | paneId = Pane4 }
    , sliderState = SliderIdle
    , scene3d = []
    , mapState = MapPortController.defaultMapState
    }


type Msg
    = SetPaneLayout PaneLayout
    | SetCurrentPosition Int
    | TogglePopup
    | SetViewMode PaneId ViewMode
    | ThirdPersonViewMessage PaneId ViewThirdPerson.Msg
    | ProfileViewMessage PaneId ViewProfileCharts.Msg
    | PlanViewMessage PaneId ViewPlan.Msg
    | MapPortsMessage MapPortController.MapMsg
    | MapViewMessage ViewMap.Msg
    | SliderTimeout
    | PaneNoOp


paneLayoutMenu : (Msg -> msg) -> Options -> Element msg
paneLayoutMenu msgWrapper options =
    Input.button
        [ padding 5
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , Border.color FlatColors.FlatUIPalette.peterRiver
        , Border.width 2
        , inFront <| showOptionsMenu msgWrapper options
        ]
        { onPress = Just <| msgWrapper TogglePopup
        , label = E.text "Choose layout"
        }


showOptionsMenu : (Msg -> msg) -> Options -> Element msg
showOptionsMenu msgWrapper options =
    if options.popupVisible then
        el
            [ moveDown 30
            , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute (style "z-index" "20")
            ]
        <|
            Input.radio
                (neatToolsBorder
                    ++ [ padding 10, spacing 10 ]
                )
                { options = optionList
                , onChange = msgWrapper << SetPaneLayout
                , selected = Just options.paneLayout
                , label = Input.labelHidden "Choose layout"
                }

    else
        none


optionList =
    [ Input.option PanesOne <| row [ spacing 20 ] [ useIcon FeatherIcons.square, E.text "Just the one" ]
    , Input.option PanesLeftRight <| row [ spacing 20 ] [ useIcon FeatherIcons.columns, E.text "Cupboards" ]
    , Input.option PanesUpperLower <| row [ spacing 20 ] [ useIcon FeatherIcons.server, E.text "Drawers" ]
    , Input.option PanesGrid <| row [ spacing 20 ] [ useIcon FeatherIcons.grid, E.text "Grid of four" ]
    ]


render :
    ToolsController.Options
    -> Options
    -> TrackLoaded msg
    -> Dict String PreviewData
    -> Options
render toolSettings options track previews =
    --Profile stuff now lives in the pane context, as each pane could
    --have different version!
    let
        imperial =
            toolSettings.imperial

        gradientChanges =
            List.map Tuple.first toolSettings.gradientProblemOptions.breaches
    in
    { options
        | scene3d =
            SceneBuilder3D.renderPreviews previews
                ++ SceneBuilder3D.render3dView toolSettings.displaySettings track
        , pane1 = renderPaneIfProfileVisible imperial gradientChanges options.pane1 track
        , pane2 = renderPaneIfProfileVisible imperial gradientChanges options.pane2 track
        , pane3 = renderPaneIfProfileVisible imperial gradientChanges options.pane3 track
        , pane4 = renderPaneIfProfileVisible imperial gradientChanges options.pane4 track
    }


renderProfile : ToolsController.Options -> Options -> TrackLoaded msg -> Options
renderProfile toolSettings options track =
    -- Same but only renders profile, because of zoom, pan, or something.
    let
        imperial =
            toolSettings.imperial

        gradientChanges =
            List.map Tuple.first toolSettings.gradientProblemOptions.breaches
    in
    { options
        | pane1 = renderPaneIfProfileVisible imperial gradientChanges options.pane1 track
        , pane2 = renderPaneIfProfileVisible imperial gradientChanges options.pane2 track
        , pane3 = renderPaneIfProfileVisible imperial gradientChanges options.pane3 track
        , pane4 = renderPaneIfProfileVisible imperial gradientChanges options.pane4 track
    }


renderPaneIfProfileVisible : Bool -> List Int -> PaneContext -> TrackLoaded msg -> PaneContext
renderPaneIfProfileVisible imperial gradientChanges pane track =
    case ( pane.activeView, pane.profileContext ) of
        ( ViewProfile, Just context ) ->
            { pane
                | profileContext =
                    Just <|
                        ViewProfileCharts.renderProfileDataForCharts
                            imperial
                            gradientChanges
                            context
                            track
            }

        _ ->
            pane


update :
    Msg
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Options
    -> ( Options, List (ToolAction msg) )
update paneMsg msgWrapper mTrack contentArea options =
    let
        updatePaneWith : PaneId -> (PaneContext -> PaneContext) -> Options
        updatePaneWith id updateFn =
            let
                currentPane =
                    case id of
                        Pane1 ->
                            options.pane1

                        Pane2 ->
                            options.pane2

                        Pane3 ->
                            options.pane3

                        Pane4 ->
                            options.pane4

                updatedPane =
                    updateFn currentPane
            in
            case id of
                Pane1 ->
                    { options | pane1 = updatedPane }

                Pane2 ->
                    { options | pane2 = updatedPane }

                Pane3 ->
                    { options | pane3 = updatedPane }

                Pane4 ->
                    { options | pane4 = updatedPane }
    in
    case paneMsg of
        PaneNoOp ->
            ( options, [] )

        SetPaneLayout paneLayout ->
            let
                newOptions =
                    { options | paneLayout = paneLayout }
            in
            ( newOptions
            , [ MapRefresh
              , StoreLocally "panes" <| encodePaneState newOptions
              ]
            )

        TogglePopup ->
            ( { options | popupVisible = not options.popupVisible }, [] )

        SetViewMode paneId viewMode ->
            let
                newOptions =
                    updatePaneWith paneId
                        (\pane ->
                            { pane
                                | activeView = viewMode
                            }
                        )
            in
            ( newOptions
            , [ MapRefresh
              , StoreLocally "panes" <| encodePaneState newOptions
              ]
            )

        ThirdPersonViewMessage paneId imageMsg ->
            let
                paneInfo =
                    -- Tedious is good, tedious works.
                    --TODO: Local refactor here.
                    case paneId of
                        Pane1 ->
                            options.pane1

                        Pane2 ->
                            options.pane2

                        Pane3 ->
                            options.pane3

                        Pane4 ->
                            options.pane4

                ( newContext, actions ) =
                    case ( mTrack, paneInfo.thirdPersonContext ) of
                        ( Just track, Just third ) ->
                            let
                                ( new, act ) =
                                    ViewThirdPerson.update
                                        imageMsg
                                        (msgWrapper << ThirdPersonViewMessage Pane1)
                                        track
                                        (dimensionsWithLayout options.paneLayout contentArea)
                                        third
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane =
                    { paneInfo | thirdPersonContext = newContext }

                newOptions =
                    case paneId of
                        Pane1 ->
                            { options | pane1 = newPane }

                        Pane2 ->
                            { options | pane2 = newPane }

                        Pane3 ->
                            { options | pane3 = newPane }

                        Pane4 ->
                            { options | pane4 = newPane }
            in
            ( newOptions, actions )

        PlanViewMessage paneId imageMsg ->
            let
                paneInfo =
                    -- Tedious is good, tedious works.
                    --TODO: Local refactor here.
                    case paneId of
                        Pane1 ->
                            options.pane1

                        Pane2 ->
                            options.pane2

                        Pane3 ->
                            options.pane3

                        Pane4 ->
                            options.pane4

                ( newContext, actions ) =
                    case ( mTrack, paneInfo.planContext ) of
                        ( Just track, Just planContext ) ->
                            let
                                ( new, act ) =
                                    ViewPlan.update
                                        imageMsg
                                        (msgWrapper << PlanViewMessage Pane1)
                                        track
                                        (dimensionsWithLayout options.paneLayout contentArea)
                                        planContext
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane =
                    { paneInfo | planContext = newContext }

                newOptions =
                    case paneId of
                        Pane1 ->
                            { options | pane1 = newPane }

                        Pane2 ->
                            { options | pane2 = newPane }

                        Pane3 ->
                            { options | pane3 = newPane }

                        Pane4 ->
                            { options | pane4 = newPane }
            in
            ( newOptions, actions )

        ProfileViewMessage paneId imageMsg ->
            let
                paneInfo =
                    -- Tedious is good, tedious works.
                    --TODO: Local refactor here.
                    case paneId of
                        Pane1 ->
                            options.pane1

                        Pane2 ->
                            options.pane2

                        Pane3 ->
                            options.pane3

                        Pane4 ->
                            options.pane4

                ( newContext, actions ) =
                    case ( mTrack, paneInfo.profileContext ) of
                        ( Just track, Just profile ) ->
                            let
                                ( new, act ) =
                                    ViewProfileCharts.update
                                        imageMsg
                                        (msgWrapper << ProfileViewMessage Pane1)
                                        track
                                        (dimensionsWithLayout options.paneLayout contentArea)
                                        profile
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane =
                    { paneInfo | profileContext = newContext }

                newOptions =
                    case paneId of
                        Pane1 ->
                            { options | pane1 = newPane }

                        Pane2 ->
                            { options | pane2 = newPane }

                        Pane3 ->
                            { options | pane3 = newPane }

                        Pane4 ->
                            { options | pane4 = newPane }
            in
            ( newOptions, actions )

        MapViewMessage mapViewMsg ->
            let
                paneInfo =
                    options.pane1

                ( newContext, actions ) =
                    case ( mTrack, paneInfo.mapContext ) of
                        ( Just track, Just mapContext ) ->
                            let
                                ( new, act ) =
                                    ViewMap.update
                                        mapViewMsg
                                        (msgWrapper << MapViewMessage)
                                        track
                                        (dimensionsWithLayout options.paneLayout contentArea)
                                        mapContext
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane =
                    { paneInfo | mapContext = newContext }
            in
            ( { options | pane1 = newPane }, actions )

        MapPortsMessage mapMsg ->
            case mTrack of
                Just track ->
                    let
                        ( newState, actions ) =
                            MapPortController.update mapMsg track options.mapState
                    in
                    ( { options | mapState = newState }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        SetCurrentPosition pos ->
            -- Slider moves pointer and re-centres view.
            -- The actions will re-render and repaint the map.
            let
                mapFollowsOrange =
                    case options.pane1.mapContext of
                        Just mapContext ->
                            mapContext.followOrange

                        Nothing ->
                            False

                newOptions =
                    { options | sliderState = SliderMoved }
            in
            ( newOptions
            , [ SetCurrent pos
              , RenderProfile
              , if mapFollowsOrange then
                    MapCenterOnCurrent

                else
                    Actions.NoAction
              , Actions.DelayMessage 100 (msgWrapper SliderTimeout)
              ]
            )

        SliderTimeout ->
            let
                newOptions =
                    { options
                        | sliderState =
                            case options.sliderState of
                                SliderIdle ->
                                    SliderIdle

                                SliderMoved ->
                                    SliderWaitingForTimeout

                                SliderWaitingForTimeout ->
                                    SliderIdle
                    }
            in
            ( newOptions
            , [ if options.sliderState /= SliderIdle && newOptions.sliderState == SliderIdle then
                    -- Force re-render once only.
                    TrackHasChanged

                else
                    Actions.NoAction
              , if newOptions.sliderState /= SliderIdle then
                    -- Ask for a timer, to see if control has stopped moving.
                    Actions.DelayMessage 100 (msgWrapper SliderTimeout)

                else
                    Actions.NoAction
              ]
            )


initialise : TrackLoaded msg -> Options -> Options
initialise track options =
    { options
        | pane1 = initialisePane track options options.pane1
        , pane2 = initialisePane track options options.pane2
        , pane3 = initialisePane track options options.pane3
        , pane4 = initialisePane track options options.pane4
    }


initialisePane : TrackLoaded msg -> Options -> PaneContext -> PaneContext
initialisePane track options pane =
    { pane
        | thirdPersonContext =
            Just <|
                ViewThirdPerson.initialiseView 0 track.trackTree pane.thirdPersonContext
        , profileContext =
            Just <|
                ViewProfileCharts.initialiseView 0 track.trackTree pane.profileContext
        , planContext =
            Just <|
                ViewPlan.initialiseView 0 track.trackTree pane.planContext
        , mapContext =
            Just <|
                ViewMap.initialiseContext pane.mapContext
    }


viewModeChoices : (Msg -> msg) -> PaneContext -> Element msg
viewModeChoices msgWrapper context =
    let
        fullOptionList =
            [ Input.optionWith ViewMap <| radioButton "Map"
            , Input.optionWith ViewThird <| radioButton "Perspective"
            , Input.optionWith ViewProfile <| radioButton "Profile"
            , Input.optionWith ViewPlan <| radioButton "Plan"
            ]
    in
    Input.radioRow
        [ spacing 5
        , paddingEach { top = 4, left = 4, bottom = 0, right = 0 }
        ]
        { onChange = msgWrapper << SetViewMode context.paneId
        , selected = Just context.activeView
        , label = Input.labelHidden "Choose view"
        , options = fullOptionList
        }


viewModeChoicesNoMap : (Msg -> msg) -> PaneContext -> Element msg
viewModeChoicesNoMap msgWrapper pane =
    let
        reducedOptionList =
            [ Input.optionWith ViewThird <| radioButton "Perspective"
            , Input.optionWith ViewProfile <| radioButton "Profile"
            , Input.optionWith ViewPlan <| radioButton "Plan"
            ]
    in
    Input.radioRow
        [ spacing 5
        , padding 5
        ]
        { onChange = msgWrapper << SetViewMode pane.paneId
        , selected = Just pane.activeView
        , label = Input.labelHidden "Choose view"
        , options = reducedOptionList
        }


takeHalf qty =
    qty |> Quantity.toFloatQuantity |> Quantity.half |> Quantity.truncate


dimensionsWithLayout layout ( w, h ) =
    case layout of
        PanesOne ->
            ( w, h )

        PanesLeftRight ->
            ( takeHalf w, h )

        PanesUpperLower ->
            ( w, takeHalf h |> Quantity.minus (Pixels.pixels 20) )

        PanesOnePlusTwo ->
            -- Later, not that simple
            ( w, h )

        PanesGrid ->
            ( takeHalf w, takeHalf h |> Quantity.minus (Pixels.pixels 20) )


viewPanes :
    (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Options
    -> Element msg
viewPanes msgWrapper mTrack ( w, h ) options =
    let
        ( paneWidth, paneHeight ) =
            dimensionsWithLayout options.paneLayout ( w, h )

        showNonMapViews pane =
            case pane.activeView of
                ViewThird ->
                    case ( pane.thirdPersonContext, mTrack ) of
                        ( Just context, Just track ) ->
                            ViewThirdPerson.view
                                context
                                ( paneWidth, paneHeight )
                                track
                                options.scene3d
                                (msgWrapper << ThirdPersonViewMessage pane.paneId)

                        _ ->
                            none

                ViewPlan ->
                    case ( pane.planContext, mTrack ) of
                        ( Just context, Just track ) ->
                            ViewPlan.view
                                context
                                ( paneWidth, paneHeight )
                                track
                                options.scene3d
                                (msgWrapper << PlanViewMessage pane.paneId)

                        _ ->
                            none

                ViewProfile ->
                    case ( pane.profileContext, mTrack ) of
                        ( Just context, Just track ) ->
                            ViewProfileCharts.view
                                context
                                ( paneWidth, paneHeight )
                                track
                                (msgWrapper << ProfileViewMessage pane.paneId)

                        _ ->
                            none

                _ ->
                    none

        viewPaneZeroWithMap pane =
            -- The Map DIV must be constructed once only, even before we have a Track,
            -- or the map gets upset. So we use CSS to show and hide these elements.
            column [ width fill, centerX ]
                [ viewModeChoices msgWrapper pane
                , conditionallyVisible (pane.activeView /= ViewMap) <|
                    showNonMapViews pane
                , conditionallyVisible (pane.activeView == ViewMap) <|
                    ViewMap.view
                        ( paneWidth, paneHeight )
                        pane.mapContext
                        (msgWrapper << MapViewMessage)
                ]

        viewPaneNoMap pane =
            column [ width fill, alignTop, centerX ]
                [ viewModeChoicesNoMap msgWrapper pane
                , showNonMapViews pane
                ]

        slider =
            case mTrack of
                Just track ->
                    el [ centerX ] <|
                        Input.slider
                            (ViewPureStyles.wideSliderStylesWithWidth w)
                            { onChange = round >> SetCurrentPosition >> msgWrapper
                            , value = toFloat track.currentPosition
                            , label = Input.labelHidden "Current position slider"
                            , min = 0
                            , max = toFloat <| skipCount track.trackTree
                            , step = Just 1
                            , thumb = sliderThumb
                            }

                Nothing ->
                    none
    in
    column [ alignTop, width fill ]
        [ wrappedRow [ centerX, width fill ] <|
            case options.paneLayout of
                PanesOne ->
                    [ viewPaneZeroWithMap options.pane1
                    , slider
                    ]

                PanesLeftRight ->
                    [ viewPaneZeroWithMap options.pane1
                    , viewPaneNoMap options.pane2
                    , slider
                    ]

                PanesUpperLower ->
                    [ viewPaneZeroWithMap options.pane1
                    , viewPaneNoMap options.pane2
                    , slider
                    ]

                PanesGrid ->
                    [ viewPaneZeroWithMap options.pane1
                    , viewPaneNoMap options.pane2
                    , viewPaneNoMap options.pane3
                    , viewPaneNoMap options.pane4
                    , slider
                    ]

                PanesOnePlusTwo ->
                    -- Later.
                    [ viewPaneZeroWithMap options.pane1
                    , slider
                    ]
        ]


encodePaneState : Options -> E.Value
encodePaneState options =
    E.object
        [ ( "layout", E.string <| encodePanesLayout options.paneLayout )
        , ( "pane1", encodeOnePane options.pane1 )
        , ( "pane2", encodeOnePane options.pane2 )
        , ( "pane3", encodeOnePane options.pane3 )
        , ( "pane4", encodeOnePane options.pane4 )
        ]


paneLayoutHelper : List ( PaneLayout, String )
paneLayoutHelper =
    [ ( PanesOne, "One" )
    , ( PanesLeftRight, "LR" )
    , ( PanesUpperLower, "UL" )
    , ( PanesOnePlusTwo, "OneUpTwoDown" )
    , ( PanesGrid, "Grid" )
    ]


encodePanesLayout : PaneLayout -> String
encodePanesLayout layout =
    paneLayoutHelper
        |> List.Extra.find (\entry -> Tuple.first entry == layout)
        |> Maybe.withDefault ( PanesOne, "One" )
        |> Tuple.second


decodePanesLayout : String -> PaneLayout
decodePanesLayout layout =
    paneLayoutHelper
        |> List.Extra.find (\entry -> Tuple.second entry == layout)
        |> Maybe.withDefault ( PanesOne, "One" )
        |> Tuple.first


encodeOnePane : PaneContext -> E.Value
encodeOnePane pane =
    E.object
        [ ( "paneid", E.string <| encodePaneId pane.paneId )
        , ( "activeView", E.string <| encodeView pane.activeView )
        ]


paneIdHelper : List ( PaneId, String )
paneIdHelper =
    [ ( Pane1, "pane1" )
    , ( Pane2, "pane2" )
    , ( Pane3, "pane3" )
    , ( Pane4, "pane4" )
    ]


encodePaneId : PaneId -> String
encodePaneId paneId =
    paneIdHelper
        |> List.Extra.find (\entry -> Tuple.first entry == paneId)
        |> Maybe.withDefault ( Pane1, "pane1" )
        |> Tuple.second


decodePaneId : String -> PaneId
decodePaneId paneId =
    paneIdHelper
        |> List.Extra.find (\entry -> Tuple.second entry == paneId)
        |> Maybe.withDefault ( Pane1, "pane1" )
        |> Tuple.first


viewHelper : List ( ViewMode, String )
viewHelper =
    [ ( ViewInfo, "info" )
    , ( ViewThird, "third" )
    , ( ViewFirst, "first" )
    , ( ViewPlan, "plan" )
    , ( ViewProfile, "profile" )
    , ( ViewMap, "map" )
    ]


encodeView : ViewMode -> String
encodeView view =
    viewHelper
        |> List.Extra.find (\entry -> Tuple.first entry == view)
        |> Maybe.withDefault ( ViewInfo, "info" )
        |> Tuple.second


decodeView : String -> ViewMode
decodeView view =
    viewHelper
        |> List.Extra.find (\entry -> Tuple.second entry == view)
        |> Maybe.withDefault ( ViewInfo, "info" )
        |> Tuple.first


restoreStoredValues : Options -> D.Value -> Options
restoreStoredValues options values =
    case D.decodeValue paneStateDecoder values of
        Ok fromStorage ->
            { defaultOptions
                | paneLayout = decodePanesLayout fromStorage.layoutName
                , popupVisible = False
                , pane1 = applyStoredPaneDetails fromStorage.pane1
                , pane2 = applyStoredPaneDetails fromStorage.pane2
                , pane3 = applyStoredPaneDetails fromStorage.pane3
                , pane4 = applyStoredPaneDetails fromStorage.pane4
            }

        Err error ->
            options


paneStateDecoder =
    D.map5 RestoredOptions
        (D.field "layout" D.string)
        (D.field "pane1" paneDecoder)
        (D.field "pane2" paneDecoder)
        (D.field "pane3" paneDecoder)
        (D.field "pane4" paneDecoder)


paneDecoder =
    D.map2 StoredPane
        (D.field "activeView" D.string)
        (D.field "paneid" D.string)


applyStoredPaneDetails : StoredPane -> PaneContext
applyStoredPaneDetails stored =
    { defaultPaneContext
        | activeView = decodeView stored.activeView
        , paneId = decodePaneId stored.paneId
    }


type alias RestoredOptions =
    { layoutName : String
    , pane1 : StoredPane
    , pane2 : StoredPane
    , pane3 : StoredPane
    , pane4 : StoredPane
    }


type alias StoredPane =
    { activeView : String
    , paneId : String
    }
