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
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import SceneBuilder3D
import Tools.Flythrough
import Tools.GraphOptions exposing (Graph)
import ToolsController
import TrackLoaded exposing (TrackLoaded)
import View3dCommonElements exposing (stopProp)
import ViewAbout
import ViewFirstPerson
import ViewGraph
import ViewMap
import ViewPlan
import ViewProfileCharts
import ViewPureStyles exposing (..)
import ViewThirdPerson


type ViewMode
    = ViewInfo
    | ViewThird
    | ViewFirst
    | ViewPlan
    | ViewProfile
    | ViewMap
    | ViewGraph


type ViewContext
    = ThirdPersonContext View3dCommonElements.Context
    | MapContext ViewMap.Context
    | InfoContext
    | ProfileContext
    | GraphContext


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
    , thirdPersonContext : Maybe View3dCommonElements.Context
    , firstPersonContext : Maybe View3dCommonElements.Context
    , mapContext : Maybe ViewMap.Context
    , profileContext : Maybe ViewProfileCharts.Context
    , planContext : Maybe ViewPlan.Context
    , graphContext : Maybe ViewGraph.Context
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
    , firstPersonContext = Nothing
    , mapContext = Nothing
    , profileContext = Nothing
    , planContext = Nothing
    , graphContext = Nothing
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
    | ThirdPersonViewMessage PaneId View3dCommonElements.Msg
    | ProfileViewMessage PaneId View3dCommonElements.Msg
    | PlanViewMessage PaneId ViewPlan.Msg
    | GraphViewMessage PaneId ViewGraph.Msg
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
            , moveLeft 30
            , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always PaneNoOp >> msgWrapper)
            , htmlAttribute (style "z-index" "20")
            ]
        <|
            Input.radio
                (subtleToolStyles
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
    ToolsController.Options msg
    -> Options
    -> Quantity Int Pixels
    -> TrackLoaded msg
    -> Dict String PreviewData
    -> Options
render toolSettings options width track previews =
    --Profile stuff now lives in the pane context, as each pane could
    --have different version!
    { options
        | scene3d =
            SceneBuilder3D.renderPreviews previews
                ++ SceneBuilder3D.render3dView toolSettings.displaySettings track
    }


update :
    Msg
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Graph msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Options
    -> Dict String PreviewData
    -> ( Options, List (ToolAction msg) )
update paneMsg msgWrapper mTrack graph contentArea options previews =
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

                effectiveContext =
                    case paneInfo.activeView of
                        ViewFirst ->
                            paneInfo.firstPersonContext

                        ViewThird ->
                            paneInfo.thirdPersonContext

                        _ ->
                            Nothing

                ( newContext, actions ) =
                    case ( mTrack, effectiveContext ) of
                        ( Just track, Just context ) ->
                            let
                                ( new, act ) =
                                    ViewThirdPerson.update
                                        imageMsg
                                        (msgWrapper << ThirdPersonViewMessage Pane1)
                                        track
                                        (dimensionsWithLayout options.paneLayout contentArea)
                                        context
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane =
                    case paneInfo.activeView of
                        ViewFirst ->
                            { paneInfo | firstPersonContext = newContext }

                        ViewThird ->
                            { paneInfo | thirdPersonContext = newContext }

                        _ ->
                            paneInfo

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

        GraphViewMessage paneId imageMsg ->
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
                    case paneInfo.graphContext of
                        Just graphContext ->
                            let
                                ( new, act ) =
                                    ViewGraph.update
                                        imageMsg
                                        (msgWrapper << GraphViewMessage Pane1)
                                        graph
                                        (dimensionsWithLayout options.paneLayout contentArea)
                                        graphContext
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane =
                    { paneInfo | graphContext = newContext }

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
                                        previews
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
              , TrackHasChanged

              --, RenderProfile
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
        | activeView =
            if pane.activeView == ViewInfo then
                ViewThird

            else
                pane.activeView
        , thirdPersonContext =
            Just <|
                ViewThirdPerson.initialiseView 0 track.trackTree pane.thirdPersonContext
        , firstPersonContext =
            Just <|
                ViewThirdPerson.initialiseView 0 track.trackTree pane.firstPersonContext
        , profileContext =
            Just <|
                ViewProfileCharts.initialiseView 0 track.trackTree pane.profileContext
        , planContext =
            Just <|
                ViewPlan.initialiseView 0 track.trackTree pane.planContext
        , graphContext =
            Just <|
                ViewGraph.initialiseView 0 track.trackTree pane.graphContext
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
            , Input.optionWith ViewFirst <| radioButton "Rider view"
            , Input.optionWith ViewProfile <| radioButton "Profile"
            , Input.optionWith ViewPlan <| radioButton "Plan"
            , Input.optionWith ViewGraph <| radioButton "Route"
            , Input.optionWith ViewInfo <| radioButton "About"
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
            , Input.optionWith ViewFirst <| radioButton "Rider view"
            , Input.optionWith ViewProfile <| radioButton "Profile"
            , Input.optionWith ViewPlan <| radioButton "Plan"
            , Input.optionWith ViewGraph <| radioButton "Route"
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
    -> Tools.GraphOptions.Graph msg
    -> Tools.GraphOptions.Options msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Options
    -> Maybe Tools.Flythrough.Flythrough
    -> Dict String PreviewData
    -> Element msg
viewPanes msgWrapper mTrack graph graphOptions ( w, h ) options mFlythrough previews =
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

                ViewFirst ->
                    case ( pane.thirdPersonContext, mTrack ) of
                        ( Just context, Just track ) ->
                            ViewFirstPerson.view
                                context
                                ( paneWidth, paneHeight )
                                track
                                options.scene3d
                                (msgWrapper << ThirdPersonViewMessage pane.paneId)
                                mFlythrough

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

                ViewGraph ->
                    case ( pane.graphContext, mTrack ) of
                        ( Just context, Just track ) ->
                            ViewGraph.view
                                context
                                ( paneWidth, paneHeight )
                                graph
                                graphOptions
                                (msgWrapper << GraphViewMessage pane.paneId)

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
                                previews

                        _ ->
                            none

                _ ->
                    ViewAbout.view
                        ( paneWidth, paneHeight )

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
            if mTrack == Nothing then
                [ viewPaneZeroWithMap defaultOptions.pane1
                , slider
                ]

            else
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
    , ( ViewGraph, "route" )
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
