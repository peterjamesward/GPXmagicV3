module PaneLayoutManager exposing
    ( Msg(..)
    , PaneType(..)
    , StoredPane
    , ViewContext(..)
    , defaultOptions
    , exitRouteView
    , forceMapView
    , forceRouteView
    , initialise
    , paintProfileCharts
    , profileViewHandlesClick
    , render
    , resizeOccured
    , restoreStoredValues
    , subscriptions
    , trackChanged
    , update
    , viewPanes
    )

import Actions exposing (..)
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Json.Encode as E
import Length
import List.Extra
import MapPortController
import MapStyles
import MapViewer
import PaneContext exposing (PaneContext, PaneId(..), PaneLayout(..), PaneLayoutOptions, SliderState(..), paneIdToString)
import Pixels exposing (Pixels)
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import SceneBuilder3D
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import Tools.Flythrough
import Tools.GraphOptions exposing (Graph)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.NamedSegmentOptions exposing (NamedSegment)
import Tools.Tracks as Tracks
import Tools.TracksOptions as Tracks
import ToolsController
import TrackLoaded exposing (TrackLoaded)
import View3dCommonElements
import ViewFirstPerson
import ViewGraph
import ViewMap
import ViewMapContext
import ViewMode exposing (ViewMode(..))
import ViewPlan
import ViewProfileChartContext
import ViewProfileChartsCanvas
import ViewProfileChartsWebGL
import ViewPureStyles exposing (..)
import ViewThirdPerson


type ViewContext
    = ThirdPersonContext View3dCommonElements.Context


type PaneType
    = PaneWithMap


defaultPaneContext : PaneContext
defaultPaneContext =
    { paneId = Pane1
    , activeView = ViewThird
    , thirdPersonContext = Nothing
    , firstPersonContext = Nothing
    , mapContext = Nothing
    , profileContext = Nothing
    , planContext = Nothing
    , graphContext = Nothing
    }


alwaysShowsMapContext : PaneContext
alwaysShowsMapContext =
    { defaultPaneContext
        | mapContext = Just ViewMapContext.default
    }


defaultOptions : PaneLayoutOptions
defaultOptions =
    { paneLayout = PanesOne
    , popupVisible = False
    , pane1 = alwaysShowsMapContext
    , pane2 = { defaultPaneContext | paneId = Pane2 }
    , pane3 = { defaultPaneContext | paneId = Pane3 }
    , pane4 = { defaultPaneContext | paneId = Pane4 }
    , sliderState = SliderIdle
    , scene3d = []
    , mapState = MapPortController.defaultMapState
    , viewBeforeRouteViewForced = Nothing
    , mapData =
        MapViewer.initMapData
            "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/dinProMediumEncoded.json"
            MapStyles.mapStyle
    }


type Msg
    = SetPaneLayout PaneLayout
    | SetCurrentPosition Int
    | TogglePopup
    | SetViewMode PaneId ViewMode
    | ThirdPersonViewMessage PaneId View3dCommonElements.Msg
    | ProfileViewMessage PaneId ViewProfileChartContext.Msg
    | PlanViewMessage PaneId View3dCommonElements.Msg
    | GraphViewMessage PaneId ViewGraph.Msg
    | MapPortsMessage MapPortController.MapMsg
    | MapViewMessage ViewMap.Msg
    | PaneNoOp
    | DeferredProfile PaneId ViewProfileChartContext.ProfileContext


subscriptions : PaneLayoutOptions -> Sub Msg
subscriptions options =
    let
        panes =
            [ options.pane1
            , options.pane2
            , options.pane3
            , options.pane4
            ]

        subsForPane : PaneContext -> Maybe (Sub Msg)
        subsForPane pane =
            case pane.activeView of
                ViewFirst ->
                    case pane.firstPersonContext of
                        Just context ->
                            ViewFirstPerson.subscriptions options.mapData context
                                |> Sub.map (ThirdPersonViewMessage pane.paneId)
                                |> Just

                        Nothing ->
                            Nothing

                ViewThird ->
                    case pane.thirdPersonContext of
                        Just context ->
                            ViewThirdPerson.subscriptions options.mapData context
                                |> Sub.map (ThirdPersonViewMessage pane.paneId)
                                |> Just

                        Nothing ->
                            Nothing

                ViewPlan ->
                    case pane.planContext of
                        Just context ->
                            ViewPlan.subscriptions options.mapData context
                                |> Sub.map (PlanViewMessage pane.paneId)
                                |> Just

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
    in
    Sub.batch <| List.filterMap subsForPane panes


paneLayoutMenu : I18NOptions.Location -> (Msg -> msg) -> PaneLayoutOptions -> Element msg
paneLayoutMenu location msgWrapper options =
    el
        [ alignRight, alignTop, moveLeft 20 ]
    <|
        Input.radioRow []
            { options = optionList location
            , onChange = msgWrapper << SetPaneLayout
            , selected = Just options.paneLayout
            , label = Input.labelHidden "layout"
            }


optionList location =
    let
        localise =
            I18N.text location "panes"
    in
    [ Input.optionWith PanesOne <| layoutModeTab First (useIconWithSize 12 FeatherIcons.square)
    , Input.optionWith PanesLeftRight <| layoutModeTab Mid (useIconWithSize 12 FeatherIcons.columns)
    , Input.optionWith PanesUpperLower <|
        layoutModeTab Mid
            (el
                [ rotate (pi / 2) ]
                (useIconWithSize 12 FeatherIcons.columns)
            )
    , Input.optionWith PanesGrid <| layoutModeTab Last (useIconWithSize 12 FeatherIcons.grid)
    ]


render :
    ToolsController.Options msg
    -> PaneLayoutOptions
    -> Tracks.Options msg
    -> Tools.DisplaySettingsOptions.Options
    -> Dict String PreviewData
    -> PaneLayoutOptions
render toolSettings options tracks display previews =
    --Profile stuff now lives in the pane context, as each pane could
    --have different version!
    let
        renderTrack track active =
            if active then
                SceneBuilder3D.render3dView
                    toolSettings.displaySettings
                    track

            else
                SceneBuilder3D.renderInactiveView track
    in
    { options
        | scene3d =
            SceneBuilder3D.renderPreviews display previews
                ++ (List.concat <| Tracks.mapOverVisibleTracks renderTrack tracks)
                ++ (SceneBuilder3D.renderKeyPlaces display <| Tracks.getKeyPlaces tracks)
    }


update :
    Msg
    -> (Msg -> msg)
    -> Tracks.Options msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PaneLayoutOptions
    -> Dict String PreviewData
    -> Maybe String
    -> ( PaneLayoutOptions, Tracks.Options msg, List (ToolAction msg) )
update paneMsg msgWrapper tracks contentArea options previews paintTool =
    let
        mTrack =
            Tracks.getActiveTrack tracks

        currentPane id =
            case id of
                Pane1 ->
                    options.pane1

                Pane2 ->
                    options.pane2

                Pane3 ->
                    options.pane3

                Pane4 ->
                    options.pane4

        updatePaneWith :
            PaneId
            ->
                (PaneContext
                 -> Tracks.Options msg
                 -> ( ( PaneContext, MapViewer.MapData ), Tracks.Options msg, List (ToolAction msg) )
                )
            -> ( PaneLayoutOptions, Tracks.Options msg, List (ToolAction msg) )
        updatePaneWith id updateFn =
            -- Helper avoids tedious repetition of these case statements.
            let
                ( ( updatedPane, newMapData ), tracksOut, actions ) =
                    updateFn (currentPane id) tracks

                updatedOptions =
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
            ( { updatedOptions | mapData = newMapData }
            , tracksOut
            , actions
            )
    in
    case paneMsg of
        PaneNoOp ->
            ( options, tracks, [] )

        SetPaneLayout paneLayout ->
            let
                newOptions =
                    { options | paneLayout = paneLayout }
                        |> resizeOccured contentArea
            in
            ( newOptions
            , tracks
            , [ MapRefresh
              , StoreLocally "panes" <| encodePaneState newOptions
              ]
            )

        TogglePopup ->
            ( { options | popupVisible = not options.popupVisible }, tracks, [] )

        SetViewMode paneId viewMode ->
            let
                ( newOptions, _, paneActions ) =
                    updatePaneWith paneId
                        (\pane _ ->
                            ( ( { pane | activeView = viewMode }
                              , options.mapData
                              )
                            , tracks
                            , case ( viewMode, pane.profileContext ) of
                                ( ViewProfileCanvas, Just profile ) ->
                                    -- Wait until DIV is created before loading chart data.
                                    [ DelayMessage 500 (msgWrapper <| DeferredProfile paneId profile) ]

                                ( _, Just profile ) ->
                                    -- If old mode was profile, destroy the charts.
                                    if pane.activeView == ViewProfileCanvas then
                                        [ ExternalCommand <|
                                            MapPortController.destroyCharts profile
                                        ]

                                    else
                                        []

                                _ ->
                                    []
                            )
                        )
            in
            ( newOptions
            , tracks
            , [ MapRefresh
              , StoreLocally "panes" <| encodePaneState newOptions
              ]
                ++ paneActions
            )

        DeferredProfile paneId profileContext ->
            -- We let the `view` complete so that the canvas DIV is present, then render the data,
            ( options
            , tracks
            , [ RenderProfile profileContext ]
            )

        ThirdPersonViewMessage paneId imageMsg ->
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( ( PaneContext, MapViewer.MapData ), Tracks.Options msg, List (ToolAction msg) )
                paneUpdateFunction paneInfo _ =
                    let
                        effectiveContext =
                            case paneInfo.activeView of
                                ViewFirst ->
                                    paneInfo.firstPersonContext

                                ViewThird ->
                                    paneInfo.thirdPersonContext

                                _ ->
                                    Nothing

                        ( newContext, actions, newMapDataFromPane ) =
                            case ( mTrack, effectiveContext ) of
                                ( Just track, Just context ) ->
                                    let
                                        ( newContextFromView, act, newMapData ) =
                                            ViewThirdPerson.update
                                                imageMsg
                                                (msgWrapper << ThirdPersonViewMessage paneId)
                                                track
                                                (dimensionsWithLayout options.paneLayout contentArea)
                                                options.mapData
                                                context
                                                paintTool
                                    in
                                    ( Just newContextFromView, act, newMapData )

                                _ ->
                                    ( Nothing, [], options.mapData )

                        newPane =
                            case paneInfo.activeView of
                                ViewFirst ->
                                    { paneInfo | firstPersonContext = newContext }

                                ViewThird ->
                                    { paneInfo | thirdPersonContext = newContext }

                                _ ->
                                    paneInfo
                    in
                    ( ( newPane, newMapDataFromPane ), tracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        PlanViewMessage paneId imageMsg ->
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( ( PaneContext, MapViewer.MapData ), Tracks.Options msg, List (ToolAction msg) )
                paneUpdateFunction paneInfo _ =
                    let
                        ( newContext, actions, newMapDataFromPane ) =
                            case ( mTrack, paneInfo.planContext ) of
                                ( Just track, Just planContext ) ->
                                    let
                                        ( new, act, newMapData ) =
                                            ViewPlan.update
                                                imageMsg
                                                (msgWrapper << PlanViewMessage paneId)
                                                track
                                                (dimensionsWithLayout options.paneLayout contentArea)
                                                planContext
                                                options.mapData
                                                paintTool
                                    in
                                    ( Just new, act, newMapData )

                                _ ->
                                    ( Nothing, [], options.mapData )

                        newPane =
                            { paneInfo | planContext = newContext }
                    in
                    ( ( newPane, newMapDataFromPane ), tracks, actions )

                ( newPaneOptions, newOptions, newActions ) =
                    updatePaneWith paneId paneUpdateFunction
            in
            ( newPaneOptions
            , newOptions
            , newActions
            )

        GraphViewMessage paneId imageMsg ->
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( ( PaneContext, MapViewer.MapData ), Tracks.Options msg, List (ToolAction msg) )
                paneUpdateFunction paneInfo _ =
                    let
                        ( newContext, newTracks, actions ) =
                            case paneInfo.graphContext of
                                Just graphContext ->
                                    let
                                        ( innerContext, innerTracks, innerActions ) =
                                            ViewGraph.update
                                                imageMsg
                                                (msgWrapper << GraphViewMessage Pane1)
                                                tracks
                                                (dimensionsWithLayout options.paneLayout contentArea)
                                                graphContext
                                    in
                                    ( Just innerContext, innerTracks, innerActions )

                                _ ->
                                    ( Nothing, tracks, [] )

                        newPane =
                            { paneInfo | graphContext = newContext }
                    in
                    ( ( newPane, options.mapData ), newTracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        ProfileViewMessage paneId imageMsg ->
            --TODO: Stop sharing this message type between two profile types.
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( ( PaneContext, MapViewer.MapData ), Tracks.Options msg, List (ToolAction msg) )
                paneUpdateFunction paneInfo _ =
                    let
                        ( newContext, actions ) =
                            case ( mTrack, paneInfo.profileContext ) of
                                ( Just track, Just profile ) ->
                                    let
                                        ( new, act ) =
                                            if (currentPane paneId).activeView == ViewProfileCanvas then
                                                ViewProfileChartsCanvas.update
                                                    imageMsg
                                                    (msgWrapper << ProfileViewMessage Pane1)
                                                    track
                                                    (dimensionsWithLayout options.paneLayout contentArea)
                                                    previews
                                                    profile

                                            else
                                                ViewProfileChartsWebGL.update
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
                    in
                    ( ( newPane, options.mapData ), tracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        MapViewMessage mapViewMsg ->
            -- Can only be pane 1!
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
            ( { options | pane1 = newPane }, tracks, actions )

        MapPortsMessage mapMsg ->
            let
                ( newState, actions ) =
                    MapPortController.update mapMsg mTrack options.mapState
            in
            ( { options | mapState = newState }
            , tracks
            , actions
            )

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
            , tracks
            , [ SetCurrent pos
              , PointerChange
              , if mapFollowsOrange then
                    MapCenterOnCurrent

                else
                    Actions.NoAction
              ]
            )


paneSpacing =
    5


paneModeHeight =
    20


resizeOccured : ( Quantity Int Pixels, Quantity Int Pixels ) -> PaneLayoutOptions -> PaneLayoutOptions
resizeOccured ( w, h ) options =
    let
        paneArea =
            dimensionsWithLayout options.paneLayout ( w, h )

        resizePane : PaneContext -> PaneContext
        resizePane pane =
            case pane.activeView of
                ViewPlan ->
                    { pane | planContext = Maybe.map (ViewPlan.resizeOccured paneArea) pane.planContext }

                ViewThird ->
                    { pane | thirdPersonContext = Maybe.map (ViewThirdPerson.resizeOccured paneArea) pane.thirdPersonContext }

                ViewFirst ->
                    { pane | firstPersonContext = Maybe.map (ViewFirstPerson.resizeOccured paneArea) pane.firstPersonContext }

                _ ->
                    pane
    in
    { options
        | pane1 = resizePane options.pane1
        , pane2 = resizePane options.pane2
        , pane3 = resizePane options.pane3
        , pane4 = resizePane options.pane4
    }


isViewVisible : ViewMode -> PaneLayoutOptions -> Bool
isViewVisible mode options =
    List.any (\pane -> pane.activeView == mode) <|
        case options.paneLayout of
            PanesOne ->
                [ options.pane1 ]

            PanesLeftRight ->
                [ options.pane1, options.pane2 ]

            PanesUpperLower ->
                [ options.pane1, options.pane2 ]

            PanesOnePlusTwo ->
                [ options.pane1, options.pane2, options.pane3 ]

            PanesGrid ->
                [ options.pane1, options.pane2, options.pane3, options.pane4 ]


forceRouteView : PaneLayoutOptions -> PaneLayoutOptions
forceRouteView options =
    if isViewVisible ViewGraph options then
        options

    else
        let
            pane1 =
                options.pane1
        in
        { options
            | pane1 = { pane1 | activeView = ViewGraph }
            , viewBeforeRouteViewForced = Just options.pane1.activeView
        }


forceMapView : PaneLayoutOptions -> PaneLayoutOptions
forceMapView options =
    if isViewVisible ViewMap options then
        options

    else
        let
            pane1 =
                options.pane1
        in
        { options
            | pane1 = { pane1 | activeView = ViewMap }
        }


exitRouteView : PaneLayoutOptions -> PaneLayoutOptions
exitRouteView options =
    let
        pane1 =
            options.pane1
    in
    case ( options.viewBeforeRouteViewForced, pane1.activeView ) of
        ( Just savedView, ViewGraph ) ->
            { options
                | pane1 = { pane1 | activeView = savedView }
                , viewBeforeRouteViewForced = Nothing
            }

        _ ->
            options


initialise :
    TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PaneLayoutOptions
    -> PaneLayoutOptions
initialise track contentArea options =
    let
        ( paneWidth, paneHeight ) =
            dimensionsWithLayout options.paneLayout contentArea
    in
    { options
        | pane1 = initialisePane track options ( paneWidth, paneHeight ) options.pane1
        , pane2 = initialisePane track options ( paneWidth, paneHeight ) options.pane2
        , pane3 = initialisePane track options ( paneWidth, paneHeight ) options.pane3
        , pane4 = initialisePane track options ( paneWidth, paneHeight ) options.pane4
        , mapData =
            MapViewer.initMapData
                "https://raw.githubusercontent.com/MartinSStewart/elm-map/master/public/dinProMediumEncoded.json"
                MapStyles.mapStyle
    }


initialisePane :
    TrackLoaded msg
    -> PaneLayoutOptions
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PaneContext
    -> PaneContext
initialisePane track options paneArea pane =
    { pane
        | activeView = pane.activeView
        , thirdPersonContext =
            Just <|
                ViewThirdPerson.initialiseView
                    0
                    paneArea
                    track
                    pane.thirdPersonContext
        , firstPersonContext =
            Just <|
                ViewFirstPerson.initialiseView
                    0
                    paneArea
                    track
                    pane.firstPersonContext
        , profileContext =
            Just <|
                ViewProfileChartsCanvas.initialiseView
                    (paneIdToString pane.paneId)
                    track.trackTree
                    pane.profileContext
        , planContext = Just <| ViewPlan.initialiseView 0 paneArea track pane.planContext
        , graphContext = Just <| ViewGraph.initialiseView 0 track.trackTree pane.graphContext
        , mapContext = Just <| ViewMap.initialiseContext pane.mapContext
    }


viewModeChoices :
    SystemSettings
    -> (Msg -> msg)
    -> PaneContext
    -> PaneLayoutOptions
    -> Bool
    -> Element msg
viewModeChoices settings msgWrapper context options includeMap =
    let
        localise =
            I18N.localisedString settings.location "panes"

        fullOptionList =
            [ Input.optionWith ViewMap <| viewModeTab First <| localise "Map"
            , Input.optionWith ViewThird <| viewModeTab Mid <| localise "Perspective"
            , Input.optionWith ViewFirst <| viewModeTab Mid <| localise "Rider"
            , Input.optionWith ViewProfileCanvas <| viewModeTab Mid <| localise "Profile"
            , Input.optionWith ViewProfileWebGL <| viewModeTab Mid <| localise "OldProfile"
            , Input.optionWith ViewPlan <| viewModeTab Mid <| localise "Plan"
            , Input.optionWith ViewGraph <| viewModeTab Last <| localise "Route"
            ]
    in
    Input.radioRow
        []
        { onChange = msgWrapper << SetViewMode context.paneId
        , selected = Just context.activeView
        , label = Input.labelHidden "Choose view"
        , options =
            if includeMap then
                fullOptionList

            else
                List.drop 1 fullOptionList
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
            ( w, takeHalf h |> Quantity.minus (Pixels.pixels paneModeHeight) )

        PanesOnePlusTwo ->
            -- Later, not that simple
            ( w, h )

        PanesGrid ->
            ( takeHalf w, takeHalf h |> Quantity.minus (Pixels.pixels paneModeHeight) )


trackChanged :
    TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PaneLayoutOptions
    -> PaneLayoutOptions
trackChanged newTrack contentArea options =
    let
        tellPane : PaneContext -> PaneContext
        tellPane pane =
            case pane.activeView of
                ViewPlan ->
                    { pane
                        | planContext =
                            Maybe.map
                                (ViewPlan.trackChanged
                                    newTrack
                                    (dimensionsWithLayout options.paneLayout contentArea)
                                )
                                pane.planContext
                    }

                _ ->
                    pane
    in
    { options
        | pane1 = tellPane options.pane1
        , pane2 = tellPane options.pane2
        , pane3 = tellPane options.pane3
        , pane4 = tellPane options.pane4
    }


paintProfileCharts :
    PaneLayoutOptions
    -> SystemSettings
    -> TrackLoaded msg
    -> Dict String PreviewData
    -> Cmd msg
paintProfileCharts panes settings track previews =
    let
        segments =
            track.namedSegments

        paintIfProfileVisible pane =
            if pane.activeView == ViewProfileCanvas then
                case pane.profileContext of
                    Just context ->
                        Cmd.batch
                            [ MapPortController.paintCanvasProfileChart
                                context
                                settings
                                track
                                previews
                            , MapPortController.paintCanvasGradientChart
                                context
                                settings
                                track
                            ]

                    Nothing ->
                        Cmd.none

            else
                Cmd.none

        visiblePanes =
            case panes.paneLayout of
                PanesOne ->
                    [ panes.pane1 ]

                PanesLeftRight ->
                    [ panes.pane1, panes.pane2 ]

                PanesUpperLower ->
                    [ panes.pane1, panes.pane2 ]

                PanesOnePlusTwo ->
                    [ panes.pane1, panes.pane2, panes.pane3 ]

                PanesGrid ->
                    [ panes.pane1, panes.pane2, panes.pane3, panes.pane4 ]
    in
    Cmd.batch <| List.map paintIfProfileVisible visiblePanes


profileViewHandlesClick : String -> Length.Length -> PaneLayoutOptions -> TrackLoaded msg -> Maybe Int
profileViewHandlesClick container trackDistance options track =
    case container |> String.split "." |> List.Extra.last of
        Just "1" ->
            ViewProfileChartsCanvas.handleClick trackDistance options.pane1.profileContext track

        Just "2" ->
            ViewProfileChartsCanvas.handleClick trackDistance options.pane2.profileContext track

        Just "3" ->
            ViewProfileChartsCanvas.handleClick trackDistance options.pane3.profileContext track

        Just "4" ->
            ViewProfileChartsCanvas.handleClick trackDistance options.pane4.profileContext track

        _ ->
            Nothing


viewPanes :
    SystemSettings
    -> (Msg -> msg)
    -> ToolsController.Options msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PaneLayoutOptions
    -> Dict String PreviewData
    -> Element msg
viewPanes settings msgWrapper tools ( w, h ) options previews =
    let
        tracksOptions =
            tools.tracksOptions

        displayOptions =
            tools.displaySettings

        paintTool =
            tools.paintTool

        mFlythrough =
            tools.flythroughSettings.flythrough

        mTrack =
            Tracks.getActiveTrack tracksOptions

        ( paneWidth, paneHeight ) =
            dimensionsWithLayout options.paneLayout ( w, h )

        showNonMapViews : PaneContext -> Element msg
        showNonMapViews pane =
            -- Try having all the DIVs there but hidden.
            conditionallyVisible (pane.activeView /= ViewMap) <|
                case pane.activeView of
                    ViewThird ->
                        case ( pane.thirdPersonContext, mTrack ) of
                            ( Just context, Just track ) ->
                                ViewThirdPerson.view
                                    settings
                                    options.mapData
                                    context
                                    displayOptions
                                    tools
                                    ( paneWidth, paneHeight )
                                    track
                                    options.scene3d
                                    (msgWrapper << ThirdPersonViewMessage pane.paneId)
                                    paintTool

                            _ ->
                                none

                    ViewFirst ->
                        case ( pane.firstPersonContext, mTrack ) of
                            ( Just context, Just track ) ->
                                ViewFirstPerson.view
                                    settings
                                    context
                                    displayOptions
                                    options.mapData
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
                                    options.mapData
                                    settings
                                    displayOptions
                                    tools
                                    ( paneWidth, paneHeight )
                                    track
                                    options.scene3d
                                    (msgWrapper << PlanViewMessage pane.paneId)
                                    paintTool

                            _ ->
                                none

                    ViewGraph ->
                        case ( pane.graphContext, mTrack ) of
                            ( Just context, Just _ ) ->
                                ViewGraph.view
                                    settings
                                    context
                                    ( paneWidth, paneHeight )
                                    tracksOptions
                                    (msgWrapper << GraphViewMessage pane.paneId)

                            _ ->
                                none

                    ViewProfileCanvas ->
                        case pane.profileContext of
                            Just context ->
                                ViewProfileChartsCanvas.view
                                    context
                                    settings
                                    pane.paneId
                                    ( paneWidth, paneHeight )
                                    (msgWrapper << ProfileViewMessage pane.paneId)

                            _ ->
                                none

                    ViewProfileWebGL ->
                        case ( pane.profileContext, mTrack ) of
                            ( Just context, Just track ) ->
                                ViewProfileChartsWebGL.view
                                    context
                                    settings
                                    ( paneWidth, paneHeight )
                                    track
                                    (msgWrapper << ProfileViewMessage pane.paneId)
                                    previews

                            _ ->
                                none

                    ViewMap ->
                        none

        viewPaneZeroWithMap : PaneContext -> Element msg
        viewPaneZeroWithMap pane =
            -- The Map DIV must be constructed once only, even before we have a Track,
            -- or the map gets upset. So we use CSS to show and hide these elements.
            column [ width fill, centerX ]
                [ viewModeChoices settings msgWrapper pane options True
                , showNonMapViews pane
                , conditionallyVisible (pane.activeView == ViewMap) <|
                    ViewMap.view
                        settings
                        mTrack
                        ( paneWidth, paneHeight )
                        pane.mapContext
                        (msgWrapper << MapViewMessage)
                ]

        viewPaneNoMap : PaneContext -> Element msg
        viewPaneNoMap pane =
            column [ width fill, centerX ]
                [ viewModeChoices settings msgWrapper pane options False
                , showNonMapViews pane
                ]

        slider =
            case mTrack of
                Just track ->
                    el [ centerX, alignBottom ] <|
                        Input.slider
                            (ViewPureStyles.wideSliderStylesWithWidth w)
                            { onChange = round >> SetCurrentPosition >> msgWrapper
                            , value = toFloat track.currentPosition
                            , label = Input.labelHidden "position"
                            , min = 0
                            , max = toFloat <| skipCount track.trackTree
                            , step = Just 1
                            , thumb = sliderThumb
                            }

                Nothing ->
                    none
    in
    column
        [ centerX
        , width fill
        , spacing 5
        , paddingEach { left = 10, right = 0, top = 0, bottom = 0 }
        , inFront <| paneLayoutMenu settings.location msgWrapper options
        ]
        [ wrappedRow
            [ centerX, width fill, spacing paneSpacing ]
          <|
            case options.paneLayout of
                PanesOne ->
                    [ viewPaneZeroWithMap options.pane1 ]

                PanesLeftRight ->
                    [ viewPaneZeroWithMap options.pane1
                    , viewPaneNoMap options.pane2
                    ]

                PanesUpperLower ->
                    [ viewPaneZeroWithMap options.pane1
                    , viewPaneNoMap options.pane2
                    ]

                PanesGrid ->
                    [ viewPaneZeroWithMap options.pane1
                    , viewPaneNoMap options.pane2
                    , viewPaneNoMap options.pane3
                    , viewPaneNoMap options.pane4
                    ]

                PanesOnePlusTwo ->
                    -- Later.
                    [ viewPaneZeroWithMap options.pane1 ]
        , slider
        ]


encodePaneState : PaneLayoutOptions -> E.Value
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
    [ ( ViewThird, "third" )
    , ( ViewFirst, "first" )
    , ( ViewPlan, "plan" )
    , ( ViewProfileCanvas, "profile" )
    , ( ViewProfileWebGL, "profNew" )
    , ( ViewMap, "map" )
    , ( ViewGraph, "route" )
    ]


encodeView : ViewMode -> String
encodeView view =
    viewHelper
        |> List.Extra.find (\entry -> Tuple.first entry == view)
        |> Maybe.withDefault ( ViewThird, "third" )
        |> Tuple.second


decodeView : String -> ViewMode
decodeView view =
    viewHelper
        |> List.Extra.find (\entry -> Tuple.second entry == view)
        |> Maybe.withDefault ( ViewThird, "third" )
        |> Tuple.first


restoreStoredValues : PaneLayoutOptions -> D.Value -> PaneLayoutOptions
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

        --options
        Err _ ->
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
