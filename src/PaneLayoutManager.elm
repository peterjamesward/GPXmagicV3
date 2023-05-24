module PaneLayoutManager exposing
    ( Msg(..)
    , PaneType(..)
    , StoredPane
    , ViewContext(..)
    , defaultOptions
    , forceRouteView
    , initialise
    , profileViewHandlesClick
    , render
    , restoreStoredValues
    , update
    , viewPanes
    )

import Actions exposing (..)
import Dict exposing (Dict)
import DomainModel exposing (skipCount)
import Element exposing (..)
import Element.Input as Input
import FeatherIcons
import Json.Decode as D
import Json.Encode as E
import Length
import List.Extra
import PaneContext exposing (PaneContext, PaneId(..), PaneLayout(..), PaneLayoutOptions, SliderState(..), paneIdToString)
import Pixels exposing (Pixels)
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import SceneBuilder3D
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import Tools.Flythrough
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Tracks as Tracks
import Tools.TracksOptions as Tracks
import ToolsController
import TrackLoaded exposing (TrackLoaded)
import View3dCommonElements
import ViewFirstPerson
import ViewGraph
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
    , profileContext = Nothing
    , planContext = Nothing
    , graphContext = Nothing
    }


defaultOptions : PaneLayoutOptions
defaultOptions =
    { paneLayout = PanesOne
    , popupVisible = False
    , pane1 = { defaultPaneContext | paneId = Pane1 }
    , pane2 = { defaultPaneContext | paneId = Pane2 }
    , pane3 = { defaultPaneContext | paneId = Pane3 }
    , pane4 = { defaultPaneContext | paneId = Pane4 }
    , sliderState = SliderIdle
    , scene3d = []
    , viewBeforeRouteViewForced = Nothing
    }


type Msg
    = SetPaneLayout PaneLayout
    | SetCurrentPosition Int
    | TogglePopup
    | SetViewMode PaneId ViewMode
    | ThirdPersonViewMessage PaneId View3dCommonElements.Msg
    | ProfileViewMessage PaneId ViewProfileChartContext.Msg
    | PlanViewMessage PaneId ViewPlan.Msg
    | GraphViewMessage PaneId ViewGraph.Msg
    | PaneNoOp


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
                ++ (SceneBuilder3D.renderGroundPlane toolSettings.displaySettings <| Tracks.boundingBox tracks)
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
    -> ( PaneLayoutOptions, Tracks.Options msg, List (ToolAction msg) )
update paneMsg msgWrapper tracks contentArea options previews =
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
            -> (PaneContext -> Tracks.Options msg -> ( PaneContext, Tracks.Options msg, List (ToolAction msg) ))
            -> ( PaneLayoutOptions, Tracks.Options msg, List (ToolAction msg) )
        updatePaneWith id updateFn =
            -- Helper avoids tedious repetition of these case statements.
            let
                ( updatedPane, tracksOut, actions ) =
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
            ( updatedOptions, tracksOut, actions )
    in
    case paneMsg of
        PaneNoOp ->
            ( options, tracks, [] )

        SetPaneLayout paneLayout ->
            let
                newOptions =
                    { options | paneLayout = paneLayout }
            in
            ( newOptions
            , tracks
            , [ StoreLocally "panes" <| encodePaneState newOptions ]
            )

        TogglePopup ->
            ( { options | popupVisible = not options.popupVisible }, tracks, [] )

        SetViewMode paneId viewMode ->
            let
                ( newOptions, _, _ ) =
                    updatePaneWith paneId
                        (\pane _ ->
                            ( { pane
                                | activeView = viewMode
                              }
                            , tracks
                            , []
                            )
                        )
            in
            ( newOptions
            , tracks
            , [ StoreLocally "panes" <| encodePaneState newOptions ]
            )

        ThirdPersonViewMessage paneId imageMsg ->
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( PaneContext, Tracks.Options msg, List (ToolAction msg) )
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

                        ( newContext, actions ) =
                            case ( mTrack, effectiveContext ) of
                                ( Just track, Just context ) ->
                                    let
                                        ( newThirdContext, act ) =
                                            ViewThirdPerson.update
                                                imageMsg
                                                (msgWrapper << ThirdPersonViewMessage Pane1)
                                                track
                                                (dimensionsWithLayout options.paneLayout contentArea)
                                                context
                                    in
                                    ( Just newThirdContext, act )

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
                    in
                    ( newPane, tracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        PlanViewMessage paneId imageMsg ->
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( PaneContext, Tracks.Options msg, List (ToolAction msg) )
                paneUpdateFunction paneInfo _ =
                    let
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
                    in
                    ( newPane, tracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        GraphViewMessage paneId imageMsg ->
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( PaneContext, Tracks.Options msg, List (ToolAction msg) )
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
                    ( newPane, newTracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        ProfileViewMessage paneId imageMsg ->
            --TODO: Stop sharing this message type between two profile types.
            let
                paneUpdateFunction :
                    PaneContext
                    -> Tracks.Options msg
                    -> ( PaneContext, Tracks.Options msg, List (ToolAction msg) )
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
                    ( newPane, tracks, actions )
            in
            updatePaneWith paneId paneUpdateFunction

        SetCurrentPosition pos ->
            -- Slider moves pointer and re-centres view.
            -- The actions will re-render and repaint the map.
            ( { options | sliderState = SliderMoved }
            , tracks
            , [ SetCurrent pos
              , PointerChange
              ]
            )


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


initialise : TrackLoaded msg -> PaneLayoutOptions -> PaneLayoutOptions
initialise track options =
    { options
        | pane1 = initialisePane track options options.pane1
        , pane2 = initialisePane track options options.pane2
        , pane3 = initialisePane track options options.pane3
        , pane4 = initialisePane track options options.pane4
    }


initialisePane : TrackLoaded msg -> PaneLayoutOptions -> PaneContext -> PaneContext
initialisePane track options pane =
    { pane
        | activeView = pane.activeView
        , thirdPersonContext = Just <| ViewThirdPerson.initialiseView 0 track.trackTree pane.thirdPersonContext
        , firstPersonContext = Just <| ViewThirdPerson.initialiseView 0 track.trackTree pane.firstPersonContext
        , profileContext =
            Just <|
                ViewProfileChartsCanvas.initialiseView
                    (paneIdToString pane.paneId)
                    track.trackTree
                    pane.profileContext
        , planContext = Just <| ViewPlan.initialiseView 0 track.trackTree pane.planContext
        , graphContext = Just <| ViewGraph.initialiseView 0 track.trackTree pane.graphContext
    }


viewModeChoices :
    SystemSettings
    -> (Msg -> msg)
    -> PaneContext
    -> PaneLayoutOptions
    -> Element msg
viewModeChoices settings msgWrapper context options =
    let
        localise =
            I18N.localisedString settings.location "panes"

        fullOptionList =
            [ Input.optionWith ViewThird <| viewModeTab Mid <| localise "Perspective"
            , Input.optionWith ViewFirst <| viewModeTab Mid <| localise "Rider"
            , Input.optionWith ViewProfileCanvas <| viewModeTab Mid <| localise "Profile"
            , Input.optionWith ViewProfileWebGL <| viewModeTab Mid <| localise "OldProfile"
            , Input.optionWith ViewPlan <| viewModeTab Mid <| localise "Plan"
            , Input.optionWith ViewGraph <| viewModeTab Last <| localise "Route"

            --, Input.optionWith ViewDerivatives <| viewModeTab Last <| localise "Calculus"
            --, Input.optionWith ViewInfo <| viewModeTab Last <| localise "About"
            ]
    in
    Input.radioRow
        []
        { onChange = msgWrapper << SetViewMode context.paneId
        , selected = Just context.activeView
        , label = Input.labelHidden "Choose view"
        , options = fullOptionList
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

        PanesGrid ->
            ( takeHalf w, takeHalf h |> Quantity.minus (Pixels.pixels 20) )


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
    -> Tracks.Options msg
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PaneLayoutOptions
    -> Maybe Tools.Flythrough.Flythrough
    -> Dict String PreviewData
    -> Element msg
viewPanes settings msgWrapper tracksOptions displayOptions ( w, h ) options mFlythrough previews =
    let
        mTrack =
            Tracks.getActiveTrack tracksOptions

        ( paneWidth, paneHeight ) =
            dimensionsWithLayout options.paneLayout ( w, h )

        showNonMapViews : PaneContext -> Element msg
        showNonMapViews pane =
            --TODO: elm-map means we can be much simpler.
            case pane.activeView of
                ViewThird ->
                    case ( pane.thirdPersonContext, mTrack ) of
                        ( Just context, Just track ) ->
                            ViewThirdPerson.view
                                settings
                                context
                                displayOptions
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
                                settings
                                displayOptions
                                ( paneWidth, paneHeight )
                                track
                                options.scene3d
                                (msgWrapper << PlanViewMessage pane.paneId)

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

        viewPaneNoMap : PaneContext -> Element msg
        viewPaneNoMap context =
            column [ width fill, centerX ]
                [ viewModeChoices settings msgWrapper context options
                , showNonMapViews context
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

        numberOfPanes =
            case options.paneLayout of
                PanesOne ->
                    1

                PanesLeftRight ->
                    2

                PanesUpperLower ->
                    2

                PanesGrid ->
                    4
    in
    column
        [ centerX
        , width fill
        , spacing 5
        , paddingEach { left = 10, right = 0, top = 0, bottom = 0 }
        , inFront <| paneLayoutMenu settings.location msgWrapper options
        ]
        [ wrappedRow
            [ centerX, width fill, spacing 5 ]
          <|
            List.take numberOfPanes
                [ viewPaneNoMap options.pane1
                , viewPaneNoMap options.pane2
                , viewPaneNoMap options.pane3
                , viewPaneNoMap options.pane4
                ]
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
