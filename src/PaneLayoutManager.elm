module PaneLayoutManager exposing (..)

import Actions exposing (..)
import DomainModel exposing (skipCount)
import Element as E exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (..)
import FeatherIcons
import FlatColors.ChinesePalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import LocalCoords exposing (LocalCoords)
import MapPortController
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import TrackLoaded exposing (TrackLoaded)
import ViewContextThirdPerson exposing (Context)
import ViewMap
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
    = ThirdPersonContext ViewContextThirdPerson.Context
    | MapContext ViewMap.Context
    | InfoContext


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
    , thirdPersonContext : Maybe ViewContextThirdPerson.Context
    , mapContext : Maybe ViewMap.Context
    }


type alias Options =
    { paneLayout : PaneLayout
    , popupVisible : Bool
    , pane1 : PaneContext
    , pane2 : PaneContext
    , pane3 : PaneContext
    , pane4 : PaneContext
    }


defaultPaneContext : PaneContext
defaultPaneContext =
    { paneId = Pane1
    , activeView = ViewInfo
    , thirdPersonContext = Nothing
    , mapContext = Nothing
    }


defaultOptions : Options
defaultOptions =
    { paneLayout = PanesOne
    , popupVisible = False
    , pane1 = defaultPaneContext
    , pane2 = defaultPaneContext
    , pane3 = defaultPaneContext
    , pane4 = defaultPaneContext
    }


type Msg
    = SetPaneLayout PaneLayout
    | SetCurrentPosition Int
    | TogglePopup
    | SetViewMode PaneId ViewMode
    | ImageMessage PaneId ViewThirdPerson.Msg
    | MapPortsMessage MapPortController.MapMsg
    | PaneNoOp


paneLayoutMenu : (Msg -> msg) -> Options -> Element msg
paneLayoutMenu msgWrapper options =
    Input.button
        [ padding 5
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
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
    [ Input.option PanesOne <| row [ spacing 20 ] [ useIcon FeatherIcons.square, E.text "One big one" ]
    , Input.option PanesLeftRight <| row [ spacing 20 ] [ useIcon FeatherIcons.columns, E.text "Wardrobe doors" ]
    , Input.option PanesUpperLower <| row [ spacing 20 ] [ useIcon FeatherIcons.server, E.text "Bunk beds" ]
    , Input.option PanesGrid <| row [ spacing 20 ] [ useIcon FeatherIcons.grid, E.text "Grid of four" ]
    ]


update :
    Msg
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Options
    -> ( Options, List (ToolAction msg) )
update paneMsg msgWrapper mTrack contentArea options =
    case paneMsg of
        PaneNoOp ->
            ( options, [] )

        SetPaneLayout paneLayout ->
            ( { options | paneLayout = paneLayout }, [] )

        TogglePopup ->
            ( { options | popupVisible = not options.popupVisible }, [] )

        SetViewMode pane viewMode ->
            let
                pane1 =
                    options.pane1

                newPane1 =
                    { pane1 | activeView = viewMode }

                newOptions =
                    { options | pane1 = newPane1 }
            in
            ( newOptions, [ Actions.MapCenterOnCurrent ] )

        ImageMessage pane imageMsg ->
            let
                pane1 =
                    options.pane1

                ( newContext, actions ) =
                    case ( mTrack, pane1.thirdPersonContext ) of
                        ( Just track, Just third ) ->
                            let
                                ( new, act ) =
                                    ViewThirdPerson.update
                                        imageMsg
                                        (msgWrapper << ImageMessage Pane1)
                                        track
                                        contentArea
                                        -- need this for hit detection.
                                        third
                            in
                            ( Just new, act )

                        _ ->
                            ( Nothing, [] )

                newPane1 =
                    { pane1 | thirdPersonContext = newContext }

                newOptions =
                    { options | pane1 = newPane1 }
            in
            ( newOptions, actions )

        MapPortsMessage mapMsg ->
            case mTrack of
                Just track ->
                    let
                        actions =
                            MapPortController.update mapMsg track
                    in
                    ( options, actions )

                Nothing ->
                    ( options, [] )

        SetCurrentPosition pos ->
            -- Slider moves pointer and re-centres view.
            -- The actions will re-render and repaint the map.
            ( options
            , [ SetCurrent pos, TrackHasChanged, MapCenterOnCurrent ]
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
                ViewThirdPerson.initialiseView 0 track.trackTree
        , mapContext = Just ViewMap.initialiseContext
        , activeView =
            if pane.activeView == ViewInfo then
                ViewThird

            else
                pane.activeView
    }


viewModeChoices : (Msg -> msg) -> Options -> Element msg
viewModeChoices msgWrapper options =
    let
        fullOptionList =
            [ Input.optionWith ViewThird <| radioButton "Perspective"
            , Input.optionWith ViewMap <| radioButton "Map"
            ]
    in
    Input.radioRow
        [ spacing 5
        , padding 5
        ]
        { onChange = msgWrapper << SetViewMode Pane1
        , selected = Just options.pane1.activeView
        , label = Input.labelHidden "Choose view"
        , options = fullOptionList
        }


viewModeChoicesNoMap : (Msg -> msg) -> PaneId -> PaneContext -> Element msg
viewModeChoicesNoMap msgWrapper paneId settings =
    let
        reducedOptionList =
            [ Input.optionWith ViewThird <| radioButton "Perspective"
            ]
    in
    Input.radioRow
        [ spacing 5
        , padding 5
        ]
        { onChange = msgWrapper << SetViewMode paneId
        , selected = Just settings.activeView
        , label = Input.labelHidden "Choose view"
        , options = reducedOptionList
        }


viewPanes :
    (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> List (Entity LocalCoords)
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Options
    -> Element msg
viewPanes msgWrapper mTrack scene ( w, h ) options =
    let
        takeHalf qty =
            qty |> Quantity.toFloatQuantity |> Quantity.half |> Quantity.truncate

        ( paneWidth, paneHeight ) =
            case options.paneLayout of
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

        showNonMapViews paneId paneContext =
            case ( paneContext, mTrack ) of
                ( Just context, Just track ) ->
                    ViewThirdPerson.view
                        context
                        ( paneWidth, paneHeight )
                        track
                        scene
                        (msgWrapper << ImageMessage Pane1)

                _ ->
                    none

        viewPaneZeroWithMap =
            -- The Map DIV must be constructed once only, even before we have a Track,
            -- or the map gets upset. So we use CSS to show and hide these elements.
            column [ width fill, alignTop, centerX ]
                [ viewModeChoices msgWrapper options
                , conditionallyVisible (options.pane1.activeView /= ViewMap) <|
                    showNonMapViews Pane1 options.pane1.thirdPersonContext
                , conditionallyVisible (options.pane1.activeView == ViewMap) <|
                    ViewMap.view ( paneWidth, paneHeight ) (msgWrapper << MapPortsMessage)
                ]

        viewPaneNoMap paneId paneSettings =
            column [ width fill, alignTop, centerX ]
                [ viewModeChoicesNoMap msgWrapper paneId paneSettings
                , showNonMapViews Pane1 options.pane1.thirdPersonContext
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
                    [ viewPaneZeroWithMap
                    , slider
                    ]

                PanesLeftRight ->
                    [ viewPaneZeroWithMap
                    , viewPaneNoMap Pane2 options.pane2
                    , slider
                    ]

                PanesUpperLower ->
                    [ viewPaneZeroWithMap
                    , viewPaneNoMap Pane2 options.pane2
                    , slider
                    ]

                PanesGrid ->
                    [ viewPaneZeroWithMap
                    , viewPaneNoMap Pane2 options.pane2
                    , viewPaneNoMap Pane3 options.pane3
                    , viewPaneNoMap Pane4 options.pane4
                    , slider
                    ]

                PanesOnePlusTwo ->
                    -- Later.
                    [ viewPaneZeroWithMap
                    , slider
                    ]
        ]
