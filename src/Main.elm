module Main exposing (main)

import AbruptDirectionChanges
import Actions
import Browser exposing (application)
import Browser.Dom as Dom exposing (getViewport, getViewportOf)
import Browser.Events
import Browser.Navigation exposing (Key)
import Direction2d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Select as Select
import FlatColors.ChinesePalette
import GeoCodeDecoders exposing (IpInfo)
import GpxParser exposing (parseGPXPoints)
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Http
import Json.Encode as E
import LocalCoords exposing (LocalCoords)
import LocalStorage
import MapPortsController
import MyIP
import OAuthPorts as O exposing (randomBytes)
import OAuthTypes as O exposing (OAuthMsg(..))
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import SceneBuilder
import SplitPane.SplitPane as SplitPane exposing (..)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import ToolsProforma exposing (ToolEntry)
import TrackLoaded exposing (TrackLoaded)
import Url exposing (Url)
import ViewContext exposing (ViewContext(..), ViewMode(..))
import ViewContextThirdPerson exposing (ThirdPersonContext)
import ViewMap exposing (MapContext)
import ViewPureStyles exposing (commonLayoutStyles, conditionallyVisible, radioButton, sliderThumb)
import ViewThirdPerson


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | SetCurrentPosition Int
    | SetViewMode ViewMode
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | ImageMessage ViewThirdPerson.Msg
    | MapPortsMessage MapPortsController.MapMsg
    | StorageMessage E.Value
    | SplitLeftDockRightEdge SplitPane.Msg
    | SplitLeftDockInternal SplitPane.Msg
    | SplitRightDockLeftEdge SplitPane.Msg
    | SplitRightDockInternal SplitPane.Msg
    | SplitBottomDockTopEdge SplitPane.Msg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsProforma.ToolMsg


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , ipInfo : Maybe IpInfo
    , stravaAuthentication : O.Model

    -- Track stuff
    , track : Maybe TrackLoaded

    -- Visuals
    , scene : List (Entity LocalCoords)
    , viewMode : ViewMode
    , viewThirdPersonContext : Maybe ThirdPersonContext
    , viewMapContext : Maybe MapContext

    -- Layout stuff
    , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , leftDockInternal : SplitPane.State
    , rightDockLeftEdge : SplitPane.State
    , rightDockInternal : SplitPane.State
    , bottomDockTopEdge : SplitPane.State

    -- Tools
    , tools : List ToolEntry

    -- Tool specific options
    , directionChangeOptions : AbruptDirectionChanges.Options
    }


main : Program (Maybe (List Int)) Model Msg
main =
    -- This is the 'main' from OAuth example.
    application
        { init = Maybe.map StravaAuth.convertBytes >> init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always (OAuthMessage O.NoOp)
        , onUrlChange = always (OAuthMessage O.NoOp)
        , view = view
        }


init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    -- We stitch in the OAuth init stuff somehow here.
    let
        ( authData, authCmd ) =
            StravaAuth.init mflags origin navigationKey OAuthMessage
    in
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , ipInfo = Nothing
      , stravaAuthentication = authData
      , track = Nothing
      , viewDimensions = ( Pixels.pixels 800, Pixels.pixels 500 )
      , scene = []
      , viewMode = ViewInfo
      , viewThirdPersonContext = Nothing
      , viewMapContext = Nothing
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (percentage 0.2 <| Just ( 0.01, 0.4 ))
      , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (percentage 0.4 <| Just ( 0.1, 0.9 ))
      , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (percentage 0.8 <| Just ( 0.6, 0.97 ))
      , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (percentage 0.6 <| Just ( 0.1, 0.9 ))
      , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter (percentage 0.8 <| Just ( 0.6, 0.97 ))
      , tools = ToolsProforma.tools
      , directionChangeOptions = AbruptDirectionChanges.defaultOptions
      }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , LocalStorage.storageListKeys
        , Task.attempt GotWindowSize Dom.getViewport
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
            )

        MapPortsMessage mapMsg ->
            case model.track of
                Just track ->
                    ( model, MapPortsController.update mapMsg track )

                Nothing ->
                    ( model, Cmd.none )

        ReceivedIpDetails response ->
            let
                ipInfo =
                    MyIP.processIpInfo response

                mapInfoWithLocation =
                    case ipInfo of
                        Just ip ->
                            { mapZoom = 10.0
                            , centreLon = ip.longitude
                            , centreLat = ip.latitude
                            }

                        Nothing ->
                            { mapZoom = 1.0
                            , centreLon = 0.0
                            , centreLat = 0.0
                            }
            in
            ( { model | ipInfo = ipInfo }
            , Cmd.batch
                [ MapPortsController.createMap mapInfoWithLocation

                --, MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                ]
            )

        IpInfoAcknowledged _ ->
            ( model, Cmd.none )

        GpxRequested ->
            ( model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( { model | filename = Just (File.name file) }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            let
                gpxTrack =
                    parseGPXPoints content

                trackTree =
                    treeFromList gpxTrack
            in
            case trackTree of
                Just aTree ->
                    let
                        newTrack =
                            { trackTree = aTree
                            , currentPosition = 0
                            , renderDepth = 10
                            , referenceLonLat =
                                List.head gpxTrack
                                    |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)
                            }

                        modelWithTrack =
                            { model
                                | track = Just newTrack
                                , viewThirdPersonContext =
                                    Just <|
                                        ViewThirdPerson.initialiseView
                                            0
                                            newTrack.trackTree
                                            model.contentArea
                                , viewMapContext = Just ViewMap.initialiseContext
                                , scene = SceneBuilder.render3dView newTrack
                            }

                        ( finalModel, cmd ) =
                            modelWithTrack
                                |> ToolsProforma.refreshAllTools
                    in
                    ( finalModel, Actions.updateAllDisplays newTrack )

                Nothing ->
                    ( model, Cmd.none )

        SetRenderDepth depth ->
            case model.track of
                Just track ->
                    let
                        newTrack =
                            { track | renderDepth = depth }

                        newModel =
                            { model | track = Just track }
                    in
                    ( newModel, Actions.updateAllDisplays newTrack )

                Nothing ->
                    ( model, Cmd.none )

        --Delegate wrapped OAuthmessages. Be bowled over if this works first time. Or fiftieth.
        --Maybe look after to see if there is yet a token. Easy way to know.
        OAuthMessage authMsg ->
            let
                ( newAuthData, authCmd ) =
                    StravaAuth.update authMsg model.stravaAuthentication

                isToken =
                    getStravaToken newAuthData
            in
            ( { model | stravaAuthentication = newAuthData }
            , Cmd.map OAuthMessage authCmd
            )

        SetCurrentPosition pos ->
            -- Slider moves pointer and recentres view.
            case model.track of
                Just track ->
                    let
                        newTrack =
                            { track | currentPosition = pos }

                        newModel =
                            { model
                                | track = Just newTrack
                                , scene = SceneBuilder.render3dView newTrack
                            }
                    in
                    ( newModel, Actions.setCurrentPosition pos track )

                Nothing ->
                    ( model, Cmd.none )

        SetViewMode viewMode ->
            case model.track of
                Just track ->
                    let
                        newModel =
                            { model | viewMode = viewMode }
                    in
                    ( newModel, Actions.updateAllDisplays track )

                Nothing ->
                    ( model, Cmd.none )

        ImageMessage imageMsg ->
            case model.track of
                Just track ->
                    let
                        oldContext =
                            model.viewThirdPersonContext

                        ( newContext, actions ) =
                            case model.viewThirdPersonContext of
                                Just third ->
                                    let
                                        ( new, act ) =
                                            ViewThirdPerson.update imageMsg track third
                                    in
                                    ( Just new, act )

                                Nothing ->
                                    ( Nothing, [] )

                        newModel =
                            { model | viewThirdPersonContext = newContext }
                    in
                    ( newModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        StorageMessage json ->
            ( model, Cmd.none )

        SplitLeftDockRightEdge m ->
            ( { model | leftDockRightEdge = SplitPane.update m model.leftDockRightEdge }
                |> adjustSpaceForContent
            , MapPortsController.refreshMap
            )

        SplitLeftDockInternal m ->
            ( { model | leftDockInternal = SplitPane.update m model.leftDockInternal }
                |> adjustSpaceForContent
            , MapPortsController.refreshMap
            )

        SplitRightDockLeftEdge m ->
            ( { model | rightDockLeftEdge = SplitPane.update m model.rightDockLeftEdge }
                |> adjustSpaceForContent
            , MapPortsController.refreshMap
            )

        SplitRightDockInternal m ->
            ( { model | rightDockInternal = SplitPane.update m model.rightDockInternal }
                |> adjustSpaceForContent
            , MapPortsController.refreshMap
            )

        SplitBottomDockTopEdge m ->
            ( { model | bottomDockTopEdge = SplitPane.update m model.bottomDockTopEdge }
                |> adjustSpaceForContent
            , MapPortsController.refreshMap
            )

        Resize width height ->
            ( { model | windowSize = ( toFloat width, toFloat height ) }
                |> adjustSpaceForContent
            , MapPortsController.refreshMap
            )

        GotWindowSize result ->
            case result of
                Ok info ->
                    ( { model
                        | windowSize =
                            ( info.viewport.width
                            , info.viewport.height
                            )
                      }
                        |> adjustSpaceForContent
                    , MapPortsController.refreshMap
                    )

                Err error ->
                    ( model, Cmd.none )

        ToolsMsg toolMsg ->
            let
                ( newModel, cmds ) =
                    ToolsProforma.update toolMsg ToolsMsg model
            in
            ( newModel, Cmd.none )


adjustSpaceForContent : Model -> Model
adjustSpaceForContent model =
    let
        availableWidthFraction =
            (1.0 - SplitPane.getPosition model.leftDockRightEdge)
                * SplitPane.getPosition model.rightDockLeftEdge

        availableHeightFraction =
            SplitPane.getPosition model.bottomDockTopEdge

        ( reservedWidth, reservedHeight ) =
            -- This by experiment, not ideal.
            ( 50, 130 )

        ( availableWidthPixels, availableHeightPixels ) =
            ( Tuple.first model.windowSize * availableWidthFraction - reservedWidth
            , Tuple.second model.windowSize * availableHeightFraction - reservedHeight
            )
    in
    { model
        | contentArea =
            ( Pixels.pixels <| round availableWidthPixels
            , Pixels.pixels <| round availableHeightPixels
            )
    }


view : Model -> Browser.Document Msg
view model =
    { title = "GPXmagic Labs V3 concepts"
    , body =
        [ layout
            (Background.color FlatColors.ChinesePalette.peace
                :: commonLayoutStyles
            )
          <|
            column [ width fill, height fill ]
                [ topLoadingBar model
                , html <|
                    div
                        [ style "width" "100%"
                        , style "height" "100%"
                        ]
                        [ SplitPane.view
                            leftDockConfig
                            (leftDockView model)
                            (notTheLeftDockView model)
                            model.leftDockRightEdge
                        ]
                ]
        ]
    }


leftDockConfig : ViewConfig Msg
leftDockConfig =
    createViewConfig
        { toMsg = SplitLeftDockRightEdge
        , customSplitter = Nothing
        }


rightDockConfig : ViewConfig Msg
rightDockConfig =
    createViewConfig
        { toMsg = SplitRightDockLeftEdge
        , customSplitter = Nothing
        }


bottomDockConfig : ViewConfig Msg
bottomDockConfig =
    createViewConfig
        { toMsg = SplitBottomDockTopEdge
        , customSplitter = Nothing
        }


leftDockInternalConfig : ViewConfig Msg
leftDockInternalConfig =
    createViewConfig
        { toMsg = SplitLeftDockInternal
        , customSplitter = Nothing
        }


rightDockInternalConfig : ViewConfig Msg
rightDockInternalConfig =
    createViewConfig
        { toMsg = SplitRightDockInternal
        , customSplitter = Nothing
        }


leftDockView : Model -> Html Msg
leftDockView model =
    SplitPane.view
        leftDockInternalConfig
        (upperLeftDockView model)
        (lowerLeftDockView model)
        model.leftDockInternal


upperLeftDockView : Model -> Html Msg
upperLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsProforma.toolsForDock ToolsProforma.DockUpperLeft ToolsMsg model


lowerLeftDockView : Model -> Html Msg
lowerLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsProforma.toolsForDock ToolsProforma.DockLowerLeft ToolsMsg model


rightDockView : Model -> Html Msg
rightDockView model =
    SplitPane.view
        rightDockInternalConfig
        (upperRightDockView model)
        (lowerRightDockView model)
        model.rightDockInternal


upperRightDockView : Model -> Html Msg
upperRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsProforma.toolsForDock ToolsProforma.DockUpperRight ToolsMsg model


lowerRightDockView : Model -> Html Msg
lowerRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsProforma.toolsForDock ToolsProforma.DockLowerRight ToolsMsg model


bottomDockView : Model -> Html Msg
bottomDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsProforma.toolsForDock ToolsProforma.DockBottom ToolsMsg model


notTheLeftDockView : Model -> Html Msg
notTheLeftDockView model =
    SplitPane.view
        rightDockConfig
        (centralAreaView model)
        (rightDockView model)
        model.rightDockLeftEdge


centralAreaView : Model -> Html Msg
centralAreaView model =
    SplitPane.view
        bottomDockConfig
        (viewPaneArea model)
        (bottomDockView model)
        model.bottomDockTopEdge


viewPaneArea : Model -> Html Msg
viewPaneArea model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        contentArea model


topLoadingBar model =
    let
        loadGpxButton =
            button
                [ padding 5
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                { onPress = Just GpxRequested
                , label = text "Load GPX file"
                }
    in
    row
        (commonLayoutStyles
            ++ [ spacing 20
               , padding 10
               , width fill
               , Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
               , Border.color FlatColors.ChinesePalette.twinkleBlue
               ]
        )
        [ loadGpxButton
        ]


viewModeChoices : Model -> Element Msg
viewModeChoices model =
    let
        fullOptionList =
            --[ Input.option ViewThird <| text "Third person"
            --, Input.option ViewMap <| text "Map"
            --]
            [ Input.optionWith ViewThird <| radioButton "Third person"
            , Input.optionWith ViewMap <| radioButton "Map"
            ]
    in
    Input.radioRow
        [ spacing 5
        , padding 5
        ]
        { onChange = SetViewMode
        , selected = Just ViewThird
        , label = Input.labelHidden "Choose view"
        , options = fullOptionList
        }


contentArea : Model -> Element Msg
contentArea model =
    let
        slider trackLength =
            Input.slider
                ViewPureStyles.wideSliderStyles
                { onChange = round >> SetCurrentPosition
                , value =
                    case model.track of
                        Just track ->
                            toFloat track.currentPosition

                        Nothing ->
                            0.0
                , label = Input.labelHidden "Current position slider"
                , min = 0
                , max = toFloat <| trackLength - 1
                , step = Just 1
                , thumb = sliderThumb
                }
    in
    -- NOTE that the Map DIV must be constructed once only, or the map gets upset.
    column
        [ width fill, alignTop, padding 10, centerX ]
        [ column
            [ width fill
            , alignTop
            , centerX
            ]
            [ viewModeChoices model
            , conditionallyVisible (model.viewMode /= ViewMap) <|
                case ( model.viewThirdPersonContext, model.track ) of
                    ( Just context, Just track ) ->
                        ViewThirdPerson.view
                            context
                            track
                            model.scene
                            ImageMessage

                    _ ->
                        none
            , conditionallyVisible (model.viewMode == ViewMap) <|
                ViewMap.view model MapPortsMessage
            ]
        , case model.track of
            Just track ->
                el [ centerX ] <| slider <| 1 + skipCount track.trackTree

            Nothing ->
                none
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
        , MapPortsController.mapResponses (MapPortsMessage << MapPortsController.MapPortMessage)
        , LocalStorage.storageResponses StorageMessage
        , Sub.map SplitLeftDockRightEdge <| SplitPane.subscriptions model.leftDockRightEdge
        , Sub.map SplitLeftDockInternal <| SplitPane.subscriptions model.leftDockInternal
        , Sub.map SplitRightDockLeftEdge <| SplitPane.subscriptions model.rightDockLeftEdge
        , Sub.map SplitRightDockInternal <| SplitPane.subscriptions model.rightDockInternal
        , Sub.map SplitBottomDockTopEdge <| SplitPane.subscriptions model.bottomDockTopEdge
        , Browser.Events.onResize (\w h -> Resize w h)
        ]
