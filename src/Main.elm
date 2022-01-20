module Main exposing (main)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Browser exposing (application)
import Browser.Dom as Dom exposing (getViewport, getViewportOf)
import Browser.Events
import Browser.Navigation exposing (Key)
import Delay
import Dict exposing (Dict)
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
import Json.Decode as D
import Json.Encode as E exposing (string)
import LocalCoords exposing (LocalCoords)
import LocalStorage
import MapPortController
import MyIP
import OAuthPorts as O exposing (randomBytes)
import OAuthTypes as O exposing (OAuthMsg(..))
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import SceneBuilder
import SceneBuilderMap
import SplitPane.SplitPane as SplitPane exposing (..)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import Tools.DeletePoints as DeletePoints
import ToolsController exposing (ToolEntry)
import TrackLoaded exposing (TrackLoaded)
import Url exposing (Url)
import UtilsForViews exposing (colourHexString)
import ViewContext exposing (ViewContext(..), ViewMode(..))
import ViewContextThirdPerson exposing (ThirdPersonContext)
import ViewMap exposing (MapContext)
import ViewPureStyles exposing (commonLayoutStyles, conditionallyVisible, neatToolsBorder, radioButton, sliderThumb)
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
    | MapPortsMessage MapPortController.MapMsg
    | StorageMessage E.Value
    | SplitLeftDockRightEdge SplitPane.Msg
    | SplitLeftDockInternal SplitPane.Msg
    | SplitRightDockLeftEdge SplitPane.Msg
    | SplitRightDockInternal SplitPane.Msg
    | SplitBottomDockTopEdge SplitPane.Msg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsController.ToolMsg


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , ipInfo : Maybe IpInfo
    , stravaAuthentication : O.Model

    -- Track stuff
    , track : Maybe (TrackLoaded Msg)

    -- Visuals
    , scene : List (Entity LocalCoords)
    , viewMode : ViewMode
    , viewThirdPersonContext : Maybe ThirdPersonContext
    , viewMapContext : Maybe MapContext
    , previews : Dict String PreviewData

    -- Layout stuff
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
    , modalMessage : Maybe String

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , leftDockInternal : SplitPane.State
    , rightDockLeftEdge : SplitPane.State
    , rightDockInternal : SplitPane.State
    , bottomDockTopEdge : SplitPane.State

    -- Tools
    , toolOptions : ToolsController.Options
    }


encodeSplitValues : Model -> E.Value
encodeSplitValues model =
    E.object
        [ ( "left", E.float <| getPosition model.leftDockRightEdge )
        , ( "right", E.float <| getPosition model.rightDockLeftEdge )
        , ( "bottom", E.float <| getPosition model.bottomDockTopEdge )
        , ( "internalleft", E.float <| getPosition model.leftDockInternal )
        , ( "internalright", E.float <| getPosition model.rightDockInternal )
        ]


type alias SplitDecode =
    { left : Int
    , right : Int
    , bottom : Int
    , leftInternal : Int
    , rightInternal : Int
    }


decodeSplitValues : E.Value -> Model -> Model
decodeSplitValues values model =
    let
        decoder =
            D.map5 SplitDecode
                (D.field "left" D.int)
                (D.field "right" D.int)
                (D.field "bottom" D.int)
                (D.field "internalleft" D.int)
                (D.field "internalright" D.int)

        decoded =
            D.decodeValue decoder values

        ( width, height ) =
            ( truncate <| Tuple.first model.windowSize
            , truncate <| Tuple.second model.windowSize
            )
    in
    case decoded of
        Ok data ->
            { model
                | leftDockRightEdge =
                    SplitPane.init Horizontal
                        |> configureSplitter (SplitPane.px data.left <| Just ( 20, width // 3 ))
                , leftDockInternal =
                    SplitPane.init Vertical
                        |> configureSplitter (SplitPane.px data.leftInternal <| Just ( 50, height - 75 ))
                , rightDockLeftEdge =
                    SplitPane.init Horizontal
                        |> configureSplitter (SplitPane.px data.right <| Just ( 2 * width // 3, width - 20 ))
                , rightDockInternal =
                    SplitPane.init Vertical
                        |> configureSplitter (SplitPane.px data.rightInternal <| Just ( 50, height - 75 ))
                , bottomDockTopEdge =
                    SplitPane.init Vertical
                        |> configureSplitter (SplitPane.px data.bottom <| Just ( height * 2 // 3, height - 75 ))
            }
                |> adjustSpaceForContent

        Err _ ->
            model


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
      , scene = []
      , previews = Dict.empty
      , viewMode = ViewInfo
      , viewThirdPersonContext = Nothing
      , viewMapContext = Nothing
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , modalMessage = Nothing
      , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px 200 <| Just ( 20, 200 ))
      , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px (500 // 2) <| Just ( 50, 400 ))
      , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px (800 - 200) <| Just ( 600, 990 ))
      , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px (500 // 2) <| Just ( 50, 400 ))
      , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px (500 - 200) <| Just ( 300, 470 ))
      , toolOptions = ToolsController.defaultOptions
      }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , LocalStorage.storageListKeys
        , Task.attempt GotWindowSize Dom.getViewport
        , LocalStorage.storageGetItem "splits"
        , LocalStorage.storageGetItem "tools"
        ]
    )


render : Model -> Model
render model =
    -- This is or should be the one place where rendering for 3D (and similar) happens.
    -- Map is different: it's imperative by nature, and we don't need to retain the json.
    case model.track of
        Just track ->
            let
                renderedTrack =
                    SceneBuilder.render3dView track

                renderedPreviews =
                    SceneBuilder.renderPreviews model.previews
            in
            { model | scene = renderedPreviews ++ renderedTrack }

        Nothing ->
            model


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
                    let
                        actions =
                            MapPortController.update mapMsg track

                        newModel =
                            performActionsOnModel actions model
                    in
                    ( newModel, performActionCommands actions model )

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
                [ MapPortController.createMap mapInfoWithLocation

                --, MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                ]
            )

        IpInfoAcknowledged _ ->
            ( model, Cmd.none )

        GpxRequested ->
            ( { model | modalMessage = Just "Select GPX file" }
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( { model
                | filename = Just (File.name file)
                , modalMessage = Just <| ("Loading " ++ File.name file)
              }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            let
                gpxTrack =
                    parseGPXPoints content

                trackTree =
                    treeFromSourcePoints gpxTrack
            in
            case trackTree of
                Just aTree ->
                    let
                        newTrack : TrackLoaded Msg
                        newTrack =
                            { trackTree = aTree
                            , currentPosition = 0
                            , markerPosition = Nothing
                            , renderDepth = 10
                            , referenceLonLat =
                                List.head gpxTrack
                                    |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)
                            , undos = []
                            , redos = []
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
                                , viewMode =
                                    if model.viewMode == ViewInfo then
                                        ViewThird

                                    else
                                        model.viewMode
                                , modalMessage = Nothing
                            }

                        actions =
                            [ TrackHasChanged ]

                        modelAfterActions =
                            -- e.g. collect previews and render ...
                            performActionsOnModel actions modelWithTrack
                    in
                    ( modelAfterActions
                    , Cmd.batch
                        [ performActionCommands actions modelAfterActions
                        , showTrackOnMapCentered newTrack
                        ]
                    )

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
                    ( newModel, showTrackOnMapCentered newTrack )

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
            -- Slider moves pointer and re-centres view.
            -- The actions will re-render and repaint the map.
            -- TODO: Make this into a Tool
            let
                actions =
                    [ SetCurrent pos, TrackHasChanged, MapCenterOnCurrent ]

                modelAfterActions =
                    performActionsOnModel actions model
            in
            ( modelAfterActions
            , performActionCommands actions modelAfterActions
            )

        SetViewMode viewMode ->
            case model.track of
                Just track ->
                    let
                        newModel =
                            { model | viewMode = viewMode }
                    in
                    ( newModel, showTrackOnMapCentered track )

                Nothing ->
                    ( model, Cmd.none )

        ImageMessage imageMsg ->
            case model.track of
                Just track ->
                    let
                        ( newContext, actions ) =
                            case model.viewThirdPersonContext of
                                Just third ->
                                    let
                                        ( new, act ) =
                                            ViewThirdPerson.update imageMsg ImageMessage track third
                                    in
                                    ( Just new, act )

                                Nothing ->
                                    ( Nothing, [] )

                        newModel =
                            { model | viewThirdPersonContext = newContext }
                                |> performActionsOnModel actions
                    in
                    ( newModel, performActionCommands actions model )

                Nothing ->
                    ( model, Cmd.none )

        StorageMessage json ->
            let
                actions =
                    LocalStorage.processStoragePortMessage json model

                newModel =
                    performActionsOnModel actions model
            in
            ( newModel, performActionCommands actions model )

        SplitLeftDockRightEdge m ->
            let
                newModel =
                    { model | leftDockRightEdge = SplitPane.update m model.leftDockRightEdge }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands [ MapRefresh, StoreSplitConfig ] newModel
            )

        SplitLeftDockInternal m ->
            let
                newModel =
                    { model | leftDockInternal = SplitPane.update m model.leftDockInternal }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands [ MapRefresh, StoreSplitConfig ] newModel
            )

        SplitRightDockLeftEdge m ->
            let
                newModel =
                    { model | rightDockLeftEdge = SplitPane.update m model.rightDockLeftEdge }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands [ MapRefresh, StoreSplitConfig ] newModel
            )

        SplitRightDockInternal m ->
            let
                newModel =
                    { model | rightDockInternal = SplitPane.update m model.rightDockInternal }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands [ MapRefresh, StoreSplitConfig ] newModel
            )

        SplitBottomDockTopEdge m ->
            let
                newModel =
                    { model | bottomDockTopEdge = SplitPane.update m model.bottomDockTopEdge }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands [ MapRefresh, StoreSplitConfig ] newModel
            )

        Resize width height ->
            let
                newModel =
                    { model | windowSize = ( toFloat width, toFloat height ) }
                        |> allocateSpaceForDocksAndContent width height
            in
            ( newModel
            , performActionCommands [ MapRefresh, StoreSplitConfig ] newModel
            )

        GotWindowSize result ->
            case result of
                Ok info ->
                    let
                        newModel =
                            model
                                |> allocateSpaceForDocksAndContent
                                    (truncate info.viewport.width)
                                    (truncate info.viewport.height)
                    in
                    ( newModel
                    , performActionCommands [ MapRefresh ] newModel
                    )

                Err error ->
                    ( model, Cmd.none )

        ToolsMsg toolMsg ->
            let
                ( newToolOptions, actions ) =
                    -- Some of the actions update the model, some issue commands.
                    ToolsController.update toolMsg model.track ToolsMsg model.toolOptions

                newModel =
                    { model | toolOptions = newToolOptions }

                modelAfterActions =
                    performActionsOnModel actions newModel
            in
            ( modelAfterActions
            , performActionCommands actions modelAfterActions
            )


allocateSpaceForDocksAndContent : Int -> Int -> Model -> Model
allocateSpaceForDocksAndContent width height model =
    let
        currentLeftSplit =
            truncate <| getPosition model.leftDockRightEdge

        currentRightSplit =
            truncate <| getPosition model.rightDockLeftEdge

        currentBottomSplit =
            truncate <| getPosition model.bottomDockTopEdge

        currentLeftInternal =
            truncate <| getPosition model.leftDockInternal

        currentRightInternal =
            truncate <| getPosition model.rightDockInternal
    in
    { model
        | windowSize = ( toFloat width, toFloat height )
        , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px currentLeftSplit <| Just ( 20, width // 3 ))
        , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px currentLeftInternal <| Just ( 50, height - 75 ))
        , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px currentRightSplit <| Just ( 2 * width // 3, width - 20 ))
        , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px currentRightInternal <| Just ( 50, height - 75 ))
        , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px currentBottomSplit <| Just ( height * 2 // 3, height - 75 ))
    }
        |> adjustSpaceForContent


adjustSpaceForContent : Model -> Model
adjustSpaceForContent model =
    let
        ( width, height ) =
            ( Tuple.first model.windowSize
            , Tuple.second model.windowSize
            )

        availableWidthPixels =
            SplitPane.getPosition model.rightDockLeftEdge
                - SplitPane.getPosition model.leftDockRightEdge
                - reservedWidth

        availableHeightPixels =
            SplitPane.getPosition model.bottomDockTopEdge
                - reservedHeight

        ( reservedWidth, reservedHeight ) =
            -- This by experiment, not ideal.
            ( 20, 80 )
    in
    { model
        | contentArea =
            ( Pixels.pixels <| round availableWidthPixels
            , Pixels.pixels <| round availableHeightPixels
            )
    }


showModalMessage msg =
    el (centerX :: centerY :: neatToolsBorder) <|
        text msg


view : Model -> Browser.Document Msg
view model =
    { title = "GPXmagic Labs V3 concepts"
    , body =
        [ layout
            (Background.color FlatColors.ChinesePalette.peace
                :: (inFront <|
                        case model.modalMessage of
                            Just msg ->
                                showModalMessage msg

                            Nothing ->
                                none
                   )
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
                            rightDockConfig
                            (notTheRightDockView model)
                            (rightDockView model)
                            model.rightDockLeftEdge
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


rightDockView : Model -> Html Msg
rightDockView model =
    SplitPane.view
        rightDockInternalConfig
        (upperRightDockView model)
        (lowerRightDockView model)
        model.rightDockInternal


notTheRightDockView : Model -> Html Msg
notTheRightDockView model =
    SplitPane.view
        leftDockConfig
        (leftDockView model)
        (centralAreaView model)
        model.leftDockRightEdge


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
        ToolsController.toolsForDock ToolsController.DockUpperLeft ToolsMsg model.track model.toolOptions


lowerLeftDockView : Model -> Html Msg
lowerLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            ToolsController.DockLowerLeft
            ToolsMsg
            model.track
            model.toolOptions


upperRightDockView : Model -> Html Msg
upperRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            ToolsController.DockUpperRight
            ToolsMsg
            model.track
            model.toolOptions


lowerRightDockView : Model -> Html Msg
lowerRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            ToolsController.DockLowerRight
            ToolsMsg
            model.track
            model.toolOptions


bottomDockView : Model -> Html Msg
bottomDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            ToolsController.DockBottom
            ToolsMsg
            model.track
            model.toolOptions


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
        , selected = Just model.viewMode
        , label = Input.labelHidden "Choose view"
        , options = fullOptionList
        }


contentArea : Model -> Element Msg
contentArea model =
    let
        ( w, h ) =
            model.contentArea

        slider trackLength =
            Input.slider
                (ViewPureStyles.wideSliderStylesWithWidth w)
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
        [ width <| Element.px <| Pixels.inPixels w
        , height <| Element.px <| Pixels.inPixels h
        , alignTop
        , centerX
        ]
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
                            model.contentArea
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
        , MapPortController.mapResponses (MapPortsMessage << MapPortController.MapPortMessage)
        , LocalStorage.storageResponses StorageMessage
        , Sub.map SplitLeftDockRightEdge <| SplitPane.subscriptions model.leftDockRightEdge
        , Sub.map SplitLeftDockInternal <| SplitPane.subscriptions model.leftDockInternal
        , Sub.map SplitRightDockLeftEdge <| SplitPane.subscriptions model.rightDockLeftEdge
        , Sub.map SplitRightDockInternal <| SplitPane.subscriptions model.rightDockInternal
        , Sub.map SplitBottomDockTopEdge <| SplitPane.subscriptions model.bottomDockTopEdge
        , Browser.Events.onResize (\w h -> Resize w h)
        ]


performActionsOnModel : List (ToolAction Msg) -> Model -> Model
performActionsOnModel actions model =
    let
        performAction : ToolAction Msg -> Model -> Model
        performAction action foldedModel =
            case ( action, foldedModel.track ) of
                ( SetCurrent position, Just track ) ->
                    let
                        newTrack =
                            { track | currentPosition = position }
                    in
                    { foldedModel | track = Just newTrack }

                ( SetCurrentFromMapClick position, Just track ) ->
                    let
                        newTrack =
                            { track | currentPosition = position }
                    in
                    { foldedModel | track = Just newTrack }

                ( ShowPreview previewData, Just track ) ->
                    -- Put preview into the scene.
                    -- After some thought, it is sensible to collect the preview data
                    -- since it's handy, as the alternative is another complex case
                    -- statement in ToolController.
                    { foldedModel | previews = Dict.insert previewData.tag previewData foldedModel.previews }

                ( HidePreview tag, Just track ) ->
                    { foldedModel | previews = Dict.remove tag foldedModel.previews }

                ( DelayMessage int msg, Just track ) ->
                    foldedModel

                ( DeletePointsBetween fromStart fromEnd, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            DeletePoints.deletePointsBetween fromStart fromEnd track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action fromStart fromEnd oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel | track = Just newTrack }

                ( DeleteSinglePoint fromStart fromEnd, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            DeletePoints.deleteSinglePoint
                                fromStart
                                fromEnd
                                track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action fromStart fromEnd oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel | track = Just newTrack }

                ( TrackHasChanged, Just track ) ->
                    -- Must be wary of looping here.
                    -- Purpose is to refresh all tools' options and all presentations.
                    let
                        ( refreshedToolOptions, secondaryActions ) =
                            ToolsController.refreshOpenTools foldedModel.track foldedModel.toolOptions

                        innerModelWithNewToolSettings =
                            { foldedModel | toolOptions = refreshedToolOptions }

                        modelAfterSecondaryActions =
                            innerModelWithNewToolSettings |> performActionsOnModel secondaryActions
                    in
                    -- This model should contain all updated previews from open tools.
                    modelAfterSecondaryActions

                ( SetMarker maybeMarker, Just track ) ->
                    let
                        updatedTrack =
                            { track | markerPosition = maybeMarker }
                    in
                    { foldedModel | track = Just updatedTrack }

                ( StoredValueRetrieved key value, _ ) ->
                    case key of
                        "splits" ->
                            foldedModel |> decodeSplitValues value

                        "tools" ->
                            { foldedModel
                                | toolOptions =
                                    ToolsController.restoreStoredValues foldedModel.toolOptions value
                            }

                        _ ->
                            foldedModel

                ( UndoLastAction, Just track ) ->
                    { foldedModel | track = Just <| TrackLoaded.undoLastAction track }

                ( RedoUndoneAction, Just track ) ->
                    case track.redos of
                        redo :: moreRedos ->
                            -- More care needed or the repeated edit will flush the Redo stack.
                            let
                                modelAfterRedo =
                                    performActionsOnModel [ redo.action ] model

                            in
                            case modelAfterRedo.track of
                                Just trackAfterRedo ->
                                    let
                                         trackWithCorrectRedoStack =
                                           { trackAfterRedo | redos = moreRedos }
                                    in
                                    { modelAfterRedo | track = Just trackWithCorrectRedoStack }

                                Nothing ->
                                    -- Not good, live with it.
                                    modelAfterRedo

                        _ ->
                            foldedModel

                _ ->
                    foldedModel
    in
    List.foldl performAction model actions
        |> render


performActionCommands : List (ToolAction Msg) -> Model -> Cmd Msg
performActionCommands actions model =
    let
        showPreviewOnMap tag =
            case Dict.get tag model.previews of
                Just useThisData ->
                    MapPortController.showPreview
                        useThisData.tag
                        (case useThisData.shape of
                            PreviewCircle ->
                                "circle"

                            PreviewLine ->
                                "line"
                        )
                        (colourHexString useThisData.colour)
                        (SceneBuilderMap.renderPreview useThisData)

                Nothing ->
                    Cmd.none

        performAction : ToolAction Msg -> Cmd Msg
        performAction action =
            case ( action, model.track ) of
                ( SetCurrent position, Just track ) ->
                    Cmd.batch
                        [ MapPortController.addTrackToMap track
                        , MapPortController.centreMapOnCurrent track
                        , MapPortController.addMarkersToMap track
                        ]

                ( SetCurrentFromMapClick position, Just track ) ->
                    MapPortController.addMarkersToMap track

                ( MapCenterOnCurrent, Just track ) ->
                    MapPortController.centreMapOnCurrent track

                ( MapRefresh, Just track ) ->
                    MapPortController.refreshMap

                ( ShowPreview previewData, Just track ) ->
                    -- Add source and layer to map, via Port commands.
                    -- Use preview data from model dictionary, as that could be
                    -- more up to date than this version.
                    showPreviewOnMap previewData.tag

                ( HidePreview tag, Just track ) ->
                    MapPortController.hidePreview tag

                ( DelayMessage int msg, Just track ) ->
                    -- This used to "debounce" some clicks.
                    Delay.after int msg

                ( TrackHasChanged, Just track ) ->
                    Cmd.batch
                        [ MapPortController.addTrackToMap track
                        , MapPortController.addMarkersToMap track
                        , Cmd.batch <| List.map showPreviewOnMap (Dict.keys model.previews)
                        ]

                ( SetMarker maybeMarker, Just track ) ->
                    MapPortController.addMarkersToMap track

                ( StoreSplitConfig, _ ) ->
                    LocalStorage.storageSetItem "splits" (encodeSplitValues model)

                ( StoreToolsConfig, _ ) ->
                    LocalStorage.storageSetItem "tools" <|
                        ToolsController.encodeToolState model.toolOptions

                _ ->
                    Cmd.none
    in
    Cmd.batch <| List.map performAction actions


showTrackOnMapCentered : TrackLoaded msg -> Cmd msg
showTrackOnMapCentered track =
    Cmd.batch
        -- Must repaint track on so that selective rendering works.
        [ MapPortController.addTrackToMap track
        , MapPortController.centreMapOnCurrent track
        , MapPortController.addMarkersToMap track
        ]
