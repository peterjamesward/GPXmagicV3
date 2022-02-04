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
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import GeoCodeDecoders exposing (IpInfo)
import GpxParser exposing (parseGPXPoints)
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as D
import Json.Encode as E exposing (string)
import LocalCoords exposing (LocalCoords)
import LocalStorage
import MapPortController
import MyIP
import OAuthPorts as O exposing (randomBytes)
import OAuthTypes as O exposing (OAuthMsg(..))
import PaneLayoutManager exposing (Msg(..), ViewMode(..))
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import SceneBuilderMap
import SplitPane.SplitPane as SplitPane exposing (..)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import Tools.BezierSplines
import Tools.DeletePoints as DeletePoints
import ToolsController exposing (ToolEntry)
import TrackLoaded exposing (TrackLoaded)
import Url exposing (Url)
import UtilsForViews exposing (colourHexString)
import ViewPureStyles exposing (..)
import ViewThirdPerson exposing (stopProp)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | StorageMessage E.Value
    | SplitLeftDockRightEdge SplitPane.Msg
    | SplitLeftDockInternal SplitPane.Msg
    | SplitRightDockLeftEdge SplitPane.Msg
    | SplitRightDockInternal SplitPane.Msg
    | SplitBottomDockTopEdge SplitPane.Msg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsController.ToolMsg
    | DismissModalMessage
    | PaneMsg PaneLayoutManager.Msg
    | RepaintMap
    | ToggleToolPopup
    | BackgroundColour Element.Color
    | RestoreDefaultToolLayout
    | SaveJsonData -- for chart testing...
    | NoOp


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , ipInfo : Maybe IpInfo
    , stravaAuthentication : O.Model

    -- Track stuff
    , track : Maybe (TrackLoaded Msg)

    -- Visuals (scenes now in PaneLayoutManager)
    , previews : Dict String PreviewData

    -- Layout stuff
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
    , modalMessage : Maybe String
    , paneLayoutOptions : PaneLayoutManager.Options

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , leftDockInternal : SplitPane.State
    , rightDockLeftEdge : SplitPane.State
    , rightDockInternal : SplitPane.State
    , bottomDockTopEdge : SplitPane.State

    -- Tools
    , toolOptions : ToolsController.Options
    , isPopupOpen : Bool
    , backgroundColour : Element.Color
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
      , previews = Dict.empty
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , modalMessage = Nothing
      , paneLayoutOptions = PaneLayoutManager.defaultOptions
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
      , isPopupOpen = False
      , backgroundColour = FlatColors.AussiePalette.wizardGrey
      }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here

        --, LocalStorage.storageListKeys
        , Task.attempt GotWindowSize Dom.getViewport
        , LocalStorage.storageGetItem "splits"
        , LocalStorage.storageGetItem "tools"
        , LocalStorage.storageGetItem "panes"
        , LocalStorage.storageGetItem "measure"
        ]
    )


render : Model -> Model
render model =
    -- This is or should be the one place where rendering for 3D (and similar) happens.
    -- Map is different: it's imperative by nature, and we don't need to retain the json.
    case model.track of
        Just track ->
            let
                paneLayout =
                    PaneLayoutManager.render model.paneLayoutOptions track model.previews
            in
            { model | paneLayoutOptions = paneLayout }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
            )

        DismissModalMessage ->
            ( { model | modalMessage = Nothing }
            , Cmd.none
            )

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
                                , paneLayoutOptions =
                                    PaneLayoutManager.initialise
                                        newTrack
                                        model.paneLayoutOptions
                                , modalMessage = Nothing
                            }

                        actions =
                            [ TrackHasChanged, MapRefresh ]

                        modelAfterActions =
                            -- e.g. collect previews and render ...
                            performActionsOnModel actions modelWithTrack
                    in
                    ( modelAfterActions
                    , Cmd.batch
                        [ showTrackOnMapCentered newTrack ]
                    )

                Nothing ->
                    ( { model | modalMessage = Just """Sorry, unable to make a track.
Please check the file contains GPX data.""" }
                    , Cmd.none
                    )

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
            , performActionCommands
                [ MapRefresh
                , StoreLocally "splits" (encodeSplitValues model)
                ]
                newModel
            )

        SplitLeftDockInternal m ->
            let
                newModel =
                    { model | leftDockInternal = SplitPane.update m model.leftDockInternal }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands
                [ MapRefresh
                , StoreLocally "splits" (encodeSplitValues model)
                ]
                newModel
            )

        SplitRightDockLeftEdge m ->
            let
                newModel =
                    { model | rightDockLeftEdge = SplitPane.update m model.rightDockLeftEdge }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands
                [ MapRefresh
                , StoreLocally "splits" (encodeSplitValues model)
                ]
                newModel
            )

        SplitRightDockInternal m ->
            let
                newModel =
                    { model | rightDockInternal = SplitPane.update m model.rightDockInternal }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands
                [ MapRefresh
                , StoreLocally "splits" (encodeSplitValues model)
                ]
                newModel
            )

        SplitBottomDockTopEdge m ->
            let
                newModel =
                    { model | bottomDockTopEdge = SplitPane.update m model.bottomDockTopEdge }
                        |> adjustSpaceForContent
            in
            ( newModel
            , performActionCommands
                [ MapRefresh
                , StoreLocally "splits" (encodeSplitValues model)
                ]
                newModel
            )

        Resize width height ->
            let
                newModel =
                    allocateSpaceForDocksAndContent width height model
            in
            ( newModel
            , performActionCommands
                [ MapRefresh
                , StoreLocally "splits" (encodeSplitValues model)
                ]
                newModel
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

        PaneMsg paneMsg ->
            let
                ( newOptions, actions ) =
                    PaneLayoutManager.update
                        paneMsg
                        PaneMsg
                        model.track
                        model.contentArea
                        model.paneLayoutOptions

                newModel =
                    { model | paneLayoutOptions = newOptions }
                        |> performActionsOnModel actions
            in
            ( newModel
            , performActionCommands actions newModel
            )

        RepaintMap ->
            ( model, MapPortController.refreshMap )

        ToggleToolPopup ->
            ( { model | isPopupOpen = not model.isPopupOpen }, Cmd.none )

        BackgroundColour colour ->
            ( { model | backgroundColour = colour }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        RestoreDefaultToolLayout ->
            ( { model | toolOptions = ToolsController.defaultOptions }
            , Cmd.none
            )

        SaveJsonData ->
            let
                content =
                    Maybe.map TrackLoaded.jsonProfileData model.track
            in
            ( model
            , case content of
                Just json ->
                    Download.string "DATA.JSON" "text/json" json

                Nothing ->
                    Cmd.none
            )



--task to write the data


allocateSpaceForDocksAndContent : Int -> Int -> Model -> Model
allocateSpaceForDocksAndContent newWidth newHeight model =
    let
        ( startWidth, startHeight ) =
            model.windowSize

        currentLeftSplit =
            truncate <| getPosition model.leftDockRightEdge

        currentRightSplit =
            -- Note that this measurement is from the left window edge,
            -- but we seek to preserve the width of the dock her.
            truncate startWidth - (truncate <| getPosition model.rightDockLeftEdge)

        currentBottomSplit =
            truncate <| getPosition model.bottomDockTopEdge

        currentLeftInternal =
            truncate <| getPosition model.leftDockInternal

        currentRightInternal =
            truncate <| getPosition model.rightDockInternal
    in
    { model
        | windowSize = ( toFloat newWidth, toFloat newHeight )
        , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter
                    (SplitPane.px currentLeftSplit <|
                        Just ( 20, newWidth // 3 )
                    )
        , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter
                    (SplitPane.px currentLeftInternal <|
                        Just ( 50, newHeight - 75 )
                    )
        , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter
                    (SplitPane.px (newWidth - currentRightSplit) <|
                        Just ( 2 * newWidth // 3, newWidth - 20 )
                    )
        , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter
                    (SplitPane.px currentRightInternal <|
                        Just ( 50, newHeight - 75 )
                    )
        , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter
                    (SplitPane.px currentBottomSplit <|
                        Just ( newHeight * 2 // 3, newHeight - 75 )
                    )
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


view : Model -> Browser.Document Msg
view model =
    { title = "GPXmagic Labs V3 concepts"
    , body =
        [ layout
            (Background.color model.backgroundColour
                :: (inFront <|
                        case model.modalMessage of
                            Just message ->
                                showModalMessage message DismissModalMessage

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
        PaneLayoutManager.viewPanes
            PaneMsg
            model.track
            model.contentArea
            model.paneLayoutOptions


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

        saveButton =
            button
                [ padding 5
                , Background.color FlatColors.AussiePalette.quinceJelly
                ]
                { onPress = Just SaveJsonData
                , label = text "SAVE JSON TEST"
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
        , PaneLayoutManager.paneLayoutMenu PaneMsg model.paneLayoutOptions
        , saveButton
        , globalOptions model
        ]


globalOptions : Model -> Element Msg
globalOptions model =
    el
        [ alignRight
        , inFront <|
            column
                [ alignRight
                , moveDown 26
                , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always NoOp)
                , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always NoOp)
                , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always NoOp)
                , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always NoOp)
                , htmlAttribute (style "z-index" "20")
                , Background.color FlatColors.AussiePalette.coastalBreeze
                ]
                [ showOptionsMenu model
                ]
        ]
    <|
        Input.button
            [ alignRight ]
            { onPress = Just <| ToggleToolPopup
            , label = useIcon FeatherIcons.settings
            }


showOptionsMenu model =
    let
        colourBlock colour =
            Input.button
                [ Background.color colour, width fill, height <| Element.px 20 ]
                { label = none
                , onPress = Just <| BackgroundColour colour
                }
    in
    if model.isPopupOpen then
        column (spacing 4 :: neatToolsBorder)
            [ row (alignRight :: width fill :: neatToolsBorder)
                [ colourBlock FlatColors.AussiePalette.coastalBreeze
                , colourBlock FlatColors.AussiePalette.soaringEagle
                , colourBlock FlatColors.AussiePalette.wizardGrey
                ]
            , el (alignRight :: width fill :: neatToolsBorder) <|
                Input.button [ alignRight ]
                    { onPress = Just <| RestoreDefaultToolLayout
                    , label = text "Restore default layout"
                    }
            , el (alignRight :: width fill :: neatToolsBorder) <|
                ToolsController.imperialToggleMenuEntry ToolsMsg model.toolOptions
            ]

    else
        none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
        , MapPortController.mapResponses (PaneMsg << MapPortsMessage << MapPortController.MapPortMessage)
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
                    { foldedModel
                        | track =
                            Just { track | currentPosition = position }
                    }

                ( SetCurrentFromMapClick position, Just track ) ->
                    { foldedModel
                        | track =
                            Just { track | currentPosition = position }
                    }

                ( ShowPreview previewData, Just track ) ->
                    -- Put preview into the scene.
                    -- After some thought, it is sensible to collect the preview data
                    -- since it's handy, as the alternative is another complex case
                    -- statement in ToolController.
                    { foldedModel
                        | previews =
                            Dict.insert previewData.tag previewData foldedModel.previews
                    }

                ( HidePreview tag, Just track ) ->
                    { foldedModel
                        | previews =
                            Dict.remove tag foldedModel.previews
                    }

                ( RenderProfile, Just track ) ->
                    { foldedModel
                        | paneLayoutOptions =
                            PaneLayoutManager.renderProfile
                                foldedModel.paneLayoutOptions
                                track
                    }

                ( DelayMessage int msg, Just track ) ->
                    foldedModel

                ( DeletePointsBetween fromStart fromEnd, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            DeletePoints.deletePointsBetween fromStart fromEnd track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack
                                    action
                                    fromStart
                                    fromEnd
                                    oldPoints
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
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel | track = Just newTrack }

                ( BezierApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.BezierSplines.applyUsingOptions options track

                        ( fromStart, fromEnd ) =
                            TrackLoaded.getRangeFromMarkers track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel | track = Just newTrack }

                ( TrackHasChanged, Just track ) ->
                    -- Must be wary of looping here.
                    -- Purpose is to refresh all tools' options and all presentations.
                    --TODO: Isolate what this is supposed to achieve, and just do it.
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

                        "panes" ->
                            { foldedModel
                                | paneLayoutOptions =
                                    PaneLayoutManager.restoreStoredValues foldedModel.paneLayoutOptions value
                            }

                        "measure" ->
                            { foldedModel
                                | toolOptions =
                                    ToolsController.restoreMeasure foldedModel.toolOptions value
                            }

                        _ ->
                            foldedModel

                ( HeapStatusUpdate heapStatus, _ ) ->
                    --TODO: Make a tool for these values, but meanwhile...
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
                    MapPortController.addMarkersToMap track

                ( SetCurrentFromMapClick position, Just track ) ->
                    Cmd.none

                --MapPortController.addMarkersToMap track
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

                ( StoreLocally key value, _ ) ->
                    LocalStorage.storageSetItem key value

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
