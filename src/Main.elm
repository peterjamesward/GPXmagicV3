module Main exposing (Model, Msg, main)

import Actions exposing (ToolAction(..))
import Angle
import Browser exposing (application)
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import CommonToolStyles
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
import FlatColors.FlatUIPalette
import GeoCodeDecoders exposing (IpInfo)
import GpxParser
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as D
import Json.Encode as E
import LandUseDataOSM
import LandUseDataTypes
import Length
import List.Extra
import LocalStorage
import MapPortController
import MapTypes exposing (MapState(..))
import Markdown
import MyIP
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (OAuthMsg(..))
import PaneContext
import PaneLayoutManager exposing (Msg(..))
import Pixels exposing (Pixels)
import PreviewData exposing (PreviewData, PreviewShape(..))
import Quantity exposing (Quantity)
import SceneBuilderMap
import SplitPane.SplitPane as SplitPane exposing (..)
import StravaAuth exposing (getStravaToken)
import SvgPathExtractor
import SystemSettings exposing (SystemSettings)
import Task
import Time
import ToolTip exposing (localisedTooltip, myTooltip, tooltip)
import Tools.BendSmoother
import Tools.BezierSplines
import Tools.CentroidAverage
import Tools.CurveFormer
import Tools.DeletePoints as DeletePoints
import Tools.DirectionChanges
import Tools.DisplaySettings
import Tools.Graph
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Interpolate
import Tools.MapMatchingRouter
import Tools.MapMatchingRouterOptions
import Tools.MoveAndStretch
import Tools.MoveScaleRotate
import Tools.NamedSegment
import Tools.NamedSegmentOptions exposing (CreateMode(..), NamedSegment)
import Tools.Nudge
import Tools.OneClickQuickFix
import Tools.OutAndBack
import Tools.ProfileSmooth
import Tools.RGTOptions
import Tools.Simplify
import Tools.SmartSmoother
import Tools.SplitAndJoin
import Tools.StartFinish
import Tools.Straightener
import Tools.StravaDataLoad
import Tools.StravaTools
import Tools.Timestamp
import Tools.TrackInfoBox
import Tools.Tracks
import Tools.TracksOptions as Tracks
import ToolsController exposing (encodeColour)
import TrackLoaded exposing (TrackLoaded, indexLeaves)
import Url exposing (Url)
import UtilsForViews exposing (uiColourHexString)
import ViewMap
import ViewPureStyles exposing (..)
import WriteGPX


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | TryRemoteLoad
    | GpxFromUrl (Result Http.Error String)
    | ToggleLoadOptionMenu
    | ToggleRGTOptions
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | StorageMessage E.Value
    | SplitLeftDockRightEdge SplitPane.Msg
    | SplitRightDockLeftEdge SplitPane.Msg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsController.ToolMsg
    | DismissModalMessage
    | PaneMsg PaneLayoutManager.Msg
    | ToggleToolPopup
    | SetColourTheme SystemSettings.ColourTheme
    | Language I18NOptions.Location
    | ToggleLanguageEditor
    | RestoreDefaultToolLayout
    | WriteGpxFile
    | FilenameChange String
    | TimeToUpdateMemory
    | OneClickMsg Tools.OneClickQuickFix.Msg
    | FetchElevationsFromMap String
    | ReplaceTrackOnMapAfterStyleChange
    | SvgMsg SvgPathExtractor.Msg
    | FlythroughTick Time.Posix
    | HideInfoPopup
    | ReceivedLandUseData (Result Http.Error LandUseDataTypes.OSMLandUseData)
    | I18NMsg I18N.Msg
    | BackgroundClick Mouse.Event
    | DisplayWelcome
    | RGTOptions Tools.RGTOptions.Msg
    | ProfilePaint
    | ToggleImperial
    | MatchingRoute (Result Http.Error Tools.MapMatchingRouterOptions.Matchings)
    | NoOp


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    , ipInfo : Maybe IpInfo
    , stravaAuthentication : O.Model
    , loadOptionsMenuOpen : Bool
    , svgFileOptions : SvgPathExtractor.Options
    , rgtOptionsVisible : Bool
    , loadFromUrl : Maybe Url

    -- State machine for map synchronisation
    , mapState : MapState

    -- Track stuff
    , activeTrack : Maybe (TrackLoaded Msg)

    -- Visuals (scenes now in PaneLayoutManager)
    , previews : Dict String PreviewData
    , flythroughRunning : Bool
    , needsRendering : Bool
    , mapPointsDraggable : Bool

    -- Layout stuff
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
    , modalMessage : Maybe String
    , paneLayoutOptions : PaneContext.PaneLayoutOptions
    , infoText : Maybe ( String, String )
    , welcomeDisplayed : Bool

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , rightDockLeftEdge : SplitPane.State

    -- Tools
    , toolOptions : ToolsController.Options Msg
    , isPopupOpen : Bool
    , systemSettings : SystemSettings.SystemSettings
    , languageEditorOpen : Bool
    , languageEditor : I18NOptions.Options
    , rgtOptions : Tools.RGTOptions.Options
    }


encodeSplitValues : Model -> E.Value
encodeSplitValues model =
    E.object
        [ ( "left", E.float <| getPosition model.leftDockRightEdge )
        , ( "right", E.float <| getPosition model.rightDockLeftEdge )
        ]


type alias SplitDecode =
    { left : Int
    , right : Int
    }


decodeSplitValues : E.Value -> Model -> Model
decodeSplitValues values model =
    let
        decoder =
            D.map2 SplitDecode
                (D.field "left" D.int)
                (D.field "right" D.int)

        decoded =
            D.decodeValue decoder values
    in
    case decoded of
        Ok data ->
            let
                ( width, _ ) =
                    ( truncate <| Tuple.first model.windowSize
                    , truncate <| Tuple.second model.windowSize
                    )
            in
            { model
                | leftDockRightEdge =
                    SplitPane.init Horizontal
                        |> configureSplitter (SplitPane.px data.left <| Just ( 20, width // 3 ))
                , rightDockLeftEdge =
                    SplitPane.init Horizontal
                        |> configureSplitter (SplitPane.px data.right <| Just ( 2 * width // 3, width - 20 ))
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

        remoteUrl =
            origin.query
                |> Maybe.map (String.split "=")
                |> Maybe.andThen (List.Extra.getAt 1)
                |> Maybe.andThen Url.percentDecode
                |> Maybe.andThen Url.fromString
    in
    ( { time = Time.millisToPosix 0
      , zone = Time.utc
      , ipInfo = Nothing
      , stravaAuthentication = authData
      , loadOptionsMenuOpen = False
      , svgFileOptions = SvgPathExtractor.defaultOptions
      , rgtOptionsVisible = False
      , loadFromUrl = remoteUrl
      , mapState = MapDivNeeded
      , activeTrack = Nothing
      , mapPointsDraggable = False
      , previews = Dict.empty
      , needsRendering = False
      , flythroughRunning = False
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , modalMessage = Nothing
      , paneLayoutOptions = PaneLayoutManager.defaultOptions
      , infoText = Nothing
      , welcomeDisplayed = False
      , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px 200 <| Just ( 20, 300 ))
      , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px (800 - 200) <| Just ( 600, 990 ))
      , toolOptions = ToolsController.defaultOptions
      , isPopupOpen = False
      , systemSettings = SystemSettings.default
      , languageEditorOpen = False
      , languageEditor = I18N.defaultOptions
      , rgtOptions = Tools.RGTOptions.defaults
      }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , Task.attempt GotWindowSize Dom.getViewport
        , LocalStorage.storageGetItem "splits"
        , LocalStorage.storageGetItem "tools"
        , LocalStorage.storageGetItem "panes"
        , LocalStorage.storageGetItem "measure"
        , LocalStorage.storageGetItem "darkTheme"
        , LocalStorage.storageGetItem "visuals"
        , LocalStorage.storageGetItem "docks"
        , LocalStorage.storageGetItem "location"
        , LocalStorage.fetchMemoryUsage
        , LocalStorage.storageGetItem "welcome"
        , Delay.after 100 DisplayWelcome
        ]
    )


render : Model -> Model
render model =
    -- This is or should be the one place where rendering for 3D (and similar) happens.
    -- Map is different: it's imperative by nature, and we don't need to retain the json.
    if model.needsRendering then
        let
            paneLayout =
                -- This is all the DOM changes, WebGL, SVG.
                --TODO: Should `previews` sit with 3D scene, which is in PaneLayout.
                PaneLayoutManager.render
                    model.toolOptions
                    model.paneLayoutOptions
                    (Tuple.first model.contentArea)
                    model.toolOptions.tracksOptions
                    model.previews
        in
        { model
            | paneLayoutOptions = paneLayout
            , needsRendering = False
        }

    else
        model


updateActiveTrack : TrackLoaded Msg -> Model -> Model
updateActiveTrack newTrack model =
    -- Simplifies transition to multiple tracks.
    let
        toolsOptions =
            model.toolOptions

        ( useTrack, newOptions ) =
            case model.activeTrack of
                Just activeTrack ->
                    Tuple.mapFirst Just <|
                        Tools.Tracks.updateActiveTrack
                            activeTrack
                            newTrack
                            model.toolOptions.tracksOptions

                Nothing ->
                    ( Nothing, model.toolOptions.tracksOptions )

        newToolsOptions =
            { toolsOptions | tracksOptions = newOptions }
    in
    { model
        | activeTrack = useTrack
        , toolOptions = newToolsOptions
        , needsRendering = True
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        processGpxContent content =
            let
                gpxSegments =
                    GpxParser.parseSegments content

                trackName =
                    case GpxParser.parseTrackName content of
                        Just gotTrackName ->
                            gotTrackName

                        Nothing ->
                            I18N.localisedString model.systemSettings.location "main" "unnamed"
            in
            case TrackLoaded.trackFromSegments trackName gpxSegments of
                Just track ->
                    let
                        modelWithTrack =
                            adoptTrackInModel track model
                    in
                    ( modelWithTrack
                    , Cmd.batch
                        [ showTrackOnMapCentered modelWithTrack.toolOptions.tracksOptions
                        , LandUseDataOSM.requestLandUseData ReceivedLandUseData track
                        , LocalStorage.sessionClear
                        , Delay.after 1000 ProfilePaint -- wait for container to paint.
                        ]
                    )

                Nothing ->
                    ( { model | modalMessage = Just "noload" }
                    , Cmd.none
                    )
    in
    case msg of
        ToggleImperial ->
            let
                settings =
                    model.systemSettings

                newSettings =
                    { settings | imperial = not settings.imperial }
            in
            ( { model | systemSettings = newSettings }
            , LocalStorage.storageSetItem "measure" <| E.bool newSettings.imperial
            )

        DisplayWelcome ->
            let
                -- Try loading remote data now, after map may have initialised
                loadCmd =
                    Delay.after 500 TryRemoteLoad
            in
            if model.welcomeDisplayed then
                ( model, loadCmd )

            else
                ( { model | infoText = Just ( "main", "welcome" ) }
                , Cmd.batch
                    [ LocalStorage.storageSetItem "welcome" (E.bool True)
                    , loadCmd
                    ]
                )

        RGTOptions options ->
            ( { model | rgtOptions = Tools.RGTOptions.update options model.rgtOptions }
            , Cmd.none
            )

        ProfilePaint ->
            -- This does a deferred paint of profiles after a track is loaded
            -- as the needed DIVs are not reliably there on loading the app.
            case model.activeTrack of
                Just track ->
                    ( model
                    , PaneLayoutManager.paintProfileCharts
                        model.paneLayoutOptions
                        model.systemSettings
                        track
                        model.previews
                    )

                Nothing ->
                    ( model, Cmd.none )

        BackgroundClick _ ->
            let
                paneStuff =
                    model.paneLayoutOptions

                toolsStuff =
                    ToolsController.clearPopups model.toolOptions
            in
            ( { model
                | infoText = Nothing
                , isPopupOpen = False
                , loadOptionsMenuOpen = False
                , paneLayoutOptions = { paneStuff | popupVisible = False }
                , toolOptions = toolsStuff
              }
            , Cmd.none
            )

        Language location ->
            let
                settings =
                    model.systemSettings

                newSettings =
                    { settings | location = location }
            in
            ( { model | systemSettings = newSettings }
            , Cmd.batch
                [ LocalStorage.storageSetItem "location" <| E.string location.country.code
                , if Dict.isEmpty location.textDictionary then
                    I18N.requestDictionary I18NMsg location.country.code

                  else
                    Cmd.none
                ]
            )

        ToggleRGTOptions ->
            ( { model | rgtOptionsVisible = not model.rgtOptionsVisible }
            , Cmd.none
            )

        ToggleLanguageEditor ->
            ( { model | languageEditorOpen = not model.languageEditorOpen }
            , Cmd.none
            )

        I18NMsg i18n ->
            let
                ( newLocation, newOptions, cmds ) =
                    I18N.update i18n I18NMsg ( model.systemSettings.location, model.languageEditor )

                settings =
                    model.systemSettings

                newSettings =
                    { settings | location = newLocation }
            in
            ( { model
                | systemSettings = newSettings
                , languageEditor = newOptions
              }
            , cmds
            )

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
            , MapPortController.createMap
                ViewMap.defaultStyleUrl
                mapInfoWithLocation
                model.contentArea
            )

        TryRemoteLoad ->
            ( model
            , case model.loadFromUrl of
                Nothing ->
                    Cmd.none

                Just url ->
                    Http.get
                        { url = Url.toString url
                        , expect = Http.expectString GpxFromUrl
                        }
            )

        GpxFromUrl result ->
            case result of
                Ok content ->
                    let
                        ( newModel, cmds ) =
                            processGpxContent content

                        newPaneLayout =
                            PaneLayoutManager.forceMapView newModel.paneLayoutOptions
                    in
                    ( { newModel | paneLayoutOptions = newPaneLayout }
                    , cmds
                    )

                Err _ ->
                    ( model, Cmd.none )

        GpxRequested ->
            ( { model | modalMessage = Just "askgpx" }
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( { model | modalMessage = Just "loading" }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            processGpxContent content

        --Delegate wrapped OAuthmessages.
        --For v3, we're copying the state into the Tool, which is not ideal
        --but most of the tools have no need to know about this.
        OAuthMessage authMsg ->
            let
                ( newAuthData, authCmd ) =
                    StravaAuth.update authMsg model.stravaAuthentication

                isToken =
                    getStravaToken newAuthData

                ( withSharedToken, _ ) =
                    case isToken of
                        Just token ->
                            update
                                (ToolsMsg <|
                                    ToolsController.ToolStravaMsg <|
                                        Tools.StravaTools.ConnectionInfo token
                                )
                                model

                        Nothing ->
                            ( model, Cmd.none )
            in
            ( { withSharedToken | stravaAuthentication = newAuthData }
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

                Err _ ->
                    ( model, Cmd.none )

        ToolsMsg toolMsg ->
            let
                ( newToolOptions, actions ) =
                    -- Some of the actions update the model, some issue commands.
                    --TODO: Deprecate the Actions concept, JFDI.
                    --This may introduce some unwieldy function type signatures.
                    ToolsController.update toolMsg
                        model.activeTrack
                        ToolsMsg
                        model.toolOptions

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
                        model.activeTrack
                        model.toolOptions.tracksOptions.graph
                        model.contentArea
                        model.paneLayoutOptions
                        model.previews

                newModel =
                    { model | paneLayoutOptions = newOptions }
                        |> performActionsOnModel actions
            in
            ( newModel
            , performActionCommands actions newModel
            )

        ToggleToolPopup ->
            ( { model | isPopupOpen = not model.isPopupOpen }, Cmd.none )

        SetColourTheme theme ->
            let
                settings =
                    model.systemSettings

                newSettings =
                    { settings | colourTheme = theme }

                newModel =
                    { model | systemSettings = newSettings }

                actions =
                    [ StoreLocally "darkTheme" <| CommonToolStyles.encodeTheme theme ]
            in
            ( newModel
            , performActionCommands actions newModel
            )

        NoOp ->
            ( model, Cmd.none )

        RestoreDefaultToolLayout ->
            let
                newModel =
                    { model | toolOptions = ToolsController.defaultOptions }
            in
            ( newModel
            , Cmd.none
            )

        WriteGpxFile ->
            let
                outputFilename =
                    case model.activeTrack of
                        Just track ->
                            track.trackName
                                ++ (if not (String.endsWith ".GPX" (String.toUpper track.trackName)) then
                                        ".gpx"

                                    else
                                        ""
                                   )

                        Nothing ->
                            "NOFILENAME"
            in
            case model.activeTrack of
                Just track ->
                    ( model
                    , Download.string outputFilename "text/gpx" <|
                        WriteGPX.writeGPX
                            track.trackName
                            model.rgtOptions
                            track
                    )

                Nothing ->
                    ( { model | modalMessage = Just "nowrite" }
                    , Cmd.none
                    )

        FilenameChange newName ->
            case model.activeTrack of
                Just track ->
                    ( updateActiveTrack { track | trackName = newName } model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        TimeToUpdateMemory ->
            ( model, LocalStorage.fetchMemoryUsage )

        OneClickMsg oneClickMsg ->
            let
                actions =
                    -- Some of the actions update the model, some issue commands.
                    Tools.OneClickQuickFix.update oneClickMsg model.activeTrack

                modelAfterActions =
                    performActionsOnModel actions model
            in
            ( modelAfterActions
            , performActionCommands actions modelAfterActions
            )

        FetchElevationsFromMap trackName ->
            -- We have added the full track so that we can then ask
            -- the map for elevation data. Let's do that.
            ( model, MapPortController.requestElevations trackName )

        ReplaceTrackOnMapAfterStyleChange ->
            -- We have added the full track so that we can then ask
            -- the map for elevation data. Let's do that.
            let
                actions =
                    [ TrackHasChanged ]

                newModel =
                    performActionsOnModel actions model
            in
            ( newModel, performActionCommands actions newModel )

        ToggleLoadOptionMenu ->
            ( { model | loadOptionsMenuOpen = not model.loadOptionsMenuOpen }
            , Cmd.none
            )

        SvgMsg svgMsg ->
            let
                ( newOptions, actions ) =
                    SvgPathExtractor.update
                        svgMsg
                        model.svgFileOptions
                        SvgMsg

                newModel =
                    { model | svgFileOptions = newOptions }
                        |> performActionsOnModel actions
            in
            ( newModel
            , performActionCommands actions newModel
            )

        FlythroughTick posix ->
            -- Like a tool message, just isn't.
            case model.activeTrack of
                Just track ->
                    let
                        ( updatedToolOptions, actions ) =
                            ToolsController.flythroughTick model.toolOptions posix track

                        newModel =
                            { model | toolOptions = updatedToolOptions }
                                |> performActionsOnModel actions
                    in
                    ( newModel, performActionCommands actions newModel )

                Nothing ->
                    ( model, Cmd.none )

        HideInfoPopup ->
            ( { model | infoText = Nothing }, Cmd.none )

        ReceivedLandUseData results ->
            case model.activeTrack of
                Just track ->
                    let
                        ( landUse, cmds ) =
                            LandUseDataOSM.processLandUseData results track

                        newTrack =
                            { track
                                | landUseData = landUse
                            }
                    in
                    ( updateActiveTrack newTrack model, cmds )

                Nothing ->
                    ( model, Cmd.none )

        MatchingRoute result ->
            let
                toolOptions =
                    model.toolOptions

                options =
                    toolOptions.routingOptions

                ( newOptions, newTrack ) =
                    Tools.MapMatchingRouter.trackFromDrawnRoute result options

                newToolOptions =
                    { toolOptions | routingOptions = newOptions }
            in
            case newTrack of
                Just track ->
                    let
                        newModel =
                            adoptTrackInModel track { model | toolOptions = newToolOptions }
                    in
                    ( newModel
                    , Cmd.batch
                        [ MapPortController.resetMapAfterDrawing
                        , MapPortController.addAllTracksToMap newModel.toolOptions.tracksOptions
                        , LandUseDataOSM.requestLandUseData ReceivedLandUseData track
                        , Delay.after 1000 <| FetchElevationsFromMap track.trackName -- async to allow map to quiesce.
                        , Delay.after 1000 ProfilePaint -- async, seems to help
                        ]
                    )

                Nothing ->
                    ( { model | toolOptions = newToolOptions }
                    , Cmd.none
                    )


adoptTrackInModel : TrackLoaded Msg -> Model -> Model
adoptTrackInModel track model =
    --If this is not the first track, we must adjust its reference point.
    --That may be inefficient but we can absorb the cost at load time.
    --If not, we (I) will have to change it.
    let
        toolOptions =
            model.toolOptions

        tracksOptions =
            toolOptions.tracksOptions

        newTracksOptions =
            --WARN: Maybe unwise.
            if ToolsController.isToolOpen ToolsController.ToolTracks toolOptions.tools then
                -- Subsequent track only adds if tool is open
                Tools.Tracks.addTrack track tracksOptions

            else if model.activeTrack == Nothing then
                -- First track always gets added.
                Tools.Tracks.addTrack track tracksOptions

            else
                -- New track replaces current
                let
                    ( _, unloadedOptions ) =
                        Tools.Tracks.unloadActiveTrack tracksOptions
                in
                Tools.Tracks.addTrack track unloadedOptions

        newToolOptions =
            { toolOptions
                | tracksOptions = newTracksOptions
            }

        modelWithTrack =
            { model
                | activeTrack = Tools.Tracks.getActiveTrack newTracksOptions
                , paneLayoutOptions =
                    PaneLayoutManager.initialise
                        track
                        model.paneLayoutOptions
                , modalMessage = Nothing
                , previews = Dict.empty
                , toolOptions = newToolOptions
            }

        actions =
            [ TrackHasChanged
            , MapRefresh
            ]
    in
    performActionsOnModel actions modelWithTrack


allocateSpaceForDocksAndContent : Int -> Int -> Model -> Model
allocateSpaceForDocksAndContent newWidth newHeight model =
    let
        ( startWidth, _ ) =
            model.windowSize

        currentLeftSplit =
            truncate <| getPosition model.leftDockRightEdge

        currentRightSplit =
            -- Note that this measurement is from the left window edge,
            -- but we seek to preserve the width of the dock her.
            truncate startWidth - (truncate <| getPosition model.rightDockLeftEdge)
    in
    { model
        | windowSize = ( toFloat newWidth, toFloat newHeight )
        , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter
                    (SplitPane.px currentLeftSplit <|
                        Just ( 20, newWidth // 3 )
                    )
        , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter
                    (SplitPane.px (newWidth - currentRightSplit) <|
                        Just ( 2 * newWidth // 3, newWidth - 20 )
                    )
    }
        |> adjustSpaceForContent


adjustSpaceForContent : Model -> Model
adjustSpaceForContent model =
    let
        ( _, height ) =
            ( Tuple.first model.windowSize
            , Tuple.second model.windowSize
            )

        availableWidthPixels =
            SplitPane.getPosition model.rightDockLeftEdge
                - SplitPane.getPosition model.leftDockRightEdge
                - reservedWidth

        ( reservedWidth, reservedHeight ) =
            -- This by experiment, not ideal.
            ( 20, 160 )
    in
    { model
        | contentArea =
            ( Pixels.pixels <| round availableWidthPixels
            , Pixels.pixels <| round (height - reservedHeight)
            )
    }


composeTitle : Model -> String
composeTitle model =
    case model.activeTrack of
        Nothing ->
            "GPXmagic"

        Just _ ->
            "GPXmagic - " ++ bestTrackName model


bestTrackName : Model -> String
bestTrackName model =
    case model.activeTrack of
        Nothing ->
            I18N.localisedString model.systemSettings.location "main" "notrack"

        Just track ->
            track.trackName


view : Model -> Browser.Document Msg
view model =
    { title = composeTitle model
    , body =
        [ layout
            ([ Background.color (CommonToolStyles.themeBackground model.systemSettings.colourTheme)
             , Font.color (CommonToolStyles.themeForeground model.systemSettings.colourTheme)
             , inFront <|
                case model.modalMessage of
                    Just message ->
                        showModalMessage
                            model.systemSettings.location
                            (Pixels.inPixels <| Tuple.first model.contentArea)
                            (I18N.localisedString model.systemSettings.location "main" message)
                            DismissModalMessage

                    Nothing ->
                        none
             , inFront <| infoTextPopup model.systemSettings.location model.infoText
             , inFront <|
                if model.languageEditorOpen then
                    I18N.editor I18NMsg model.systemSettings.location model.languageEditor

                else
                    none
             ]
                --:: (htmlAttribute <| Mouse.onClick BackgroundClick)
                ++ commonLayoutStyles
            )
          <|
            column [ width fill, height fill ]
                [ topLoadingBar model
                , html <|
                    div
                        [ style "width" "100%", style "height" "100%" ]
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


rightDockView : Model -> Html Msg
rightDockView model =
    upperRightDockView model


notTheRightDockView : Model -> Html Msg
notTheRightDockView model =
    SplitPane.view
        leftDockConfig
        (leftDockView model)
        (centralAreaView model)
        model.leftDockRightEdge


leftDockView : Model -> Html Msg
leftDockView model =
    upperLeftDockView model


upperLeftDockView : Model -> Html Msg
upperLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            model.systemSettings
            ToolsController.DockUpperLeft
            ToolsMsg
            model.activeTrack
            model.toolOptions


upperRightDockView : Model -> Html Msg
upperRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            model.systemSettings
            ToolsController.DockUpperRight
            ToolsMsg
            model.activeTrack
            model.toolOptions


centralAreaView : Model -> Html Msg
centralAreaView model =
    viewPaneArea model


viewPaneArea : Model -> Html Msg
viewPaneArea model =
    layoutWith { options = [ noStaticStyleSheet ] }
        ((htmlAttribute <| Mouse.onClick BackgroundClick) :: commonLayoutStyles)
    <|
        PaneLayoutManager.viewPanes
            model.systemSettings
            PaneMsg
            model.activeTrack
            model.toolOptions.tracksOptions
            model.toolOptions.displaySettings
            model.contentArea
            model.paneLayoutOptions
            model.toolOptions.flythroughSettings.flythrough
            model.previews


topLoadingBar : Model -> Element Msg
topLoadingBar model =
    let
        localHelper : String -> Element msg
        localHelper =
            text << I18N.localisedString model.systemSettings.location "main"

        moreOptionsButton =
            button
                [ padding 5
                , Border.color FlatColors.FlatUIPalette.peterRiver
                , Border.width 2
                , tooltip below (localisedTooltip model.systemSettings.location "main" "import")
                , inFront <|
                    if model.loadOptionsMenuOpen then
                        column
                            [ moveRight 30
                            , Background.color FlatColors.ChinesePalette.antiFlashWhite
                            , htmlAttribute (style "z-index" "20")
                            , padding 5
                            , spacing 5
                            , Border.color FlatColors.ChinesePalette.peace
                            , Border.rounded 4
                            , Border.width 2
                            ]
                            [ SvgPathExtractor.view SvgMsg model.ipInfo

                            -- Can't have "open" URL field, as server must have CORS support.
                            --, loadFromUrl
                            ]

                    else
                        none
                , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ToggleLoadOptionMenu)
                ]
                { onPress = Just ToggleLoadOptionMenu
                , label = useIconWithSize 12 FeatherIcons.moreHorizontal
                }

        loadGpxButton =
            button
                [ padding 5
                , Border.color FlatColors.FlatUIPalette.peterRiver
                , Border.width 2
                ]
                { onPress = Just GpxRequested
                , label = localHelper "loadgpx"
                }

        saveButton =
            row [ spacing 0, padding 0 ]
                [ button
                    [ padding 5
                    , Background.color FlatColors.AussiePalette.juneBud
                    , Border.color FlatColors.FlatUIPalette.peterRiver
                    , Font.color (CommonToolStyles.themeForeground SystemSettings.LightTheme)
                    , Border.width 2
                    ]
                    { onPress = Just WriteGpxFile
                    , label = localHelper "savegpx"
                    }
                , button
                    [ padding 5
                    , Background.color FlatColors.AussiePalette.juneBud
                    , Border.color FlatColors.FlatUIPalette.peterRiver
                    , Font.color (CommonToolStyles.themeForeground SystemSettings.LightTheme)
                    , Border.width 2
                    , tooltip below (localisedTooltip model.systemSettings.location "main" "saveOptions")
                    , inFront <|
                        if model.rgtOptionsVisible then
                            el
                                [ moveDown 24
                                , Border.color FlatColors.FlatUIPalette.peterRiver
                                , Border.width 2
                                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                                , htmlAttribute (style "z-index" "20")
                                , htmlAttribute <|
                                    Mouse.onWithOptions
                                        "click"
                                        stopProp
                                        (always NoOp)
                                ]
                            <|
                                Tools.RGTOptions.view
                                    model.systemSettings.location
                                    model.rgtOptions
                                    RGTOptions

                        else
                            none
                    ]
                    { onPress = Just ToggleRGTOptions
                    , label = useIconWithSize 14 FeatherIcons.list
                    }
                ]
    in
    wrappedRow
        (commonLayoutStyles
            ++ [ spacing 20
               , padding 10
               , width fill
               , htmlAttribute <| Mouse.onClick BackgroundClick
               ]
        )
        [ globalOptions model
        , loadGpxButton
        , moreOptionsButton
        , case model.activeTrack of
            Just track ->
                Input.text
                    [ padding 5
                    , onEnter WriteGpxFile
                    , width <| minimum 200 <| fill
                    , Background.color FlatColors.FlatUIPalette.silver
                    ]
                    { text = track.trackName
                    , onChange = FilenameChange
                    , placeholder = Nothing
                    , label = Input.labelHidden "filename"
                    }

            Nothing ->
                none
        , saveButton
        , row [ alignRight, spacing 5 ]
            [ Tools.OneClickQuickFix.oneClickQuickFixButton model.systemSettings.location OneClickMsg model.activeTrack
            , StravaAuth.stravaButton model.stravaAuthentication OAuthMessage
            , buyMeACoffeeButton
            ]
        ]


buyMeACoffeeButton =
    newTabLink
        [ alignRight
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always NoOp)
        ]
        { url = "https://www.buymeacoffee.com/Peterward"
        , label =
            image [ height (Element.px 30), width (Element.px 130) ]
                { src = "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                , description = "Buy Me A Coffee"
                }
        }


infoTextPopup :
    I18NOptions.Location
    -> Maybe ( String, String )
    -> Element Msg
infoTextPopup location maybeSomething =
    case maybeSomething of
        Just ( tool, tag ) ->
            let
                close =
                    Input.button [ Font.color rgtPurple, alignRight ]
                        { onPress = Just HideInfoPopup
                        , label = useIconWithSize 20 FeatherIcons.x
                        }
            in
            column
                [ Background.color FlatColors.ChinesePalette.antiFlashWhite
                , padding 10
                , centerY
                , centerX
                , width <| Element.px 400
                , Border.color rgtPurple
                , Border.width 4
                , Border.rounded 10
                ]
                [ close
                , paragraph []
                    [ html <| Markdown.toHtml [] <| I18N.localisedString location tool tag ]
                ]

        Nothing ->
            none


globalOptions : Model -> Element Msg
globalOptions model =
    el
        [ inFront <|
            column
                [ moveDown 26
                , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always NoOp)
                , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always NoOp)
                , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always NoOp)
                , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always NoOp)
                , htmlAttribute (style "z-index" "20")
                ]
                [ showOptionsMenu model
                ]
        ]
    <|
        Input.button
            [ Font.color FlatColors.ChinesePalette.antiFlashWhite
            , Background.color rgtPurple
            , padding 2
            , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ToggleToolPopup)
            ]
            { onPress = Just <| ToggleToolPopup
            , label = useIcon FeatherIcons.settings
            }


showOptionsMenu model =
    let
        colourBlock theme =
            Input.button
                [ Background.color <| CommonToolStyles.themeBackground theme
                , width fill
                , height <| Element.px 20
                ]
                { label = none
                , onPress = Just <| SetColourTheme theme
                }

        chooseLanguage : I18NOptions.Location -> Element Msg
        chooseLanguage location =
            Input.button
                [ width fill
                , Font.size 40
                , tooltip below (myTooltip location.country.name)
                ]
                { label = el [ centerX ] <| text location.country.flag
                , onPress = Just <| Language location
                }

        imperialToggleMenuEntry location =
            Input.button [ alignRight ]
                { onPress = Just ToggleImperial
                , label =
                    if model.systemSettings.imperial then
                        I18N.text location "main" "metric"

                    else
                        I18N.text location "main" "imperial"
                }
    in
    if model.isPopupOpen then
        let
            languageEditor =
                Input.button
                    subtleToolStyles
                    { label = text "Show/Hide language file editor"
                    , onPress = Just ToggleLanguageEditor
                    }
        in
        column (spacing 4 :: subtleToolStyles)
            [ row (alignRight :: width fill :: subtleToolStyles)
                [ colourBlock SystemSettings.LightTheme
                , colourBlock SystemSettings.DarkTheme
                ]
            , el (alignRight :: width fill :: subtleToolStyles) <|
                Input.button [ alignRight ]
                    { onPress = Just <| RestoreDefaultToolLayout
                    , label = I18N.text model.systemSettings.location "main" "default"
                    }
            , el (alignRight :: width fill :: subtleToolStyles) <|
                imperialToggleMenuEntry model.systemSettings.location
            , row [ spaceEvenly, width fill ] <|
                List.map chooseLanguage I18N.availableI18N
            , languageEditor
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
        , Sub.map SplitRightDockLeftEdge <| SplitPane.subscriptions model.rightDockLeftEdge
        , Browser.Events.onResize (\w h -> Resize w h)
        , if model.flythroughRunning then
            Time.every 100 FlythroughTick

          else
            Sub.none
        ]


performActionsOnModel : List (ToolAction Msg) -> Model -> Model
performActionsOnModel actions model =
    let
        adjustReference =
            -- Some edits require us to re-align the shared common point.
            case model.toolOptions.tracksOptions.commonReferenceGPX of
                Just centre ->
                    TrackLoaded.changeReferencePoint centre

                Nothing ->
                    identity

        performAction : ToolAction Msg -> Model -> Model
        performAction action foldedModel =
            case ( action, foldedModel.activeTrack ) of
                ( UnloadActiveTrack _, Just _ ) ->
                    let
                        toolOptions =
                            model.toolOptions

                        tracksOptions =
                            toolOptions.tracksOptions

                        ( newTrack, newOptions ) =
                            Tools.Tracks.unloadActiveTrack tracksOptions

                        newToolOptions =
                            { toolOptions | tracksOptions = newOptions }
                    in
                    { foldedModel
                        | activeTrack = newTrack
                        , needsRendering = True
                        , toolOptions = newToolOptions
                    }

                ( SetActiveTrack trackIndex, _ ) ->
                    let
                        toolOptions =
                            model.toolOptions

                        tracksOptions =
                            toolOptions.tracksOptions

                        ( newTrack, newOptions ) =
                            Tools.Tracks.setTrack trackIndex tracksOptions

                        newToolOptions =
                            { toolOptions | tracksOptions = newOptions }
                    in
                    { foldedModel
                        | activeTrack = newTrack
                        , needsRendering = True
                        , toolOptions = newToolOptions
                    }

                ( ProfileClick container x, Just track ) ->
                    -- This must be handled with the right context, to prevent
                    -- sideways scroll being interpreted as a click.
                    let
                        newOrangeIndex =
                            PaneLayoutManager.profileViewHandlesClick
                                container
                                trackDistance
                                model.paneLayoutOptions
                                track

                        trackDistance =
                            if model.systemSettings.imperial then
                                Length.miles x

                            else
                                Length.kilometers x

                        newTrack =
                            case newOrangeIndex of
                                Just newOrange ->
                                    { track | currentPosition = newOrange }

                                Nothing ->
                                    track
                    in
                    updateActiveTrack newTrack foldedModel

                ( MakeMapPointsDraggable flag, _ ) ->
                    { foldedModel | mapPointsDraggable = flag }

                ( ReRender, Just _ ) ->
                    { foldedModel | needsRendering = True }

                ( DisplayInfo tool text, _ ) ->
                    { foldedModel
                        | infoText =
                            case foldedModel.infoText of
                                Just ( isTool, isText ) ->
                                    if tool == isTool && text == isText then
                                        Nothing

                                    else
                                        Just ( tool, text )

                                Nothing ->
                                    Just ( tool, text )
                    }

                ( SetCurrent position, Just track ) ->
                    updateActiveTrack { track | currentPosition = position } foldedModel

                ( SetCurrentFromMapClick position, Just track ) ->
                    updateActiveTrack { track | currentPosition = position } foldedModel

                ( ShowPreview previewData, Just _ ) ->
                    -- Put preview into the scene.
                    -- After some thought, it is sensible to collect the preview data
                    -- since it's handy, as the alternative is another complex case
                    -- statement in ToolController.
                    { foldedModel
                        | previews =
                            Dict.insert previewData.tag previewData foldedModel.previews
                        , needsRendering = True
                    }

                ( HidePreview tag, Just _ ) ->
                    { foldedModel
                        | previews =
                            Dict.remove tag foldedModel.previews
                        , needsRendering = True
                    }

                ( DelayMessage _ _, Just _ ) ->
                    foldedModel

                ( WithUndo undoAction, Just track ) ->
                    -- Finally, we can do this only once.
                    updateActiveTrack
                        (TrackLoaded.addToUndoStack undoAction track)
                        foldedModel

                ( BendSmootherApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.BendSmoother.applyUsingOptions options track)
                        foldedModel

                ( DeletePointOrPoints fromStart fromEnd, Just track ) ->
                    updateActiveTrack
                        (DeletePoints.delete fromStart fromEnd track)
                        foldedModel

                ( BezierApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.BezierSplines.applyUsingOptions options track)
                        foldedModel

                ( CentroidAverageApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.CentroidAverage.applyUsingOptions options track)
                        foldedModel

                ( SmartSmootherApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.SmartSmoother.applyUsingOptions options track)
                        foldedModel

                ( CurveFormerApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.CurveFormer.applyUsingOptions options track)
                        foldedModel

                ( NudgeApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.Nudge.applyUsingOptions options track)
                        foldedModel

                ( UpdateNamedSegments segments, Just track ) ->
                    updateActiveTrack
                        { track | namedSegments = segments }
                        foldedModel

                ( PasteStravaSegment options, Just track ) ->
                    -- This is like Nudge in that the affected area is given
                    -- by the tool, not by the range markers.
                    let
                        ( newTree, _, ( entry, exit ) ) =
                            -- Need the extra returns for the segment.
                            Tools.StravaTools.paste options track

                        namedSegment =
                            case Tools.StravaTools.segmentName options of
                                Just name ->
                                    Just
                                        { name = name
                                        , startDistance = distanceFromIndex entry track.trackTree
                                        , endDistance = distanceFromIndex exit track.trackTree
                                        , createMode = ManualSegment
                                        , startOk = True
                                        , endOk = True
                                        }

                                Nothing ->
                                    Nothing

                        newSegments =
                            case namedSegment of
                                Just isSegment ->
                                    Tools.NamedSegment.addSegment isSegment track

                                Nothing ->
                                    track.namedSegments

                        newTrack =
                            TrackLoaded.useTreeWithRepositionedMarkers newTree track
                    in
                    updateActiveTrack
                        { newTrack | namedSegments = newSegments }
                        foldedModel

                ( ClearStravaSegmentData, _ ) ->
                    let
                        toolOptions =
                            foldedModel.toolOptions

                        newStravaSettings =
                            Tools.StravaTools.clearSegmentData toolOptions.stravaSettings
                    in
                    { foldedModel
                        | toolOptions =
                            { toolOptions
                                | stravaSettings = newStravaSettings
                            }
                    }

                ( Autofix indices, Just track ) ->
                    { foldedModel
                        | activeTrack =
                            Just <|
                                Tools.BendSmoother.applyAutofix
                                    model.toolOptions.bendSmootherOptions
                                    indices
                                    track
                        , needsRendering = True
                    }

                ( OutAndBackApplyWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.OutAndBack.apply options track)
                        foldedModel

                ( ApplySimplify, Just track ) ->
                    updateActiveTrack
                        (Tools.Simplify.apply foldedModel.toolOptions.simplifySettings track)
                        foldedModel

                ( MoveAndStretchWithOptions settings, Just track ) ->
                    updateActiveTrack
                        (Tools.MoveAndStretch.apply settings track)
                        foldedModel

                ( OneClickQuickFix, Just track ) ->
                    updateActiveTrack
                        (Tools.OneClickQuickFix.apply track)
                        foldedModel

                ( ApplyInterpolateWithOptions options, Just track ) ->
                    let
                        newTree =
                            Tools.Interpolate.apply options track

                        newTrack =
                            TrackLoaded.useTreeWithRepositionedMarkers newTree track
                    in
                    updateActiveTrack newTrack foldedModel

                ( Straighten, Just track ) ->
                    updateActiveTrack
                        (Tools.Straightener.apply model.toolOptions.straightenOptions track)
                        foldedModel

                ( ApplySmoothProfile options, Just track ) ->
                    updateActiveTrack
                        (Tools.ProfileSmooth.apply options track)
                        foldedModel

                ( AdjustTimes options, Just track ) ->
                    updateActiveTrack
                        (Tools.Timestamp.applyTimeShift options track)
                        foldedModel

                ( TimeDoubling, Just track ) ->
                    updateActiveTrack
                        (Tools.Timestamp.applyDoubling track)
                        foldedModel

                ( UsePhysicsModel, Just track ) ->
                    updateActiveTrack
                        (Tools.Timestamp.applyPhysics model.toolOptions.timestampOptions track)
                        foldedModel

                ( SetTimeTicks ticks, Just track ) ->
                    updateActiveTrack
                        (Tools.Timestamp.applyTicks ticks track)
                        foldedModel

                ( CloseLoopWithOptions options, Just track ) ->
                    updateActiveTrack
                        (Tools.StartFinish.applyCloseLoop options track)
                        foldedModel

                ( ReverseTrack, Just track ) ->
                    updateActiveTrack
                        (Tools.StartFinish.applyReverse track)
                        foldedModel

                ( MoveStartPoint newStart, Just track ) ->
                    updateActiveTrack
                        (Tools.StartFinish.applyMoveStart newStart track)
                        foldedModel

                ( AddRiderPens, Just track ) ->
                    updateActiveTrack
                        (Tools.StartFinish.addPens track)
                        foldedModel

                ( ApplyRotateAndScale options, Just track ) ->
                    updateActiveTrack
                        (adjustReference <| Tools.MoveScaleRotate.applyRotateAndScale options track)
                        foldedModel

                ( ApplyRecentre coords, Just track ) ->
                    updateActiveTrack
                        (adjustReference <| Tools.MoveScaleRotate.applyRecentre coords track)
                        foldedModel

                ( ApplyMapElevations elevations, Just track ) ->
                    updateActiveTrack
                        (Tools.MoveScaleRotate.applyMapElevations elevations track)
                        foldedModel

                ( ApplyLandUseAltitudes altitudes, Just track ) ->
                    -- Using mapbox elevations to set altitude of OSM places.
                    updateActiveTrack
                        (LandUseDataOSM.applyAltitudes altitudes track)
                        foldedModel

                ( PointMovedOnMap startLon startLat endLon endLat, Just track ) ->
                    let
                        startGpx =
                            { longitude = Direction2d.fromAngle <| Angle.degrees startLon
                            , latitude = Angle.degrees startLat
                            , altitude = Quantity.zero
                            , timestamp = Nothing
                            }

                        index =
                            DomainModel.nearestToLonLat
                                startGpx
                                track.currentPosition
                                track.trackTree
                                track.referenceLonLat
                                track.leafIndex

                        positionBeforeDrag =
                            gpxPointFromIndex index track.trackTree

                        endGpx =
                            { longitude = Direction2d.fromAngle <| Angle.degrees endLon
                            , latitude = Angle.degrees endLat
                            , altitude = positionBeforeDrag.altitude
                            , timestamp = Nothing
                            }

                        newTree =
                            DomainModel.updatePointByIndexInSitu
                                index
                                endGpx
                                track.referenceLonLat
                                track.trackTree

                        withUndo =
                            TrackLoaded.addToUndoStack action track
                    in
                    updateActiveTrack
                        { withUndo | trackTree = newTree }
                        foldedModel

                ( SaveLastMapClick lon lat, Just track ) ->
                    updateActiveTrack
                        { track | lastMapClick = ( lon, lat ) }
                        foldedModel

                ( TrackFromSvg svgContent, _ ) ->
                    let
                        newTrack =
                            SvgPathExtractor.trackFromSvg
                                model.svgFileOptions
                                svgContent
                    in
                    case newTrack of
                        Just track ->
                            adoptTrackInModel track foldedModel

                        Nothing ->
                            { foldedModel | modalMessage = Just "nosvg" }

                ( ParseAndAppend gpxContent, Just track ) ->
                    let
                        newTree =
                            Tools.SplitAndJoin.parseAndAppend gpxContent track

                        newTrack =
                            track |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    updateActiveTrack
                        newTrack
                        foldedModel

                {-
                   ( StartRoutePlanning, Just track ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           graphOptions =
                               toolOptions.graphOptions

                           newPaneLayout =
                               PaneLayoutManager.forceRouteView foldedModel.paneLayoutOptions

                           ( newGraphOptions, newTree ) =
                               Tools.Graph.enterRoutePlanningMode graphOptions track

                           newToolOptions =
                               { toolOptions | graphOptions = newGraphOptions }

                           newTrack =
                               { track | trackTree = newTree }
                       in
                       updateActiveTrack
                           newTrack
                           { foldedModel | paneLayoutOptions = newPaneLayout }
                -}
                {-
                   ( ExitRoutePlanning, Just _ ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           newPaneLayout =
                               PaneLayoutManager.exitRouteView foldedModel.paneLayoutOptions

                           newToolOptions =
                               -- Hack here.
                               { toolOptions
                                   | tools =
                                       List.map
                                           (ToolsController.setToolState ToolsController.ToolGraph ToolsController.Contracted)
                                           toolOptions.tools
                               }
                       in
                       { foldedModel
                           | toolOptions = newToolOptions
                           , paneLayoutOptions = newPaneLayout
                       }
                -}
                {-
                   ( AddTraversal edge, Just _ ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           graphOptions =
                               toolOptions.graphOptions

                           newGraphOptions =
                               Tools.Graph.addTraversal edge graphOptions

                           newToolOptions =
                               { toolOptions | graphOptions = newGraphOptions }
                       in
                       { foldedModel | toolOptions = newToolOptions }
                -}
                {-
                   ( AddSelfLoop node, Just _ ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           graphOptions =
                               toolOptions.graphOptions

                           newGraphOptions =
                               Tools.Graph.addSelfLoop node graphOptions

                           newToolOptions =
                               { toolOptions | graphOptions = newGraphOptions }
                       in
                       { foldedModel | toolOptions = newToolOptions }
                -}
                {-
                   ( DeleteEdge edge, Just _ ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           graphOptions =
                               toolOptions.graphOptions

                           newToolOptions =
                               { toolOptions | graphOptions = Tools.Graph.deleteEdge edge graphOptions }
                       in
                       { foldedModel | toolOptions = newToolOptions }
                -}
                {-
                   ( ChangeActiveTrack edge, Just _ ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           graphOptions =
                               toolOptions.graphOptions

                           newGraphOptions =
                               Tools.Graph.changeActiveTrack edge graphOptions

                           newToolOptions =
                               { toolOptions | graphOptions = newGraphOptions }
                       in
                       { foldedModel
                           | toolOptions = newToolOptions
                           , activeTrack = Tools.Graph.getTrack edge graphOptions
                           , needsRendering = True
                       }
                -}
                {-
                   ( MakeRouteFromGraph, Just _ ) ->
                       let
                           toolOptions =
                               foldedModel.toolOptions

                           graphOptions =
                               toolOptions.graphOptions

                           newGraphOptions =
                               Tools.Graph.makeNewRoute graphOptions

                           newToolOptions =
                               { toolOptions | graphOptions = newGraphOptions }
                       in
                       case Tools.Graph.getTrack 0 newGraphOptions of
                           Just foundNewTrack ->
                               updateActiveTrack
                                   foundNewTrack
                                   { foldedModel | toolOptions = newToolOptions }

                           Nothing ->
                               foldedModel
                -}
                {-
                   ( CombineNearbyPoints, Just track ) ->
                       -- Trickier refactor because of extra graph option state.
                       let
                           oldToolOptions =
                               model.toolOptions

                           oldGraphOptions =
                               oldToolOptions.graphOptions

                           ( newGraphOptions, newTree ) =
                               Tools.Graph.combineNearbyPoints
                                   oldGraphOptions
                                   track

                           newToolOptions =
                               { oldToolOptions | graphOptions = newGraphOptions }

                           pointerReposition =
                               --Let's reposition by distance, not uncommon.
                               DomainModel.preserveDistanceFromStart track.trackTree newTree

                           ( newOrange, newPurple ) =
                               ( pointerReposition track.currentPosition
                               , Maybe.map pointerReposition track.markerPosition
                               )

                           newTrack =
                               { track
                                   | trackTree = newTree
                                   , currentPosition = newOrange
                                   , markerPosition = newPurple
                               }
                       in
                       updateActiveTrack newTrack foldedModel
                -}
                ( LoadGpxFromStrava gpxContent, _ ) ->
                    let
                        ( modelWithNewTrack, _ ) =
                            update (GpxLoaded gpxContent) foldedModel
                    in
                    modelWithNewTrack

                ( TrackFromStravaActivity header streams, _ ) ->
                    let
                        newTrack =
                            Tools.StravaTools.trackFromActivity header streams
                    in
                    case newTrack of
                        Just track ->
                            adoptTrackInModel track foldedModel

                        Nothing ->
                            { foldedModel | modalMessage = Just "nosvg" }

                ( Actions.WidenBend points adjustment, Just track ) ->
                    -- This for one contiguous set of points, i.e. one bend.
                    updateActiveTrack
                        (Tools.DirectionChanges.widenBend points adjustment track)
                        foldedModel

                ( TrackHasChanged, Just _ ) ->
                    -- Must be wary of looping here.
                    -- Purpose is to refresh all tools' options and all presentations.
                    --TODO: Isolate what this is supposed to achieve, and just do it.
                    let
                        ( refreshedToolOptions, secondaryActions ) =
                            ToolsController.refreshOpenTools foldedModel.activeTrack foldedModel.toolOptions

                        innerModelWithNewToolSettings =
                            { foldedModel | toolOptions = refreshedToolOptions }

                        modelAfterSecondaryActions =
                            innerModelWithNewToolSettings |> performActionsOnModel secondaryActions
                    in
                    -- This model should contain all updated previews from open tools.
                    { modelAfterSecondaryActions
                        | needsRendering = True
                    }

                ( PointerChange, Just _ ) ->
                    -- Unlike above, do not repaint map.
                    --TODO: Isolate what this is supposed to achieve, and just do it.
                    let
                        ( refreshedToolOptions, secondaryActions ) =
                            ToolsController.refreshOpenTools foldedModel.activeTrack foldedModel.toolOptions

                        innerModelWithNewToolSettings =
                            { foldedModel | toolOptions = refreshedToolOptions }

                        modelAfterSecondaryActions =
                            innerModelWithNewToolSettings |> performActionsOnModel secondaryActions
                    in
                    -- This model should contain all updated previews from open tools.
                    { modelAfterSecondaryActions
                        | needsRendering = True
                    }

                ( SetMarker maybeMarker, Just track ) ->
                    updateActiveTrack { track | markerPosition = maybeMarker } foldedModel

                ( StartFlythoughTicks, Just _ ) ->
                    { foldedModel | flythroughRunning = True }

                ( StopFlythroughTicks, Just _ ) ->
                    { foldedModel | flythroughRunning = False }

                ( StoredValueRetrieved key value, _ ) ->
                    case key of
                        "welcome" ->
                            if D.decodeValue D.bool value == Ok True then
                                { foldedModel | welcomeDisplayed = True }

                            else
                                foldedModel

                        "location" ->
                            case D.decodeValue D.string value of
                                Ok countryCode ->
                                    let
                                        settings =
                                            foldedModel.systemSettings

                                        newSettings =
                                            { settings | location = I18N.fromCountryCode countryCode }
                                    in
                                    { foldedModel | systemSettings = newSettings }

                                Err _ ->
                                    foldedModel

                        "splits" ->
                            foldedModel |> decodeSplitValues value

                        "tools" ->
                            { foldedModel
                                | toolOptions =
                                    ToolsController.restoreStoredValues foldedModel.toolOptions value
                            }

                        "docks" ->
                            { foldedModel
                                | toolOptions =
                                    ToolsController.restoreDockSettings foldedModel.toolOptions value
                            }

                        "panes" ->
                            { foldedModel
                                | paneLayoutOptions =
                                    PaneLayoutManager.restoreStoredValues foldedModel.paneLayoutOptions value
                            }

                        "measure" ->
                            let
                                newSettings =
                                    ToolsController.restoreMeasure foldedModel.systemSettings value
                            in
                            { foldedModel | systemSettings = newSettings }

                        "darkTheme" ->
                            let
                                isDark =
                                    D.decodeValue D.bool value
                            in
                            case isDark of
                                Ok True ->
                                    let
                                        settings =
                                            foldedModel.systemSettings

                                        newSettings =
                                            { settings | colourTheme = SystemSettings.DarkTheme }
                                    in
                                    { foldedModel | systemSettings = newSettings }

                                _ ->
                                    foldedModel

                        "visuals" ->
                            let
                                toolOptions =
                                    model.toolOptions

                                newToolOptions =
                                    { toolOptions
                                        | displaySettings =
                                            Tools.DisplaySettings.restoreSettings
                                                value
                                                toolOptions.displaySettings
                                    }
                            in
                            { foldedModel | toolOptions = newToolOptions }

                        _ ->
                            foldedModel

                ( HeapStatusUpdate heapStatus, _ ) ->
                    let
                        currentTools =
                            model.toolOptions

                        currentInfo =
                            currentTools.infoOptions

                        newInfo =
                            Tools.TrackInfoBox.updateMemory heapStatus currentInfo

                        newTools =
                            { currentTools | infoOptions = newInfo }
                    in
                    { foldedModel | toolOptions = newTools }

                ( UndoLastAction, Just track ) ->
                    -- Without massive replumbing, I'm making the "graph walker" undo special.
                    -- We'll see how this goes; a better solution may arise.
                    {-
                       let
                           topUndoAction =
                               track.undos |> List.head |> Maybe.map .action
                       in
                       case topUndoAction of
                           Just Actions.MakeRouteFromGraph ->
                               let
                                   toolOptions =
                                       foldedModel.toolOptions
                                   newToolOptions =
                                       { toolOptions | graphOptions = newGraphOptions }
                               in
                               case Tools.Graph.getTrack 0 newGraphOptions of
                                   Just newTrack ->
                                       updateActiveTrack newTrack foldedModel

                                   Nothing ->
                                       foldedModel

                           _ ->
                    -}
                    updateActiveTrack
                        (TrackLoaded.undoLastAction track)
                        foldedModel

                ( RedoUndoneAction, Just track ) ->
                    case track.redos of
                        redo :: moreRedos ->
                            -- More care needed or the repeated edit will flush the Redo stack.
                            --TODO: Suspect this needs rework to match Undo.
                            --Note: we must always repaint everything. Don't be clever.
                            let
                                modelAfterRedo =
                                    performActionsOnModel
                                        [ WithUndo redo.action
                                        , redo.action
                                        ]
                                        model
                            in
                            case modelAfterRedo.activeTrack of
                                Just trackAfterRedo ->
                                    updateActiveTrack
                                        { trackAfterRedo | redos = moreRedos }
                                        modelAfterRedo

                                Nothing ->
                                    -- Not good, but what can we do?
                                    modelAfterRedo

                        _ ->
                            foldedModel

                ( FlushUndo, Just track ) ->
                    { foldedModel
                        | activeTrack =
                            Just
                                { track
                                    | undos = []
                                    , redos = []
                                }
                    }

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
                    case useThisData.shape of
                        PreviewCircle ->
                            MapPortController.showPreview
                                useThisData.tag
                                "circle"
                                (uiColourHexString useThisData.colour)
                                (SceneBuilderMap.renderPreview useThisData)

                        PreviewLine ->
                            MapPortController.showPreview
                                useThisData.tag
                                "line"
                                (uiColourHexString useThisData.colour)
                                (SceneBuilderMap.renderPreview useThisData)

                        _ ->
                            -- No other shapes go to map.
                            Cmd.none

                Nothing ->
                    Cmd.none

        performAction : ToolAction Msg -> Cmd Msg
        performAction action =
            case ( action, model.activeTrack ) of
                ( SetActiveTrack _, _ ) ->
                    performAction TrackHasChanged

                ( UnloadActiveTrack oldTrackName, _ ) ->
                    Cmd.batch
                        [ MapPortController.removeTrackFromMapByName oldTrackName
                        , performAction TrackHasChanged
                        ]

                ( TryRemoteLoadIfGiven, _ ) ->
                    case model.loadFromUrl of
                        Nothing ->
                            Cmd.none

                        Just url ->
                            Http.get
                                { url = Url.toString url
                                , expect = Http.expectString GpxFromUrl
                                }

                ( SetCurrent _, Just track ) ->
                    MapPortController.addMarkersToMap track

                ( SetCurrentFromMapClick _, Just _ ) ->
                    --MapPortController.addMarkersToMap track
                    Cmd.none

                ( MapCenterOnCurrent, Just track ) ->
                    MapPortController.centreMapOnCurrent track

                ( MapRefresh, Just track ) ->
                    -- Lazy, use this to refresh profile as well.
                    Cmd.batch
                        [ MapPortController.refreshMap
                        , PaneLayoutManager.paintProfileCharts
                            model.paneLayoutOptions
                            model.systemSettings
                            track
                            model.previews
                        ]

                ( MapRefresh, Nothing ) ->
                    -- Lazy, use this to refresh profile as well.
                    MapPortController.refreshMap

                ( MakeMapPointsDraggable flag, Just track ) ->
                    MapPortController.toggleDragging flag track

                ( ShowPreview previewData, Just track ) ->
                    -- Add source and layer to map, via Port commands.
                    -- Use preview data from model dictionary, as that could be
                    -- more up to date than this version.
                    Cmd.batch
                        [ showPreviewOnMap previewData.tag
                        , PaneLayoutManager.paintProfileCharts
                            model.paneLayoutOptions
                            model.systemSettings
                            track
                            model.previews
                        ]

                ( HidePreview tag, Just track ) ->
                    Cmd.batch
                        [ MapPortController.hidePreview tag
                        , PaneLayoutManager.paintProfileCharts
                            model.paneLayoutOptions
                            model.systemSettings
                            track
                            model.previews
                        ]

                ( DelayMessage int msg, Just _ ) ->
                    -- This used to "debounce" some clicks.
                    Delay.after int msg

                ( TrackHasChanged, Just track ) ->
                    Cmd.batch
                        [ MapPortController.addAllTracksToMap model.toolOptions.tracksOptions
                        , PaneLayoutManager.paintProfileCharts
                            model.paneLayoutOptions
                            model.systemSettings
                            track
                            model.previews
                        ]

                ( PointerChange, Just track ) ->
                    Cmd.batch <|
                        PaneLayoutManager.paintProfileCharts
                            model.paneLayoutOptions
                            model.systemSettings
                            track
                            model.previews
                            :: MapPortController.addMarkersToMap track
                            :: List.map showPreviewOnMap (Dict.keys model.previews)

                ( SetMarker _, Just track ) ->
                    MapPortController.addMarkersToMap track

                ( StoreLocally key value, _ ) ->
                    LocalStorage.storageSetItem key value

                ( HeapStatusUpdate _, _ ) ->
                    Delay.after 5000 TimeToUpdateMemory

                ( AddFullTrackToMapForElevations, Just track ) ->
                    -- Deliberate pause here seems to allow map to quiesce.
                    Cmd.batch
                        [ MapPortController.addFullTrackToMap track
                        , Delay.after 100 <| FetchElevationsFromMap track.trackName
                        ]

                ( FetchMapElevations, Just track ) ->
                    MapPortController.requestElevations track.trackName

                ( SetMapStyle url, _ ) ->
                    -- Deliberate pause here seems to allow map to quiesce.
                    Cmd.batch
                        [ MapPortController.setMapStyle url
                        , Delay.after 1000 ReplaceTrackOnMapAfterStyleChange
                        ]

                ( SelectSvgFile message, _ ) ->
                    Select.file [ "text/svg" ] message

                ( LoadSvgFile message file, _ ) ->
                    Task.perform message (File.toString file)

                ( TrackFromSvg _, Just track ) ->
                    showTrackOnMapCentered model.toolOptions.tracksOptions

                ( SelectGpxFile message, _ ) ->
                    Select.file [ "text/gpx" ] message

                ( LoadGpxFile message file, _ ) ->
                    Task.perform message (File.toString file)

                ( TrackFromGpx _, Just track ) ->
                    showTrackOnMapCentered model.toolOptions.tracksOptions

                ( LoadGpxFromStrava _, Just track ) ->
                    showTrackOnMapCentered model.toolOptions.tracksOptions

                ( RequestStravaRouteHeader msg routeId token, _ ) ->
                    Tools.StravaDataLoad.requestStravaRouteHeader
                        msg
                        routeId
                        token

                ( RequestStravaRoute msg routeId token, _ ) ->
                    Tools.StravaDataLoad.requestStravaRoute
                        msg
                        routeId
                        token

                ( RequestStravaActivity msg activityId token, _ ) ->
                    Tools.StravaDataLoad.requestStravaActivity
                        msg
                        activityId
                        token

                ( RequestStravaActivityStreams msg activityId token, _ ) ->
                    Tools.StravaDataLoad.requestStravaActivityStreams
                        msg
                        activityId
                        token

                ( RequestStravaSegment msg segmentId token, _ ) ->
                    Tools.StravaDataLoad.requestStravaSegment
                        msg
                        segmentId
                        token

                ( RequestStravaSegmentStreams msg segmentId token, _ ) ->
                    Tools.StravaDataLoad.requestStravaSegmentStreams
                        msg
                        segmentId
                        token

                ( WriteTrackSections sections, Just track ) ->
                    Tools.SplitAndJoin.writeOneSection
                        sections
                        model.toolOptions.splitAndJoinOptions
                        track
                        model.rgtOptions

                ( RenderProfile context, Just track ) ->
                    Cmd.batch
                        [ MapPortController.paintCanvasProfileChart
                            context
                            model.systemSettings
                            track
                            model.previews
                        , MapPortController.paintCanvasGradientChart
                            context
                            model.systemSettings
                            track
                        ]

                ( EnablePlanningOnMap, _ ) ->
                    MapPortController.enablePlanning

                ( GetPointsFromMap, _ ) ->
                    MapPortController.getPoints

                ( FetchMatchingRoute coordinates, _ ) ->
                    Tools.MapMatchingRouter.mapMatchingApi MatchingRoute coordinates

                _ ->
                    Cmd.none
    in
    Cmd.batch <| List.map performAction actions


showTrackOnMapCentered : Tracks.Options msg -> Cmd msg
showTrackOnMapCentered tracks =
    case Tools.Tracks.getActiveTrack tracks of
        Just activeTrack ->
            Cmd.batch
                [ MapPortController.addAllTracksToMap tracks
                , MapPortController.zoomMapToFitTrack activeTrack
                , MapPortController.addMarkersToMap activeTrack
                ]

        Nothing ->
            MapPortController.addAllTracksToMap tracks
