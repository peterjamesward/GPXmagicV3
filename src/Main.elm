module Main exposing (main)

import Actions exposing (ToolAction(..))
import Angle
import Browser exposing (application)
import Browser.Dom as Dom exposing (getViewport, getViewportOf)
import Browser.Events
import Browser.Navigation exposing (Key)
import Color
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
import GpxParser exposing (parseGPXPoints)
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as D
import Json.Encode as E exposing (string)
import LandUseDataOSM
import LandUseDataTypes
import LocalStorage
import MapPortController
import Markdown
import MyIP
import OAuthPorts as O exposing (randomBytes)
import OAuthTypes as O exposing (OAuthMsg(..))
import PaneLayoutManager exposing (Msg(..), ViewMode(..))
import Pixels exposing (Pixels)
import PreviewData exposing (PreviewData, PreviewShape(..))
import Quantity exposing (Quantity)
import SceneBuilderMap
import SplitPane.SplitPane as SplitPane exposing (..)
import StravaAuth exposing (getStravaToken)
import SvgPathExtractor
import Task
import Time
import ToolTip exposing (myTooltip, tooltip)
import Tools.BendSmoother
import Tools.BezierSplines
import Tools.CentroidAverage
import Tools.CurveFormer
import Tools.DeletePoints as DeletePoints
import Tools.DirectionChanges
import Tools.DisplaySettings
import Tools.Graph
import Tools.GraphOptions exposing (Graph)
import Tools.Interpolate
import Tools.InterpolateOptions
import Tools.MoveAndStretch
import Tools.MoveScaleRotate
import Tools.Nudge
import Tools.OneClickQuickFix
import Tools.OutAndBack
import Tools.ProfileSmooth
import Tools.ProfileSmoothOptions
import Tools.Simplify
import Tools.SplitAndJoin
import Tools.StartFinish
import Tools.StartFinishTypes exposing (Loopiness(..))
import Tools.Straightener
import Tools.StravaDataLoad
import Tools.StravaTools
import Tools.TrackInfoBox
import ToolsController exposing (ToolEntry, encodeColour, encodeToolState)
import TrackLoaded exposing (TrackLoaded)
import Url exposing (Url)
import UtilsForViews exposing (uiColourHexString)
import View3dCommonElements exposing (stopProp)
import ViewMap
import ViewPureStyles exposing (..)
import WriteGPX


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | ToggleLoadOptionMenu
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | StorageMessage E.Value
    | SplitLeftDockRightEdge SplitPane.Msg
    | SplitRightDockLeftEdge SplitPane.Msg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsController.ToolMsg
    | DismissModalMessage
    | PaneMsg PaneLayoutManager.Msg
    | RepaintMap
    | ToggleToolPopup
    | BackgroundColour Element.Color
    | RestoreDefaultToolLayout
    | WriteGpxFile
    | FilenameChange String
    | TimeToUpdateMemory
    | OneClickMsg Tools.OneClickQuickFix.Msg
    | FetchElevationsFromMap
    | ReplaceTrackOnMapAfterStyleChange
    | SvgMsg SvgPathExtractor.Msg
    | FlythroughTick Time.Posix
    | HideInfoPopup
    | ReceivedLandUseData (Result Http.Error LandUseDataTypes.OSMLandUseData)
    | NoOp


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , ipInfo : Maybe IpInfo
    , stravaAuthentication : O.Model
    , loadOptionsMenuOpen : Bool
    , svgFileOptions : SvgPathExtractor.Options

    -- Track stuff
    , track : Maybe (TrackLoaded Msg)

    -- Visuals (scenes now in PaneLayoutManager)
    , previews : Dict String PreviewData
    , flythroughRunning : Bool
    , needsRendering : Bool

    -- Layout stuff
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
    , modalMessage : Maybe String
    , paneLayoutOptions : PaneLayoutManager.Options
    , infoText : Maybe ( String, String )

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , rightDockLeftEdge : SplitPane.State

    -- Tools
    , toolOptions : ToolsController.Options Msg
    , isPopupOpen : Bool
    , backgroundColour : Element.Color
    , infoTextDict : Dict String (Dict String String)
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
    in
    ( { filename = Nothing
      , time = Time.millisToPosix 0
      , zone = Time.utc
      , ipInfo = Nothing
      , stravaAuthentication = authData
      , loadOptionsMenuOpen = False
      , svgFileOptions = SvgPathExtractor.defaultOptions
      , track = Nothing
      , previews = Dict.empty
      , needsRendering = False
      , flythroughRunning = False
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , modalMessage = Nothing
      , paneLayoutOptions = PaneLayoutManager.defaultOptions
      , infoText = Nothing
      , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px 200 <| Just ( 20, 300 ))
      , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px (800 - 200) <| Just ( 600, 990 ))
      , toolOptions = ToolsController.defaultOptions
      , isPopupOpen = False
      , backgroundColour = FlatColors.FlatUIPalette.silver
      , infoTextDict = initTextDictionaries
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
        , LocalStorage.storageGetItem "background"
        , LocalStorage.storageGetItem "visuals"
        , LocalStorage.storageGetItem "docks"
        , LocalStorage.storageGetMemoryUsage
        ]
    )


initTextDictionaries =
    --TODO: Include PaneLayout in this scheme.
    ToolsController.initTextDictionaries


render : Model -> Model
render model =
    -- This is or should be the one place where rendering for 3D (and similar) happens.
    -- Map is different: it's imperative by nature, and we don't need to retain the json.
    case ( model.track, model.needsRendering ) of
        ( Just track, True ) ->
            let
                paneLayout =
                    PaneLayoutManager.render
                        model.toolOptions
                        model.paneLayoutOptions
                        (Tuple.first model.contentArea)
                        track
                        model.previews
            in
            { model
                | paneLayoutOptions = paneLayout
                , needsRendering = False
            }

        _ ->
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
                [ MapPortController.createMap
                    ViewMap.defaultStyleUrl
                    mapInfoWithLocation
                , MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                ]
            )

        IpInfoAcknowledged _ ->
            ( model, Cmd.none )

        GpxRequested ->
            ( { model | modalMessage = Just """Select GPX file.
            
If the File Open dialog does not appear, please reload the page in the browser and try again.""" }
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
                    GpxParser.parseGPXPoints content

                trackTree =
                    treeFromSourcePoints gpxTrack
            in
            case trackTree of
                Just aTree ->
                    let
                        trackName =
                            GpxParser.parseTrackName content
                                |> Maybe.andThen (always model.filename)
                                |> Maybe.withDefault "no track name"

                        newTrack : Maybe (TrackLoaded Msg)
                        newTrack =
                            TrackLoaded.trackFromPoints trackName gpxTrack
                    in
                    case newTrack of
                        Just track ->
                            ( adoptTrackInModel track model
                            , Cmd.batch
                                [ showTrackOnMapCentered track
                                , LandUseDataOSM.requestLandUseData
                                    ReceivedLandUseData
                                    track
                                ]
                            )

                        Nothing ->
                            ( { model | modalMessage = Just "Sorry, unable to load that" }
                            , Cmd.none
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

                Err error ->
                    ( model, Cmd.none )

        ToolsMsg toolMsg ->
            let
                ( newToolOptions, actions ) =
                    -- Some of the actions update the model, some issue commands.
                    ToolsController.update toolMsg
                        model.track
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
                        model.track
                        model.toolOptions.graphOptions.graph
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

        RepaintMap ->
            ( model, MapPortController.refreshMap )

        ToggleToolPopup ->
            ( { model | isPopupOpen = not model.isPopupOpen }, Cmd.none )

        BackgroundColour colour ->
            let
                newModel =
                    { model | backgroundColour = colour }

                actions =
                    [ StoreLocally "background" <| encodeColour colour ]
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

                actions =
                    [ StoreLocally "tools" <| encodeToolState newModel.toolOptions ]
            in
            ( newModel
            , Cmd.none
            )

        WriteGpxFile ->
            let
                outputFilename =
                    case model.filename of
                        Just filename ->
                            filename
                                ++ (if not (String.endsWith ".GPX" (String.toUpper filename)) then
                                        ".gpx"

                                    else
                                        ""
                                   )

                        Nothing ->
                            "NOFILENAME"
            in
            case model.track of
                Just track ->
                    ( model
                    , Download.string outputFilename "text/gpx" <|
                        WriteGPX.writeGPX model.filename track
                    )

                Nothing ->
                    ( { model | modalMessage = Just "Sorry, unable to write the file" }
                    , Cmd.none
                    )

        FilenameChange filename ->
            ( { model | filename = Just filename }
            , Cmd.none
            )

        TimeToUpdateMemory ->
            ( model, LocalStorage.storageGetMemoryUsage )

        OneClickMsg oneClickMsg ->
            let
                actions =
                    -- Some of the actions update the model, some issue commands.
                    Tools.OneClickQuickFix.update oneClickMsg model.track

                modelAfterActions =
                    performActionsOnModel actions model
            in
            ( modelAfterActions
            , performActionCommands actions modelAfterActions
            )

        FetchElevationsFromMap ->
            -- We have added the full track so that we can then ask
            -- the map for elevation data. Let's do that.
            ( model, MapPortController.requestElevations )

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
            case model.track of
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
            case model.track of
                Just track ->
                    let
                        ( landUse, cmds ) =
                            LandUseDataOSM.processLandUseData results track

                        newTrack =
                            { track
                                | landUseData = landUse
                            }
                    in
                    ( { model | track = Just newTrack }, cmds )

                Nothing ->
                    ( model, Cmd.none )


adoptTrackInModel : TrackLoaded Msg -> Model -> Model
adoptTrackInModel track model =
    let
        toolOptions =
            model.toolOptions

        graphOptions =
            toolOptions.graphOptions

        graphFromTrack =
            { graphOptions
                | graph = Tools.Graph.trivialGraph track
                , analyzed = False
            }

        newToolOptions =
            { toolOptions | graphOptions = graphFromTrack }

        modelWithTrack =
            { model
                | track = Just track
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

        modelAfterActions =
            -- e.g. collect previews and render ...
            performActionsOnModel actions modelWithTrack
    in
    modelAfterActions


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
        ( width, height ) =
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


composeTitle model =
    case model.track of
        Nothing ->
            "GPXmagic Labs V3 concepts"

        Just track ->
            "GPXmagic - " ++ bestTrackName model


bestTrackName model =
    case model.track of
        Nothing ->
            "no track"

        Just track ->
            case track.trackName of
                Just trackname ->
                    trackname

                Nothing ->
                    case model.filename of
                        Just filename ->
                            filename

                        Nothing ->
                            "unnamed track"


view : Model -> Browser.Document Msg
view model =
    { title = composeTitle model
    , body =
        [ layout
            (Background.color model.backgroundColour
                :: (inFront <|
                        case model.modalMessage of
                            Just message ->
                                showModalMessage
                                    (Pixels.inPixels <| Tuple.first model.contentArea)
                                    message
                                    DismissModalMessage

                            Nothing ->
                                none
                   )
                :: (inFront <| infoTextPopup model.infoText model.infoTextDict)
                :: commonLayoutStyles
            )
          <|
            column [ width fill, height fill ]
                [ el
                    [ width fill
                    , Border.widthEach { edges | bottom = 6 }
                    , Border.color ukraineBlue
                    ]
                  <|
                    topLoadingBar model
                , el
                    [ width fill
                    , Border.widthEach { edges | top = 6 }
                    , Border.color ukraineYellow
                    ]
                  <|
                    html <|
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
    --SplitPane.view
    --    rightDockInternalConfig
    upperRightDockView model



--(lowerRightDockView model)
--model.rightDockInternal


notTheRightDockView : Model -> Html Msg
notTheRightDockView model =
    SplitPane.view
        leftDockConfig
        (leftDockView model)
        (centralAreaView model)
        model.leftDockRightEdge


leftDockView : Model -> Html Msg
leftDockView model =
    --SplitPane.view
    --    leftDockInternalConfig
    upperLeftDockView model



--(lowerLeftDockView model)
--model.leftDockInternal


upperLeftDockView : Model -> Html Msg
upperLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        ToolsController.toolsForDock
            ToolsController.DockUpperLeft
            ToolsMsg
            model.track
            model.toolOptions


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
    --SplitPane.view
    --    bottomDockConfig
    viewPaneArea model



--(bottomDockView model)
--model.bottomDockTopEdge


viewPaneArea : Model -> Html Msg
viewPaneArea model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        PaneLayoutManager.viewPanes
            PaneMsg
            model.track
            model.toolOptions.graphOptions.graph
            model.toolOptions.graphOptions
            model.toolOptions.displaySettings
            model.contentArea
            model.paneLayoutOptions
            model.toolOptions.flythroughSettings.flythrough
            model.previews


topLoadingBar model =
    let
        moreOptionsButton =
            button
                [ padding 5
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                , Border.color FlatColors.FlatUIPalette.peterRiver
                , Border.width 2
                , tooltip below (myTooltip "Other file options")
                , inFront <|
                    if model.loadOptionsMenuOpen then
                        el
                            [ moveRight 30
                            , Background.color FlatColors.ChinesePalette.antiFlashWhite
                            , htmlAttribute (style "z-index" "20")
                            ]
                            (SvgPathExtractor.view SvgMsg model.ipInfo)

                    else
                        none
                ]
                { onPress = Just ToggleLoadOptionMenu
                , label = useIconWithSize 12 FeatherIcons.moreHorizontal
                }

        loadGpxButton =
            button
                [ padding 5
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                , Border.color FlatColors.FlatUIPalette.peterRiver
                , Border.width 2
                ]
                { onPress = Just GpxRequested
                , label = text "Load GPX file"
                }

        saveButton =
            button
                [ padding 5
                , Background.color FlatColors.AussiePalette.juneBud
                , Border.color FlatColors.FlatUIPalette.peterRiver
                , Border.width 2
                ]
                { onPress = Just WriteGpxFile
                , label = text "Save GPX file"
                }
    in
    wrappedRow
        (commonLayoutStyles
            ++ [ spacing 20
               , padding 10
               , width fill

               --, Border.widthEach { left = 0, right = 0, top = 0, bottom = 2 }
               --, Border.color FlatColors.ChinesePalette.twinkleBlue
               ]
        )
        [ globalOptions model
        , loadGpxButton
        , moreOptionsButton
        , el [ Font.color <| contrastingColour model.backgroundColour ]
            (text <| bestTrackName model)
        , case model.filename of
            Just filename ->
                Input.text
                    [ padding 5
                    , onEnter WriteGpxFile
                    , width <| minimum 200 <| fill
                    ]
                    { text = filename
                    , onChange = FilenameChange
                    , placeholder = Nothing
                    , label = Input.labelHidden "filename"
                    }

            Nothing ->
                none
        , saveButton
        , Tools.OneClickQuickFix.oneClickQuickFixButton OneClickMsg model.track

        --, buyMeACoffeeButton
        , el [ alignRight ] <| StravaAuth.stravaButton model.stravaAuthentication OAuthMessage
        , el [ alignRight ] <| PaneLayoutManager.paneLayoutMenu PaneMsg model.paneLayoutOptions
        ]


buyMeACoffeeButton =
    newTabLink
        [ alignRight ]
        { url = "https://www.buymeacoffee.com/Peterward"
        , label =
            image [ height (Element.px 30), width (Element.px 130) ]
                { src = "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                , description = "Buy Me A Coffee"
                }
        }


infoTextPopup :
    Maybe ( String, String )
    -> Dict String (Dict String String)
    -> Element Msg
infoTextPopup maybeSomething dict =
    let
        close =
            Input.button [ Font.color rgtPurple, alignRight ]
                { onPress = Just HideInfoPopup
                , label = useIconWithSize 20 FeatherIcons.x
                }
    in
    case maybeSomething of
        Just ( tool, tag ) ->
            case Dict.get tool dict of
                Just innerDict ->
                    case Dict.get tag innerDict of
                        Just gotText ->
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
                                    [ html <| Markdown.toHtml [] gotText ]
                                ]

                        Nothing ->
                            none

                Nothing ->
                    none

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
            ]
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
        column (spacing 4 :: subtleToolStyles)
            [ row (alignRight :: width fill :: subtleToolStyles)
                [ colourBlock FlatColors.FlatUIPalette.silver
                , colourBlock FlatColors.FlatUIPalette.asbestos
                , colourBlock rgtDark
                ]
            , el (alignRight :: width fill :: subtleToolStyles) <|
                Input.button [ alignRight ]
                    { onPress = Just <| RestoreDefaultToolLayout
                    , label = text "Restore default layout"
                    }
            , el (alignRight :: width fill :: subtleToolStyles) <|
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
        performAction : ToolAction Msg -> Model -> Model
        performAction action foldedModel =
            case ( action, foldedModel.track ) of
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
                    { foldedModel
                        | track =
                            Just { track | currentPosition = position }
                        , needsRendering = True
                    }

                ( SetCurrentFromMapClick position, Just track ) ->
                    { foldedModel
                        | track =
                            Just { track | currentPosition = position }
                        , needsRendering = True
                    }

                ( ShowPreview previewData, Just track ) ->
                    -- Put preview into the scene.
                    -- After some thought, it is sensible to collect the preview data
                    -- since it's handy, as the alternative is another complex case
                    -- statement in ToolController.
                    { foldedModel
                        | previews =
                            Dict.insert previewData.tag previewData foldedModel.previews
                        , needsRendering = True
                    }

                ( HidePreview tag, Just track ) ->
                    { foldedModel
                        | previews =
                            Dict.remove tag foldedModel.previews
                        , needsRendering = True
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
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

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
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( BezierApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints, ( fromStart, fromEnd ) ) =
                            Tools.BezierSplines.applyUsingOptions
                                options
                                track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( CentroidAverageApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.CentroidAverage.applyUsingOptions options track

                        ( fromStart, fromEnd ) =
                            case track.markerPosition of
                                Just _ ->
                                    TrackLoaded.getRangeFromMarkers track

                                Nothing ->
                                    ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( CurveFormerApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints, ( entry, exit ) ) =
                            Tools.CurveFormer.applyUsingOptions options track

                        ( fromStart, fromEnd ) =
                            ( entry, skipCount track.trackTree - exit )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( NudgeApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints, ( entry, exit ) ) =
                            Tools.Nudge.applyUsingOptions options track

                        ( fromStart, fromEnd ) =
                            ( entry, skipCount track.trackTree - exit )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( PasteStravaSegment options, Just track ) ->
                    -- This is like Nudge in that the affected area is given
                    -- by the tool, not by the range markers.
                    let
                        ( newTree, oldPoints, ( entry, exit ) ) =
                            Tools.StravaTools.paste options track

                        ( fromStart, fromEnd ) =
                            ( entry, skipCount track.trackTree - exit )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( BendSmootherApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.BendSmoother.applyUsingOptions options track

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
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( Autofix indices, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.BendSmoother.softenMultiplePoints
                                model.toolOptions.bendSmootherOptions
                                indices
                                track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( OutAndBackApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.OutAndBack.apply options track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ApplySimplify, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.Simplify.apply foldedModel.toolOptions.simplifySettings track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( MoveAndStretchWithOptions settings, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.MoveAndStretch.apply settings track

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
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( OneClickQuickFix, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.OneClickQuickFix.apply track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        ( newOrange, newPurple ) =
                            ( indexFromDistance
                                (distanceFromIndex track.currentPosition track.trackTree)
                                (newTree |> Maybe.withDefault track.trackTree)
                            , case track.markerPosition of
                                Just purple ->
                                    Just <|
                                        indexFromDistance
                                            (distanceFromIndex purple track.trackTree)
                                            (newTree |> Maybe.withDefault track.trackTree)

                                Nothing ->
                                    Nothing
                            )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> (\trk ->
                                        { trk
                                            | trackTree = Maybe.withDefault trk.trackTree newTree
                                            , currentPosition = newOrange
                                            , markerPosition = newPurple
                                        }
                                   )

                        --|> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ApplyInterpolateWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.Interpolate.apply options track

                        ( fromStart, fromEnd ) =
                            -- Repetition of this is untidy.
                            case options.extent of
                                Tools.InterpolateOptions.ExtentIsRange ->
                                    TrackLoaded.getRangeFromMarkers track

                                Tools.InterpolateOptions.ExtentIsTrack ->
                                    ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( Straighten, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.Straightener.apply model.toolOptions.straightenOptions track

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
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( LimitGradientWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.ProfileSmooth.apply options track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    0
                                    0
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( SmoothAltitudes options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.ProfileSmooth.apply options track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    0
                                    0
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( SmoothGradients options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.ProfileSmooth.apply options track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    0
                                    0
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( CloseLoopWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.StartFinish.applyCloseLoop options track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    0
                                    0
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ReverseTrack, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.StartFinish.applyReverse track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    0
                                    0
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( MoveStartPoint newStart, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.StartFinish.applyMoveStart newStart track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action 0 0 oldPoints
                                |> (\trk ->
                                        case newTree of
                                            Just aNewTree ->
                                                { trk
                                                    | trackTree = aNewTree
                                                    , currentPosition = 0
                                                    , markerPosition = Nothing
                                                    , referenceLonLat = gpxPointFromIndex 0 aNewTree
                                                }

                                            Nothing ->
                                                trk
                                   )
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( AddRiderPens, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.StartFinish.addPens track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action 0 0 oldPoints
                                |> (\trk ->
                                        case newTree of
                                            Just aNewTree ->
                                                { trk
                                                    | trackTree = aNewTree
                                                    , currentPosition = 0
                                                    , markerPosition = Nothing
                                                    , referenceLonLat = gpxPointFromIndex 0 aNewTree
                                                }

                                            Nothing ->
                                                trk
                                   )
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ApplyRotateAndScale options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.MoveScaleRotate.applyRotateAndScale options track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        newTrack =
                            { track
                                | referenceLonLat =
                                    case newTree of
                                        Just aTree ->
                                            gpxPointFromIndex 0 aTree

                                        Nothing ->
                                            track.referenceLonLat
                            }
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ApplyRecentre coords, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.MoveScaleRotate.applyRecentre coords track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        newTrack =
                            let
                                ( lon, lat ) =
                                    track.lastMapClick

                                newReference =
                                    { longitude = Direction2d.fromAngle <| Angle.degrees lon
                                    , latitude = Angle.degrees lat
                                    , altitude = Quantity.zero
                                    }
                            in
                            { track | referenceLonLat = newReference }
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ApplyMapElevations elevations, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.MoveScaleRotate.applyMapElevations elevations track

                        ( fromStart, fromEnd ) =
                            ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( ApplyLandUseAltitudes altitudes, Just track ) ->
                    let
                        newTrack =
                            { track
                                | landUseData =
                                    LandUseDataOSM.applyAltitudes
                                        altitudes
                                        track
                            }
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( PointMovedOnMap startLon startLat endLon endLat, Just track ) ->
                    let
                        startGpx =
                            { longitude = Direction2d.fromAngle <| Angle.degrees startLon
                            , latitude = Angle.degrees startLat
                            , altitude = Quantity.zero
                            }

                        index =
                            DomainModel.nearestToLonLat
                                startGpx
                                track.currentPosition
                                track.trackTree

                        positionBeforeDrag =
                            gpxPointFromIndex index track.trackTree

                        endGpx =
                            { longitude = Direction2d.fromAngle <| Angle.degrees endLon
                            , latitude = Angle.degrees endLat
                            , altitude = positionBeforeDrag.altitude
                            }

                        newTree =
                            DomainModel.updatePointByIndexInSitu
                                index
                                endGpx
                                track.referenceLonLat
                                track.trackTree

                        ( fromStart, fromEnd ) =
                            ( index, skipCount track.trackTree - index )

                        newTrack =
                            { track | trackTree = newTree }
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    [ positionBeforeDrag ]
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( SaveLastMapClick lon lat, Just track ) ->
                    let
                        newTrack =
                            { track | lastMapClick = ( lon, lat ) }
                    in
                    { foldedModel | track = Just newTrack }

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
                            { foldedModel | modalMessage = Just "Unable to extract SVG paths" }

                ( ParseAndAppend gpxContent, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.SplitAndJoin.parseAndAppend gpxContent track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    0
                                    0
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

                ( AddTraversal edge, Just track ) ->
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

                ( AddSelfLoop node, Just track ) ->
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

                ( ChangeActiveTrack edge, Just track ) ->
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
                        , track = Tools.Graph.getTrack edge graphOptions
                        , needsRendering = True
                    }

                ( MakeRouteFromGraph, Just track ) ->
                    let
                        toolOptions =
                            foldedModel.toolOptions

                        graphOptions =
                            toolOptions.graphOptions

                        newGraphOptions =
                            Tools.Graph.makeNewRoute graphOptions

                        newToolOptions =
                            { toolOptions | graphOptions = newGraphOptions }

                        newTrack =
                            case Tools.Graph.getTrack 0 newGraphOptions of
                                Just foundNewTrack ->
                                    foundNewTrack
                                        |> TrackLoaded.addToUndoStack action 0 0 []
                                        |> Just

                                Nothing ->
                                    foldedModel.track
                    in
                    { foldedModel
                        | toolOptions = newToolOptions
                        , track = newTrack
                        , needsRendering = True
                    }

                ( LoadGpxFromStrava gpxContent, _ ) ->
                    let
                        ( modelWithNewTrack, _ ) =
                            update (GpxLoaded gpxContent) foldedModel
                    in
                    modelWithNewTrack

                ( Actions.WidenBend points adjustment, Just track ) ->
                    -- This for one contiguous set of points, i.e. one bend.
                    let
                        ( newTree, oldPoints, ( entry, exit ) ) =
                            Tools.DirectionChanges.widenBend points adjustment track

                        ( fromStart, fromEnd ) =
                            ( entry, skipCount track.trackTree - exit )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel
                        | track = Just newTrack
                        , needsRendering = True
                    }

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
                    { modelAfterSecondaryActions
                        | needsRendering = True
                    }

                ( SetMarker maybeMarker, Just track ) ->
                    let
                        updatedTrack =
                            { track | markerPosition = maybeMarker }
                    in
                    { foldedModel
                        | track = Just updatedTrack
                        , needsRendering = True
                    }

                ( StartFlythoughTicks, Just track ) ->
                    { foldedModel | flythroughRunning = True }

                ( StopFlythroughTicks, Just track ) ->
                    { foldedModel | flythroughRunning = False }

                ( StoredValueRetrieved key value, _ ) ->
                    case key of
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
                            { foldedModel
                                | toolOptions =
                                    ToolsController.restoreMeasure foldedModel.toolOptions value
                            }

                        "background" ->
                            let
                                getColour =
                                    D.decodeValue ToolsController.colourDecoder value
                            in
                            case getColour of
                                Ok colour ->
                                    { foldedModel
                                        | backgroundColour = ToolsController.decodeColour colour
                                    }

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

                        revisedModel =
                            { foldedModel | toolOptions = newTools }
                    in
                    revisedModel

                ( UndoLastAction, Just track ) ->
                    -- Without massive replumbing, I'm making the "graph walker" undo special.
                    -- We'll see how this goes; a better solution may arise.
                    let
                        topUndoAction =
                            track.undos |> List.head |> Maybe.map .action
                    in
                    case topUndoAction of
                        Just Actions.MakeRouteFromGraph ->
                            let
                                toolOptions =
                                    foldedModel.toolOptions

                                graphOptions =
                                    toolOptions.graphOptions

                                newGraphOptions =
                                    Tools.Graph.undoWalkRoute graphOptions

                                newToolOptions =
                                    { toolOptions | graphOptions = newGraphOptions }

                                newTrack =
                                    Tools.Graph.getTrack 0 newGraphOptions
                            in
                            { foldedModel
                                | toolOptions = newToolOptions
                                , track = newTrack
                                , needsRendering = True
                            }

                        _ ->
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
                                    { modelAfterRedo
                                        | track = Just trackWithCorrectRedoStack
                                        , needsRendering = True
                                    }

                                Nothing ->
                                    -- Not good, live with it.
                                    modelAfterRedo

                        _ ->
                            foldedModel

                ( LockToolOpen open id, _ ) ->
                    let
                        newToolOptions =
                            ToolsController.lockToolOpen open id foldedModel.toolOptions
                    in
                    { foldedModel | toolOptions = newToolOptions }

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

                ( MakeMapPointsDraggable flag, Just track ) ->
                    MapPortController.toggleDragging flag track

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

                ( HeapStatusUpdate _, _ ) ->
                    Delay.after 5000 TimeToUpdateMemory

                ( AddFullTrackToMap, Just track ) ->
                    Cmd.batch
                        [ MapPortController.addFullTrackToMap track
                        , Delay.after 100 FetchElevationsFromMap
                        ]

                ( FetchMapElevations, _ ) ->
                    MapPortController.requestElevations

                ( SetMapStyle url, _ ) ->
                    Cmd.batch
                        [ MapPortController.setMapStyle url
                        , Delay.after 1000 ReplaceTrackOnMapAfterStyleChange
                        ]

                ( SelectSvgFile message, _ ) ->
                    Select.file [ "text/svg" ] message

                ( LoadSvgFile message file, _ ) ->
                    Task.perform message (File.toString file)

                ( TrackFromSvg svgContent, Just track ) ->
                    showTrackOnMapCentered track

                ( SelectGpxFile message, _ ) ->
                    Select.file [ "text/gpx" ] message

                ( LoadGpxFile message file, _ ) ->
                    Task.perform message (File.toString file)

                ( TrackFromGpx gpxContent, Just track ) ->
                    showTrackOnMapCentered track

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

                _ ->
                    Cmd.none
    in
    Cmd.batch <| List.map performAction actions


showTrackOnMapCentered : TrackLoaded msg -> Cmd msg
showTrackOnMapCentered track =
    Cmd.batch
        -- Must repaint track on so that selective rendering works.
        [ MapPortController.addTrackToMap track
        , MapPortController.zoomMapToFitTrack track
        , MapPortController.addMarkersToMap track
        ]
