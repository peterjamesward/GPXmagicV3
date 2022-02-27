module Main exposing (main)

import Actions exposing (ToolAction(..))
import Angle
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
import FlatColors.FlatUIPalette
import GeoCodeDecoders exposing (IpInfo)
import GpxParser exposing (parseGPXPoints)
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as D
import Json.Encode as E exposing (string)
import LocalStorage
import MapPortController
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
import Tools.DisplaySettings
import Tools.Interpolate
import Tools.InterpolateOptions
import Tools.LimitGradientOptions
import Tools.LimitGradients
import Tools.MoveAndStretch
import Tools.MoveScaleRotate
import Tools.Nudge
import Tools.OneClickQuickFix
import Tools.OutAndBack
import Tools.Simplify
import Tools.StravaDataLoad
import Tools.StravaTools
import Tools.TrackInfoBox
import ToolsController exposing (ToolEntry, encodeColour, encodeToolState)
import TrackLoaded exposing (TrackLoaded)
import Url exposing (Url)
import UtilsForViews exposing (colourHexString)
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
    | WriteGpxFile
    | FilenameChange String
    | TimeToUpdateMemory
    | OneClickMsg Tools.OneClickQuickFix.Msg
    | FetchElevationsFromMap
    | ReplaceTrackOnMapAfterStyleChange
    | SvgMsg SvgPathExtractor.Msg
    | FlythroughTick Time.Posix
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
      , loadOptionsMenuOpen = False
      , svgFileOptions = SvgPathExtractor.defaultOptions
      , track = Nothing
      , previews = Dict.empty
      , flythroughRunning = False
      , windowSize = ( 1000, 800 )
      , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
      , modalMessage = Nothing
      , paneLayoutOptions = PaneLayoutManager.defaultOptions
      , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px 200 <| Just ( 20, 200 ))
      , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px 400 <| Just ( 50, 600 ))
      , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (SplitPane.px (800 - 200) <| Just ( 600, 990 ))
      , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px 400 <| Just ( 50, 600 ))
      , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px (500 - 200) <| Just ( 300, 570 ))
      , toolOptions = ToolsController.defaultOptions
      , isPopupOpen = False
      , backgroundColour = FlatColors.FlatUIPalette.silver
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


render : Model -> Model
render model =
    -- This is or should be the one place where rendering for 3D (and similar) happens.
    -- Map is different: it's imperative by nature, and we don't need to retain the json.
    case model.track of
        Just track ->
            let
                paneLayout =
                    PaneLayoutManager.render
                        model.toolOptions
                        model.paneLayoutOptions
                        (Tuple.first model.contentArea)
                        track
                        model.previews
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
                [ MapPortController.createMap
                    ViewMap.defaultStyleUrl
                    mapInfoWithLocation
                , MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
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
                            , showTrackOnMapCentered track
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


adoptTrackInModel : TrackLoaded Msg -> Model -> Model
adoptTrackInModel track model =
    let
        modelWithTrack =
            { model
                | track = Just track
                , paneLayoutOptions =
                    PaneLayoutManager.initialise
                        track
                        model.paneLayoutOptions
                , modalMessage = Nothing
                , previews = Dict.empty
            }

        actions =
            [ TrackHasChanged, MapRefresh ]

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
            model.toolOptions.flythroughSettings.flythrough


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
               , Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
               , Border.color FlatColors.ChinesePalette.twinkleBlue
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
        , buyMeACoffeeButton
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
            [ Font.color FlatColors.FlatUIPalette.carrot ]
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
                , colourBlock FlatColors.FlatUIPalette.wetAsphalt
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
        , Sub.map SplitLeftDockInternal <| SplitPane.subscriptions model.leftDockInternal
        , Sub.map SplitRightDockLeftEdge <| SplitPane.subscriptions model.rightDockLeftEdge
        , Sub.map SplitRightDockInternal <| SplitPane.subscriptions model.rightDockInternal
        , Sub.map SplitBottomDockTopEdge <| SplitPane.subscriptions model.bottomDockTopEdge
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
                                foldedModel.toolOptions
                                (Tuple.first model.contentArea)
                                track
                                foldedModel.previews
                                foldedModel.paneLayoutOptions
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
                        ( newTree, oldPoints, ( fromStart, fromEnd ) ) =
                            Tools.BezierSplines.applyUsingOptions options track

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel | track = Just newTrack }

                ( CentroidAverageApplyWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.CentroidAverage.applyUsingOptions options track

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

                ( OneClickQuickFix, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.OneClickQuickFix.apply track

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

                ( LimitGradientWithOptions options, Just track ) ->
                    let
                        ( newTree, oldPoints ) =
                            Tools.LimitGradients.apply options track

                        ( fromStart, fromEnd ) =
                            -- Repetition of this is untidy.
                            case options.extent of
                                Tools.LimitGradientOptions.ExtentIsRange ->
                                    TrackLoaded.getRangeFromMarkers track

                                Tools.LimitGradientOptions.ExtentIsTrack ->
                                    ( 0, 0 )

                        newTrack =
                            track
                                |> TrackLoaded.addToUndoStack action
                                    fromStart
                                    fromEnd
                                    oldPoints
                                |> TrackLoaded.useTreeWithRepositionedMarkers newTree
                    in
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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
                    { foldedModel | track = Just newTrack }

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

                ( LoadGpxFromStrava gpxContent, _ ) ->
                    let
                        ( modelWithNewTrack, _ ) =
                            update (GpxLoaded gpxContent) foldedModel
                    in
                    modelWithNewTrack

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
                    case useThisData.shape of
                        PreviewCircle ->
                            MapPortController.showPreview
                                useThisData.tag
                                "circle"
                                (colourHexString useThisData.colour)
                                (SceneBuilderMap.renderPreview useThisData)

                        PreviewLine ->
                            MapPortController.showPreview
                                useThisData.tag
                                "line"
                                (colourHexString useThisData.colour)
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

                ( TrackFromSvg svgContent, _ ) ->
                    case model.track of
                        Just track ->
                            showTrackOnMapCentered track

                        Nothing ->
                            Cmd.none

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
