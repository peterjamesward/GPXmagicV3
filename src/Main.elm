module Main exposing (main)

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
import Element.Font as Font
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Select as Select
import FlatColors.BritishPalette
import FlatColors.ChinesePalette
import GeoCodeDecoders exposing (IpInfo)
import GpxParser exposing (parseGPXPoints)
import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Http
import Json.Encode as E
import Length exposing (Meters)
import LocalStorage
import MapPortsController
import ModelRecord exposing (Model(..), ModelRecord)
import MyIP
import OAuthPorts as O exposing (randomBytes)
import OAuthTypes as O exposing (OAuthMsg(..))
import Pixels exposing (Pixels)
import Quantity
import SplitPane.SplitPane as SplitPane exposing (..)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import TrackInfoBox exposing (trackInfoBox)
import Url exposing (Url)
import ViewMap
import ViewPureStyles exposing (commonLayoutStyles, conditionallyVisible, radioButton, sliderThumb)
import ViewThirdPerson
import ViewingMode exposing (ViewingMode(..))


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | SetCurrentPosition Int
    | SetViewMode ViewingMode
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
    ( Model
        { filename = Nothing
        , time = Time.millisToPosix 0
        , zone = Time.utc
        , stravaAuthentication = authData
        , trackTree = Nothing
        , renderDepth = 0
        , scene = []
        , currentPosition = 0
        , viewMode = ViewThird
        , viewDimensions = ( Pixels.pixels 800, Pixels.pixels 500 )
        , ipInfo = Nothing
        , mapClickDebounce = False
        , lastMapClick = ( 0.0, 0.0 )
        , viewContext = Nothing
        , referenceLonLat = GPXSource Direction2d.x Quantity.zero Quantity.zero
        , leftDockRightEdge =
            SplitPane.init Horizontal
                |> configureSplitter (percentage 0.2 <| Just ( 0.01, 0.49 ))
        , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (percentage 0.4 <| Just ( 0.1, 0.9 ))
        , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (percentage 0.8 <| Just ( 0.6, 0.99 ))
        , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (percentage 0.6 <| Just ( 0.1, 0.9 ))
        , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter (percentage 0.8 <| Just ( 0.6, 0.99 ))
        , windowSize = ( 1000, 800 )
        , contentArea = ( Pixels.pixels 800, Pixels.pixels 500 )
        }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , LocalStorage.storageListKeys
        , Task.attempt GotWindowSize Dom.getViewport
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        AdjustTimeZone newZone ->
            ( Model { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
            )

        MapPortsMessage mapMsg ->
            let
                ( newModel, cmd ) =
                    MapPortsController.update mapMsg model MapPortsMessage
            in
            ( Model model, cmd )

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
            ( Model { model | ipInfo = ipInfo }
            , Cmd.batch
                [ MapPortsController.createMap mapInfoWithLocation

                --, MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                ]
            )

        IpInfoAcknowledged _ ->
            ( Model model, Cmd.none )

        GpxRequested ->
            ( Model model
            , Select.file [ "text/gpx" ] GpxSelected
            )

        GpxSelected file ->
            ( Model { model | filename = Just (File.name file) }
            , Task.perform GpxLoaded (File.toString file)
            )

        GpxLoaded content ->
            let
                gpxTrack =
                    parseGPXPoints content

                trackTree =
                    treeFromList gpxTrack

                modelWithTrack =
                    { model
                        | trackTree = trackTree
                        , renderDepth = 10
                        , viewContext = Maybe.map (ViewThirdPerson.initialiseView 0) trackTree
                        , referenceLonLat =
                            List.head gpxTrack
                                |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)
                    }

                ( finalModel, cmd ) =
                    modelWithTrack |> Actions.updateAllDisplays
            in
            ( Model finalModel, cmd )

        SetRenderDepth depth ->
            let
                ( finalModel, cmd ) =
                    { model | renderDepth = depth } |> Actions.updateAllDisplays
            in
            ( Model finalModel, cmd )

        --Delegate wrapped OAuthmessages. Be bowled over if this works first time. Or fiftieth.
        --Maybe look after to see if there is yet a token. Easy way to know.
        OAuthMessage authMsg ->
            let
                ( newAuthData, authCmd ) =
                    StravaAuth.update authMsg model.stravaAuthentication

                isToken =
                    getStravaToken newAuthData
            in
            ( Model { model | stravaAuthentication = newAuthData }
            , Cmd.map OAuthMessage authCmd
            )

        SetCurrentPosition pos ->
            -- Slider moves pointer and recentres view.
            case model.trackTree of
                Just treeTop ->
                    let
                        ( finalModel, cmd ) =
                            { model | currentPosition = pos }
                                |> Actions.updateAllDisplays
                    in
                    ( Model finalModel, cmd )

                Nothing ->
                    ( Model model, Cmd.none )

        SetViewMode newMode ->
            let
                ( finalModel, cmd ) =
                    { model | viewMode = newMode }
                        |> Actions.updateAllDisplays
            in
            ( Model finalModel, cmd )

        ImageMessage imageMsg ->
            let
                ( newModel, cmds ) =
                    ViewThirdPerson.update imageMsg model ImageMessage
            in
            ( Model newModel, cmds )

        StorageMessage json ->
            ( Model model, Cmd.none )

        SplitLeftDockRightEdge m ->
            ( { model | leftDockRightEdge = SplitPane.update m model.leftDockRightEdge }
                |> adjustSpaceForContent
                |> Model
            , Cmd.none
            )

        SplitLeftDockInternal m ->
            ( { model | leftDockInternal = SplitPane.update m model.leftDockInternal }
                |> adjustSpaceForContent
                |> Model
            , Cmd.none
            )

        SplitRightDockLeftEdge m ->
            ( { model | rightDockLeftEdge = SplitPane.update m model.rightDockLeftEdge }
                |> adjustSpaceForContent
                |> Model
            , Cmd.none
            )

        SplitRightDockInternal m ->
            ( { model | rightDockInternal = SplitPane.update m model.rightDockInternal }
                |> adjustSpaceForContent
                |> Model
            , Cmd.none
            )

        SplitBottomDockTopEdge m ->
            ( { model | bottomDockTopEdge = SplitPane.update m model.bottomDockTopEdge }
                |> adjustSpaceForContent
                |> Model
            , Cmd.none
            )

        Resize width height ->
            ( { model | windowSize = ( toFloat width, toFloat height ) }
                |> adjustSpaceForContent
                |> Model
            , Cmd.none
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
                        |> Model
                    , Cmd.none
                    )

                Err error ->
                    ( Model model, Cmd.none )


adjustSpaceForContent : ModelRecord -> ModelRecord
adjustSpaceForContent model =
    let
        availableWidthFraction =
            (1.0 - SplitPane.getPosition model.leftDockRightEdge)
                * SplitPane.getPosition model.rightDockLeftEdge

        availableHeightFraction =
            SplitPane.getPosition model.bottomDockTopEdge

        ( reservedWidth, reservedHeight ) =
            ( 50, 120 )

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
view (Model model) =
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


leftDockView : ModelRecord -> Html Msg
leftDockView model =
    SplitPane.view
        leftDockInternalConfig
        (upperLeftDockView model)
        (lowerLeftDockView model)
        model.leftDockInternal


upperLeftDockView : ModelRecord -> Html Msg
upperLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        trackInfoBox model.trackTree


lowerLeftDockView : ModelRecord -> Html Msg
lowerLeftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        trackInfoBox model.trackTree


rightDockView : ModelRecord -> Html Msg
rightDockView model =
    SplitPane.view
        rightDockInternalConfig
        (upperRightDockView model)
        (lowerRightDockView model)
        model.rightDockInternal


upperRightDockView : ModelRecord -> Html Msg
upperRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        trackInfoBox model.trackTree


lowerRightDockView : ModelRecord -> Html Msg
lowerRightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        trackInfoBox model.trackTree


bottomDockView : ModelRecord -> Html a
bottomDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        commonLayoutStyles
    <|
        trackInfoBox model.trackTree


notTheLeftDockView : ModelRecord -> Html Msg
notTheLeftDockView model =
    SplitPane.view
        rightDockConfig
        (centralAreaView model)
        (rightDockView model)
        model.rightDockLeftEdge


centralAreaView : ModelRecord -> Html Msg
centralAreaView model =
    SplitPane.view
        bottomDockConfig
        (viewPaneArea model)
        (bottomDockView model)
        model.bottomDockTopEdge


viewPaneArea : ModelRecord -> Html Msg
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
                , Background.color FlatColors.ChinesePalette.twinkleBlue
                ]
                { onPress = Just GpxRequested
                , label = text "Load GPX file"
                }
    in
    row
        ([ spacing 20
         , padding 5
         , width fill
         , Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
         , Border.color FlatColors.ChinesePalette.twinkleBlue
         ]
            ++ commonLayoutStyles
        )
        [ loadGpxButton
        ]


minimumLeftPane =
    600


maximumLeftPane =
    1400


viewModeChoices : ModelRecord -> Element Msg
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


contentArea : ModelRecord -> Element Msg
contentArea model =
    let
        slider trackLength =
            Input.slider
                ViewPureStyles.wideSliderStyles
                { onChange = round >> SetCurrentPosition
                , value = toFloat model.currentPosition
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
            , padding 10
            , centerX
            ]
            [ viewModeChoices model
            , conditionallyVisible (model.viewMode /= ViewMap) <|
                ViewThirdPerson.view model ImageMessage
            , conditionallyVisible (model.viewMode == ViewMap) <|
                ViewMap.view model MapPortsMessage
            ]
        , case model.trackTree of
            Just treeNode ->
                el [ centerX ] <| slider <| 1 + skipCount treeNode

            Nothing ->
                none
        ]


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
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
