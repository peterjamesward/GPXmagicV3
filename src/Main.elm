module Main exposing (main)

import Actions
import Browser exposing (application)
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
import Html.Attributes exposing (style)
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
import SplitPane
    exposing
        ( Orientation(..)
        , SizeUnit(..)
        , ViewConfig
        , configureSplitter
        , createViewConfig
        , percentage
        , px
        )
import StravaAuth exposing (getStravaToken)
import Task
import Time
import TrackInfoBox exposing (trackInfoBox)
import Url exposing (Url)
import ViewMap
import ViewPureStyles exposing (conditionallyVisible, radioButton, sliderThumb)
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
                |> configureSplitter (percentage 0.2 <| Just ( 0.2, 0.8 ))
        , leftDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px 450 <| Just ( 0, 800 ))
        , rightDockLeftEdge =
            SplitPane.init Horizontal
                |> configureSplitter (percentage 0.8 <| Just ( 0.2, 0.8 ))
        , rightDockInternal =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px 450 <| Just ( 0, 800 ))
        , bottomDockTopEdge =
            SplitPane.init Vertical
                |> configureSplitter (SplitPane.px 450 <| Just ( 0, 800 ))
        }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        , LocalStorage.storageListKeys
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
            ( Model { model | leftDockRightEdge = SplitPane.update m model.leftDockRightEdge }
            , Cmd.none
            )

        SplitLeftDockInternal m ->
            ( Model { model | leftDockInternal = SplitPane.update m model.leftDockInternal }
            , Cmd.none
            )

        SplitRightDockLeftEdge m ->
            ( Model { model | rightDockLeftEdge = SplitPane.update m model.rightDockLeftEdge }
            , Cmd.none
            )

        SplitRightDockInternal m ->
            ( Model { model | rightDockInternal = SplitPane.update m model.rightDockInternal }
            , Cmd.none
            )

        SplitBottomDockTopEdge m ->
            ( Model { model | bottomDockTopEdge = SplitPane.update m model.bottomDockTopEdge }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view (Model model) =
    { title = "GPXmagic Labs"
    , body =
        [ layout
            [ width fill
            , padding 10
            , spacing 10
            , Font.size 16
            , height fill
            , Background.color FlatColors.ChinesePalette.antiFlashWhite
            ]
          <|
            column [ width fill, height fill ]
                [ topLoadingBar model
                , html <|
                    div
                        [ style "width" "1000px"
                        , style "height" "800px"
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


leftDockView : ModelRecord -> Html a
leftDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        [ padding 2, spacing 5, height fill ]
    <|
        trackInfoBox model.trackTree


rightDockView : ModelRecord -> Html a
rightDockView model =
    layoutWith { options = [ noStaticStyleSheet ] }
        [ padding 2, spacing 5, height fill ]
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
    layoutWith { options = [ noStaticStyleSheet ] }
        []
    <|
        contentArea model


topLoadingBar model =
    let
        loadGpxButton =
            button
                [ padding 5
                , Background.color FlatColors.ChinesePalette.twinkleBlue
                , Font.size 14
                , Font.family
                    [ Font.typeface "Open Sans"
                    , Font.sansSerif
                    ]
                ]
                { onPress = Just GpxRequested
                , label = text "Load GPX file"
                }
    in
    row
        [ spacing 20
        , padding 5
        , width fill
        , Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
        , Border.color FlatColors.ChinesePalette.twinkleBlue
        ]
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

        leftPane =
            -- NOTE that the Map DIV must be constructed once only, or the map gets upset.
            column
                [ width fill, alignTop, padding 10, centerX, height fill ]
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
                        slider <| 1 + skipCount treeNode

                    Nothing ->
                        none
                ]
    in
    column [ width fill, padding 5, height fill ]
        [ row []
            [ el [ width <| Element.px minimumLeftPane ] none
            ]
        , row [ width fill, spacing 5, padding 5 ]
            [ el [ width fill, alignTop ] leftPane
            ]
        , row [ width <| Element.px minimumLeftPane, alignBottom ]
            [ trackInfoBox model.trackTree
            ]
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
        ]
