module Main exposing (main)

import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Camera3d
import Delay exposing (after)
import DomainModel exposing (GPXPoint, GPXTrack, PeteTree(..), RoadSection, nearestToRay, skipCount, treeFromList, trueLength)
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
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Meters, meters)
import LocalCoords exposing (LocalCoords)
import Msg exposing (Msg(..))
import MyIP
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import Pixels exposing (Pixels)
import Point2d
import PortController
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity)
import SceneBuilder exposing (render3dView)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import Url exposing (Url)
import ViewMap
import ViewPureStyles exposing (conditionallyVisible, radioButton, sliderThumb)
import ViewThirdPerson
import ViewingMode exposing (ViewingMode(..))


type Model
    = Model ModelRecord


type alias ModelRecord =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , stravaAuthentication : O.Model
    , rawTrack : Maybe GPXTrack
    , trackTree : Maybe PeteTree
    , renderDepth : Int
    , scene : List (Entity LocalCoords)
    , currentPosition : Int
    , viewMode : ViewingMode
    , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , ipInfo : Maybe IpInfo
    }


main : Program (Maybe (List Int)) Model Msg
main =
    -- This is the 'main' from OAuth example/
    application
        { init = Maybe.map StravaAuth.convertBytes >> init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always (OAuthMessage NoOp)
        , onUrlChange = always (OAuthMessage NoOp)
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
        , rawTrack = Nothing
        , trackTree = Nothing
        , renderDepth = 0
        , scene = []
        , currentPosition = 0
        , viewMode = ViewThird
        , viewDimensions = ( Pixels.pixels 800, Pixels.pixels 500 )
        , ipInfo = Nothing
        }
    , Cmd.batch
        [ authCmd
        , Task.perform AdjustTimeZone Time.here
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        AdjustTimeZone newZone ->
            ( Model { model | zone = newZone }
            , MyIP.requestIpInformation ReceivedIpDetails
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
            ( Model { model | ipInfo = ipInfo }
            , Cmd.batch
                [ MyIP.sendIpInfo model.time IpInfoAcknowledged ipInfo
                , PortController.createMap mapInfoWithLocation
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
                        | rawTrack = Just gpxTrack
                        , trackTree = trackTree
                        , renderDepth = 10
                    }
            in
            ( modelWithTrack
                |> renderModel
                |> Model
            , Cmd.batch
                [ PortController.addTrackToMap modelWithTrack
                , PortController.centreMap modelWithTrack
                , after 100 RepaintMap
                ]
            )

        RepaintMap ->
            ( Model model, PortController.refreshMap )

        SetRenderDepth depth ->
            ( { model | renderDepth = depth }
                |> renderModel
                |> Model
            , Cmd.none
            )

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
            ( { model | currentPosition = pos }
                |> renderModel
                |> Model
            , Cmd.none
            )

        ImageClick event ->
            ( { model | currentPosition = detectHit event model }
                |> renderModel
                |> Model
            , Cmd.none
            )

        SetViewMode viewMode ->
            ( Model { model | viewMode = viewMode }
            , if viewMode == ViewMap then
                Cmd.none

              else
                Cmd.none
            )

        PortMessage json ->
            ( Model model, Cmd.none )


renderModel : ModelRecord -> ModelRecord
renderModel model =
    { model | scene = render3dView model }


detectHit : Mouse.Event -> ModelRecord -> Int
detectHit event model =
    --TODO: Move into view/pane/whatever it will be.
    case model.trackTree of
        Just (Node topNode) ->
            let
                box =
                    topNode.nodeContent.boundingBox

                ( x, y ) =
                    event.offsetPos

                screenPoint =
                    Point2d.pixels x y

                ( w, h ) =
                    model.viewDimensions

                ( wFloat, hFloat ) =
                    ( toFloatQuantity w, toFloatQuantity h )

                screenRectangle =
                    Rectangle2d.from
                        (Point2d.xy Quantity.zero hFloat)
                        (Point2d.xy wFloat Quantity.zero)

                camera =
                    ViewThirdPerson.deriveCamera box

                ray =
                    Camera3d.ray camera screenRectangle screenPoint
            in
            nearestToRay ray (Node topNode)

        _ ->
            0


view : Model -> Browser.Document Msg
view (Model model) =
    { title = "GPXmagic 2.0"
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
            column
                [ width fill ]
                [ topLoadingBar model
                , contentArea model
                ]
        ]
    }


topLoadingBar model =
    let
        loadGpxButton =
            button
                [ padding 5
                , Border.width 2
                , Border.rounded 4
                , Border.color FlatColors.BritishPalette.chainGangGrey
                ]
                { onPress = Just GpxRequested
                , label = text "Load GPX from your computer"
                }
    in
    row [ spacing 20, padding 10 ]
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
            [ Input.optionWith ViewThird <| radioButton "Third person"
            , Input.optionWith ViewMap <| radioButton "Map"
            ]
    in
    Input.radioRow
        [ Border.rounded 6 ]
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
                , label = Input.labelBelow [] (text "Label goes here")
                , min = 0
                , max = toFloat <| trackLength
                , step = Just 1
                , thumb = sliderThumb
                }

        leftPane =
            -- NOTE that the Map DIV must be constructed once only, or the map gets upset.
            column
                [ width fill, alignTop, padding 10, centerX ]
                [ column
                    [ width fill, alignTop, padding 10, centerX ]
                    [ viewModeChoices model
                    , conditionallyVisible (model.viewMode /= ViewMap) <| ViewThirdPerson.view model
                    , conditionallyVisible (model.viewMode == ViewMap) <| ViewMap.view model
                    ]
                , el [ height (px 10) ] none
                , case model.trackTree of
                    Just treeNode ->
                        column
                            [ width fill, alignTop, padding 10, centerX ]
                            [ slider <| 1 + skipCount treeNode
                            , text <| "Length: " ++ (String.fromInt <| round <| Length.inMeters <| trueLength treeNode)
                            , text <| "Points: " ++ (String.fromInt <| 1 + skipCount treeNode)
                            ]

                    Nothing ->
                        text "Information will be shown here"
                ]

        rightPane =
            column [ spacing 5, padding 5, alignTop ]
                [ text "Where are the tools?"
                ]
    in
    column [ width fill, padding 5 ]
        [ row []
            [ el [ width <| px minimumLeftPane ] none
            ]
        , row [ width fill, spacing 5, padding 5 ]
            [ el [ width fill, alignTop ] leftPane
            , el [ alignTop, width fill ] rightPane
            ]
        , row []
            [ el [ width <| px minimumLeftPane ] none
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ randomBytes (\ints -> OAuthMessage (GotRandomBytes ints))
        , PortController.messageReceiver PortMessage
        ]
