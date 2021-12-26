module Main exposing (main)

import Angle
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Camera3d
import Color exposing (grey)
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (GPXPoint, GPXTrack, PeteTree(..), RoadSection, render, treeFromList)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Select as Select
import GpxParser exposing (parseGPXPoints)
import Length exposing (Meters, meters)
import LocalCoords exposing (LocalCoords)
import OAuthPorts exposing (randomBytes)
import OAuthTypes as O exposing (..)
import Pixels
import Point3d exposing (Point3d)
import Scene3d exposing (Entity, backgroundColor)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import Url exposing (Url)
import Viewpoint3d


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int


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
            , Cmd.none
            )

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
            in
            ( Model
                { model
                    | rawTrack = Just gpxTrack
                    , trackTree = trackTree
                    , scene =
                        case model.trackTree of
                            Just tree ->
                                render model.renderDepth tree []

                            Nothing ->
                                []
                }
            , Cmd.none
            )

        SetRenderDepth depth ->
            ( Model
                { model
                    | renderDepth = depth
                    , scene =
                        case model.trackTree of
                            Just tree ->
                                render depth tree []

                            Nothing ->
                                []
                }
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
                []
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


contentArea : ModelRecord -> Element Msg
contentArea model =
    let
        cameraViewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters -5000 -5000 2000
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }

        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 75
                }

        leftPane =
            column
                [ width fill, alignTop ]
                [ case model.trackTree of
                    Just (Node topNode) ->
                        column []
                            [ text <| String.fromFloat <| Length.inMeters topNode.nodeContent.trueLength
                            , html <|
                                Scene3d.cloudy
                                    { camera = perspectiveCamera
                                    , dimensions = ( Pixels.pixels 800, Pixels.pixels 500 )
                                    , background = backgroundColor Color.lightBlue
                                    , clipDepth = Length.meters 1
                                    , entities = model.scene
                                    , upDirection = positiveZ
                                    }
                            ]

                    _ ->
                        text "No data"
                ]

        rightPane =
            column [ spacing 5, padding 5, alignTop ]
                [ Input.slider
                    [ Element.height (Element.px 30)

                    -- Here is where we're creating/styling the "track"
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Border.rounded 2
                            ]
                            Element.none
                        )
                    ]
                    { onChange = SetRenderDepth << round
                    , label =
                        Input.labelAbove []
                            (text "Render depth")
                    , min = 0
                    , max = 10
                    , step = Just 1
                    , value = toFloat model.renderDepth
                    , thumb =
                        Input.defaultThumb
                    }
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
        ]
