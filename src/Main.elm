module Main exposing (main)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Camera3d
import Color
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (GPXPoint, GPXTrack, PeteTree(..), RoadSection, treeFromList)
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
import SceneBuilder exposing (render3dView)
import StravaAuth exposing (getStravaToken)
import Task
import Time
import Url exposing (Url)
import ViewPureStyles
import Viewpoint3d


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | SetCurrentPosition Int


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
            ( { model
                | rawTrack = Just gpxTrack
                , trackTree = trackTree
                , renderDepth = 10
              }
                |> renderModel
                |> Model
            , Cmd.none
            )

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


renderModel : ModelRecord -> ModelRecord
renderModel model =
    -- SAme but always full depth with given region from marker.
    { model | scene = render3dView model }


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
        leftPane =
            column
                [ width fill, alignTop ]
                [ case model.trackTree of
                    Just (Node topNode) ->
                        let
                            box =
                                topNode.nodeContent.boundingBox

                            cameraViewpoint =
                                Viewpoint3d.lookAt
                                    { eyePoint =
                                        Point3d.xyz
                                            (BoundingBox3d.minX box)
                                            (BoundingBox3d.minY box)
                                            (BoundingBox3d.maxZ box)
                                    , focalPoint = BoundingBox3d.centerPoint box
                                    , upDirection = Direction3d.positiveZ
                                    }

                            perspectiveCamera =
                                Camera3d.perspective
                                    { viewpoint = cameraViewpoint
                                    , verticalFieldOfView = Angle.degrees 60
                                    }
                        in
                        column []
                            [ text <|
                                "Length: "
                                    ++ (String.fromFloat <| Length.inMeters topNode.nodeContent.trueLength)
                            , text <|
                                "Points: "
                                    ++ String.fromInt topNode.nodeContent.gpxGapCount
                            , html <|
                                Scene3d.cloudy
                                    { camera = perspectiveCamera
                                    , dimensions = ( Pixels.pixels 800, Pixels.pixels 500 )
                                    , background = backgroundColor Color.lightBlue
                                    , clipDepth = Length.meters 1
                                    , entities = model.scene
                                    , upDirection = positiveZ
                                    }
                            , Input.slider
                                ViewPureStyles.wideSliderStyles
                                { onChange = round >> SetCurrentPosition
                                , value = toFloat model.currentPosition
                                , label = Input.labelBelow [] (text "Label goes here")
                                , min = 0
                                , max = toFloat <| topNode.nodeContent.gpxGapCount + 1
                                , step = Just 1
                                , thumb = Input.defaultThumb
                                }
                            ]

                    _ ->
                        text "No data"
                ]

        rightPane =
            column [ spacing 5, padding 5, alignTop ]
                [ text <| "Render depth: " ++ String.fromInt model.renderDepth
                , button []
                    { label = text "More"
                    , onPress = Just <| SetRenderDepth (min 25 (model.renderDepth + 1))
                    }
                , button []
                    { label = text "Less"
                    , onPress = Just <| SetRenderDepth (max 1 (model.renderDepth - 1))
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
