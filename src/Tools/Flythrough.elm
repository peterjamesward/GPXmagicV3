module Tools.Flythrough exposing (..)

import Actions exposing (ToolAction)
import DomainModel exposing (EarthPoint, asRecord)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FeatherIcons
import FlatColors.ChinesePalette
import Length
import Point3d exposing (Point3d)
import Quantity
import Speed
import Time
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure, showSpeed)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles, useIcon)



{-
   Note that for v3, Flythrough needs to update the current point for rendering to work
-}


type Msg
    = SetFlythroughSpeed Float
    | StartFlythrough
    | PauseFlythrough
    | ResetFlythrough


type alias Flythrough =
    { cameraPosition : EarthPoint
    , savedCurrentPosition : Int
    , focusPoint : EarthPoint
    , metresFromRouteStart : Length.Length
    , lastUpdated : Time.Posix
    , running : Bool
    }


type alias Options =
    { flythroughSpeed : Float
    , flythrough : Maybe Flythrough
    , modelTime : Time.Posix
    }


defaultOptions =
    { flythroughSpeed = 1.0
    , flythrough = Nothing
    , modelTime = Time.millisToPosix 0
    }


eyeHeight =
    Length.meters 2.0


flythrough :
    Time.Posix
    -> Flythrough
    -> Float
    -> TrackLoaded msg
    -> Maybe Flythrough
flythrough newTime status speed track =
    let
        tempus =
            toFloat (Time.posixToMillis newTime - Time.posixToMillis status.lastUpdated) / 1000.0

        newDistance =
            status.metresFromRouteStart
                |> Quantity.plus (Length.meters <| tempus * 10.0 ^ speed)

        lastPointPassedIndex =
            DomainModel.indexFromDistance newDistance track.trackTree

        currentRoad =
            DomainModel.leafFromIndex lastPointPassedIndex track.trackTree

        nextRoad =
            DomainModel.leafFromIndex (lastPointPassedIndex + 1) track.trackTree

        lastPointDistance =
            DomainModel.distanceFromIndex lastPointPassedIndex track.trackTree
    in
    if not status.running then
        Just { status | lastUpdated = newTime }

    else if
        status.metresFromRouteStart
            |> Quantity.greaterThanOrEqualTo (DomainModel.trueLength track.trackTree)
    then
        Just { status | running = False }

    else
        let
            segInsetMetres =
                newDistance |> Quantity.minus lastPointDistance

            segLength =
                DomainModel.trueLength currentRoad

            segFraction =
                Quantity.ratio segInsetMetres segLength

            segRemaining =
                segLength
                    |> Quantity.minus segInsetMetres
                    |> Length.inMeters

            headTurnFraction =
                -- Allow for POV rotation as we near segment end.
                clamp 0.0 1.0 <| (10.0 - segRemaining) / 10.0

            camera3d =
                -- The camera is where the bike is!
                Point3d.translateBy
                    (Vector3d.xyz Quantity.zero Quantity.zero eyeHeight)
                <|
                    Point3d.interpolateFrom
                        (DomainModel.startPoint currentRoad)
                        (DomainModel.endPoint currentRoad)
                        segFraction

            lookingAt =
                -- Should be looking at the next point, until we are close
                -- enough to start looking at the one beyond that.
                Point3d.interpolateFrom
                    (DomainModel.endPoint currentRoad)
                    (DomainModel.endPoint nextRoad)
                    headTurnFraction
                    |> Point3d.translateBy
                        (Vector3d.xyz Quantity.zero Quantity.zero eyeHeight)
        in
        Just
            { status
                | metresFromRouteStart = newDistance
                , lastUpdated = newTime
                , cameraPosition = camera3d
                , focusPoint = lookingAt
            }


view : Bool -> Options -> (Msg -> msg) -> Element msg
view imperial options wrapper =
    let
        speed =
            Speed.metersPerSecond (10.0 ^ options.flythroughSpeed)

        flythroughSpeedSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetFlythroughSpeed
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Speed = "
                                ++ showSpeed imperial speed
                , min = 1.0 -- i.e. 1
                , max = 3.0 -- i.e. 1000
                , step = Nothing
                , value = options.flythroughSpeed
                , thumb = Input.defaultThumb
                }

        resetButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper ResetFlythrough
                , label = useIcon FeatherIcons.rewind
                }

        playButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper StartFlythrough
                , label = useIcon FeatherIcons.play
                }

        pauseButton isRunning =
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| PauseFlythrough
                , label =
                    useIcon <|
                        if isRunning then
                            FeatherIcons.pause

                        else
                            FeatherIcons.play
                }

        playPauseButton =
            case options.flythrough of
                Nothing ->
                    playButton

                Just flying ->
                    pauseButton flying.running
    in
    column
        [ padding 10
        , spacing 10
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ el [ centerX ] <|
            row [ padding 10, spacing 10, centerX ]
                [ resetButton
                , playPauseButton
                ]
        , el [ centerX ] <| flythroughSpeedSlider
        , el [ centerX ] <|
            case options.flythrough of
                Just flying ->
                    row [ spacing 10 ]
                        [ text "From start "
                        , text <| showLongMeasure imperial flying.metresFromRouteStart
                        ]

                Nothing ->
                    none
        ]


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, [] )

        _ ->
            ( { options | flythrough = Nothing }, [] )


update :
    Options
    -> Msg
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update options msg track =
    case msg of
        SetFlythroughSpeed speed ->
            ( { options | flythroughSpeed = speed }, [] )

        StartFlythrough ->
            ( startFlythrough track options, [] )

        PauseFlythrough ->
            ( togglePause options, [] )

        ResetFlythrough ->
            ( { options | flythrough = Nothing }, [] )


prepareFlythrough : TrackLoaded msg -> Options -> Maybe Flythrough
prepareFlythrough track options =
    let
        currentRoad =
            DomainModel.leafFromIndex track.currentPosition track.trackTree
                |> asRecord

        eyePoint =
            currentRoad.startPoint
                |> Point3d.translateBy
                    (Vector3d.xyz Quantity.zero Quantity.zero eyeHeight)

        focusPoint =
            currentRoad.endPoint
                |> Point3d.translateBy
                    (Vector3d.xyz Quantity.zero Quantity.zero eyeHeight)

        cameraShift =
            Point3d.interpolateFrom eyePoint focusPoint -1.0
    in
    Just
        { metresFromRouteStart = DomainModel.distanceFromIndex track.currentPosition track.trackTree
        , running = False
        , cameraPosition = eyePoint
        , focusPoint = focusPoint
        , lastUpdated = options.modelTime
        , savedCurrentPosition = track.currentPosition
        }


startFlythrough : TrackLoaded msg -> Options -> Options
startFlythrough track options =
    case prepareFlythrough track options of
        Just flying ->
            { options | flythrough = Just { flying | running = True } }

        Nothing ->
            options


togglePause : Options -> Options
togglePause options =
    case options.flythrough of
        Just flying ->
            { options
                | flythrough =
                    Just { flying | running = not flying.running }
            }

        Nothing ->
            options


isActive : Options -> Bool
isActive options =
    Maybe.map .running options.flythrough |> Maybe.withDefault False
