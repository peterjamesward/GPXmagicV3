module Tools.Flythrough exposing
    ( Flythrough
    , Msg(..)
    , Options
    , RunState(..)
    , defaultOptions
    , subscriptions
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction)
import CommonToolStyles
import DomainModel exposing (asRecord)
import Element exposing (..)
import Element.Input as Input exposing (button)
import FeatherIcons
import Length
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity
import Speed
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import Time
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure, showSpeed)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, useIcon)


toolId =
    "fly"


type Msg
    = SetFlythroughSpeed Float
    | StartFlythrough
    | PauseFlythrough
    | ResumeFlythrough
    | ResetFlythrough
    | FlythroughTick Time.Posix


type alias Flythrough =
    { cameraPosition : Point3d Length.Meters LocalCoords
    , focusPoint : Point3d Length.Meters LocalCoords
    , metresFromRouteStart : Length.Length
    , lastUpdated : Time.Posix
    , running : RunState
    , gradient : Float
    }


type RunState
    = WaitingForFirstTick
    | Running
    | Ended
    | Paused


type alias Options =
    { flythroughSpeed : Float
    , flythrough : Maybe Flythrough
    , savedCurrentPosition : Int
    }


defaultOptions =
    { flythroughSpeed = 1.0
    , flythrough = Nothing
    , savedCurrentPosition = 0
    }


eyeHeight =
    Length.meters 2.0


subscriptions : Options -> Sub Msg
subscriptions options =
    case options.flythrough of
        Just _ ->
            Time.every 100 FlythroughTick

        Nothing ->
            Sub.none


advanceInternal :
    Time.Posix
    -> Flythrough
    -> Float
    -> TrackLoaded msg
    -> Maybe Flythrough
advanceInternal newTime status speed track =
    case status.running of
        WaitingForFirstTick ->
            Just { status | lastUpdated = newTime, running = Running }

        Paused ->
            Just { status | lastUpdated = newTime }

        Ended ->
            Just status

        Running ->
            if
                status.metresFromRouteStart
                    |> Quantity.greaterThanOrEqualTo (DomainModel.trueLength track.trackTree)
            then
                Just { status | running = Ended }

            else
                let
                    tempus =
                        toFloat (Time.posixToMillis newTime - Time.posixToMillis status.lastUpdated) / 1000.0

                    newDistance =
                        status.metresFromRouteStart
                            |> Quantity.plus (Length.meters <| tempus * 10.0 ^ speed)

                    lastPointPassedIndex =
                        DomainModel.indexFromDistanceRoundedDown newDistance track.trackTree

                    currentRoad =
                        DomainModel.leafFromIndex lastPointPassedIndex track.trackTree
                            |> asRecord

                    nextRoad =
                        DomainModel.leafFromIndex (lastPointPassedIndex + 1) track.trackTree
                            |> asRecord

                    lastPointDistance =
                        DomainModel.distanceFromIndex lastPointPassedIndex track.trackTree

                    segInsetMetres =
                        newDistance |> Quantity.minus lastPointDistance

                    segLength =
                        currentRoad.trueLength

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
                                currentRoad.startPoint.space
                                currentRoad.endPoint.space
                                segFraction

                    lookingAt =
                        -- Should be looking at the next point, until we are close
                        -- enough to start looking at the one beyond that.
                        Point3d.interpolateFrom
                            currentRoad.endPoint.space
                            nextRoad.endPoint.space
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
                        , gradient = currentRoad.gradientAtStart
                    }


view : SystemSettings -> Options -> (Msg -> msg) -> Element msg
view settings options wrapper =
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
                            String.Interpolate.interpolate
                                (I18N.localisedString settings.location toolId "speed")
                                [ showSpeed settings.imperial speed ]
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

        pauseButton state =
            button
                neatToolsBorder
            <|
                case state of
                    Paused ->
                        { onPress = Just <| wrapper ResumeFlythrough
                        , label = useIcon FeatherIcons.play
                        }

                    _ ->
                        { onPress = Just <| wrapper PauseFlythrough
                        , label = useIcon FeatherIcons.pause
                        }

        playPauseButton =
            case options.flythrough of
                Nothing ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper StartFlythrough
                        , label = useIcon FeatherIcons.play
                        }

                Just flying ->
                    pauseButton flying.running
    in
    column
        (CommonToolStyles.toolContentBoxStyle settings)
        [ el [ centerX ] <|
            row [ padding 10, spacing 10, centerX ]
                [ resetButton
                , playPauseButton
                ]
        , el [ centerX ] <| flythroughSpeedSlider
        , el [ centerX ] <|
            case options.flythrough of
                Just flying ->
                    text <|
                        String.Interpolate.interpolate
                            (I18N.localisedString settings.location toolId "where")
                            [ showLongMeasure settings.imperial flying.metresFromRouteStart ]

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
        ( True, Just _ ) ->
            ( options, [] )

        _ ->
            ( { options | flythrough = Nothing }
            , []
            )


update :
    Options
    -> Msg
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update options msg track =
    case msg of
        SetFlythroughSpeed speed ->
            ( { options | flythroughSpeed = speed }
            , []
            )

        StartFlythrough ->
            ( startFlythrough track options
            , []
            )

        PauseFlythrough ->
            case options.flythrough of
                Just flythrough ->
                    ( { options | flythrough = Just { flythrough | running = Paused } }
                    , []
                    )

                Nothing ->
                    ( options, [] )

        ResumeFlythrough ->
            case options.flythrough of
                Just flythrough ->
                    ( { options | flythrough = Just { flythrough | running = Running } }
                    , []
                    )

                Nothing ->
                    ( options, [] )

        ResetFlythrough ->
            ( { options | flythrough = Nothing }
            , [ Actions.SetCurrent options.savedCurrentPosition ]
            )

        FlythroughTick posixTime ->
            case options.flythrough of
                Just flythrough ->
                    let
                        updatedFlythrough =
                            advanceInternal
                                posixTime
                                flythrough
                                options.flythroughSpeed
                                track

                        newOptions =
                            { options | flythrough = updatedFlythrough }
                    in
                    ( newOptions
                    , case updatedFlythrough of
                        Just stillFlying ->
                            [ Actions.SetCurrent <|
                                DomainModel.indexFromDistanceRoundedDown
                                    stillFlying.metresFromRouteStart
                                    track.trackTree
                            ]

                        Nothing ->
                            [ Actions.SetCurrent options.savedCurrentPosition ]
                    )

                Nothing ->
                    ( options, [] )


startFlythrough : TrackLoaded msg -> Options -> Options
startFlythrough track options =
    let
        currentRoad =
            DomainModel.leafFromIndex track.currentPosition track.trackTree
                |> asRecord

        eyePoint =
            currentRoad.startPoint.space
                |> Point3d.translateBy
                    (Vector3d.xyz Quantity.zero Quantity.zero eyeHeight)

        focusPoint =
            currentRoad.endPoint.space
                |> Point3d.translateBy
                    (Vector3d.xyz Quantity.zero Quantity.zero eyeHeight)

        flythrough =
            { metresFromRouteStart = DomainModel.distanceFromIndex track.currentPosition track.trackTree
            , cameraPosition = eyePoint
            , focusPoint = focusPoint
            , lastUpdated = Time.millisToPosix 0
            , running = WaitingForFirstTick
            , gradient = currentRoad.gradientAtStart
            }
    in
    { options
        | flythrough = Just flythrough
        , savedCurrentPosition = track.currentPosition
    }
