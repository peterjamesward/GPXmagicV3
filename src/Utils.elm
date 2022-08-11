module Utils exposing (..)

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Http exposing (Error(..))
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Quantity exposing (Quantity)
import Time


equalIntervals : Int -> Maybe Time.Posix -> Maybe Time.Posix -> List (Maybe Time.Posix)
equalIntervals pointsToAdd fromTime toTime =
    List.range 1 pointsToAdd
        |> List.map
            (\pointNum ->
                interpolateTimes
                    (toFloat pointNum / (1.0 + toFloat pointsToAdd))
                    fromTime
                    toTime
            )


interpolateTimes : Float -> Maybe Time.Posix -> Maybe Time.Posix -> Maybe Time.Posix
interpolateTimes proportion fromTime toTime =
    case ( fromTime, toTime ) of
        ( Just timeA, Just timeB ) ->
            let
                ( floatTimeA, floatTimeB ) =
                    ( toFloat <| Time.posixToMillis timeA
                    , toFloat <| Time.posixToMillis timeB
                    )

                newFloat =
                    (1.0 - proportion)
                        * floatTimeA
                        + proportion
                        * floatTimeB

                newInt =
                    floor newFloat

                intermediate =
                    Time.millisToPosix newInt
            in
            Just intermediate

        _ ->
            Nothing


interpolateLongitude : Float -> Direction2d LocalCoords -> Direction2d LocalCoords -> Direction2d LocalCoords
interpolateLongitude proportion directionA directionB =
    let
        netTurn =
            directionB |> Direction2d.angleFrom directionA

        newTurn =
            netTurn |> Quantity.multiplyBy proportion
    in
    directionA |> Direction2d.rotateBy newTurn


interpolateLatitude : Float -> Angle -> Angle -> Angle
interpolateLatitude proportion directionA directionB =
    Quantity.interpolateFrom directionA directionB proportion


interpolateAltitude : Float -> Quantity Float Meters -> Quantity Float Meters -> Quantity Float Meters
interpolateAltitude proportion altA altB =
    Quantity.interpolateFrom altA altB proportion


addTimes : Maybe Time.Posix -> Maybe Time.Posix -> Maybe Time.Posix
addTimes time1 time2 =
    case ( time1, time2 ) of
        ( Just t1, Just t2 ) ->
            Just <| Time.millisToPosix (Time.posixToMillis t1 + Time.posixToMillis t2)

        _ ->
            Nothing


subtractTimes : Maybe Time.Posix -> Maybe Time.Posix -> Maybe Time.Posix
subtractTimes startTime endTime =
    case ( startTime, endTime ) of
        ( Just start, Just end ) ->
            Just <| Time.millisToPosix (Time.posixToMillis end - Time.posixToMillis start)

        _ ->
            Nothing


timeLessThanOrEqualTo : Maybe Time.Posix -> Maybe Time.Posix -> Bool
timeLessThanOrEqualTo baseline sample =
    case ( baseline, sample ) of
        ( Just isBaseline, Just isSample ) ->
            Time.posixToMillis isSample <= Time.posixToMillis isBaseline

        _ ->
            True


reversingCons : List a -> List a -> List a
reversingCons xs ys =
    -- Use this for speed when order can be ignored.
    case ( xs, ys ) of
        ( [], _ ) ->
            ys

        ( _, [] ) ->
            xs

        ( x :: moreX, _ ) ->
            reversingCons moreX (x :: ys)


combineLists : List (List a) -> List a
combineLists lists =
    List.foldl reversingCons [] lists


elide : List a -> List a
elide input =
    -- Fold is essential  for performance.
    -- Two passes here means we get the list back the right way round.
    let
        helper : List a -> List a -> List a
        helper accum source =
            case source of
                aa :: bb :: cc ->
                    helper (aa :: accum) cc

                [ zz ] ->
                    zz :: accum

                [] ->
                    accum
    in
    input |> helper [] |> helper []


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server"

        BadStatus 504 ->
            "The Overpass server is busy; land use data not available"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Error 400 trying to fetch land use data"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            errorMessage
