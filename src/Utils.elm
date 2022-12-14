module Utils exposing (addTimes, deDupe, elide, equalIntervals, errorToString, interpolateTimes, subtractTimes)

import Http exposing (Error(..))
import Time


deDupe : (a -> a -> Bool) -> List a -> List a
deDupe areSame inputList =
    -- Simply removing stationary points fixes many problems.
    let
        helper inputs outputs =
            -- Conses non-stationary points on to outputs.
            -- Note that outputs therefore also has last point at its head.
            case ( inputs, outputs ) of
                ( [], _ ) ->
                    outputs

                ( firstInput :: moreInputs, [] ) ->
                    helper moreInputs [ firstInput ]

                ( finalInput :: [], previousOutput :: _ ) ->
                    if areSame finalInput previousOutput then
                        outputs

                    else
                        finalInput :: outputs

                ( someInput :: moreInputs, previousOutput :: _ ) ->
                    if areSame someInput previousOutput then
                        helper moreInputs outputs

                    else
                        helper moreInputs (someInput :: outputs)
    in
    List.reverse <| helper inputList []


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


elide : List a -> List a
elide input =
    -- Fold is essential  for performance.
    -- Two passes here means we get the list back the right way round.
    let
        helper : List a -> List a -> List a
        helper accum source =
            case source of
                aa :: _ :: cc ->
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
