module Tools.TimestampOptions exposing (..)

import Duration exposing (Duration)
import Mass exposing (Mass)
import Power exposing (Power, Watts)
import Speed exposing (Speed)


type alias Options =
    { extent : ExtentOption
    , desiredStartMillis : Int
    , desiredTickIntervalMillis : Int
    , endLockedToStart : Bool
    , steadyPower : Power
    , maxDownhill : Speed
    , mass : Mass
    , estimatedDuration : Duration
    , mode : TimestampMode
    }


type TimestampMode
    = Actual
    | Estimated


type ExtentOption
    = ExtentMarkers
    | ExtentOrangeToEnd
