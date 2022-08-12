module Tools.TimestampOptions exposing (..)

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
    }


type ExtentOption
    = ExtentMarkers
    | ExtentOrangeToEnd
