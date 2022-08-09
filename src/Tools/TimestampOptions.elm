module Tools.TimestampOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.


type alias Options =
    { extent : ExtentOption
    , desiredStartMillis : Int
    , desiredTickIntervalMillis : Int
    , endLockedToStart : Bool
    }


type ExtentOption
    = ExtentMarkers
    | ExtentOrangeToEnd
