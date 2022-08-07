module Tools.TimestampOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Time


type alias Options =
    { extent : ExtentOption
    , targetTime : Time.Posix
    }


type ExtentOption
    = ExtentIsRange
    | ExtentIsTrack
