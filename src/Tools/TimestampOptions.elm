module Tools.TimestampOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Time


type alias Options =
    { extent : ExtentOption
    , desiredRangeStartOffset : Time.Posix
    , startMilliseconds : Int
    , desiredRangeEndOffset : Time.Posix
    , endMilliseconds : Int
    , desiredTickInterval : Time.Posix
    , stretchTimes : Bool
    , precision : SliderPrecision
    }


type ExtentOption
    = ExtentMarkers
    | ExtentOrangeToEnd


type SliderPrecision
    = SliderOneSecond
    | SliderHalfSecond
    | SliderMillisecond
