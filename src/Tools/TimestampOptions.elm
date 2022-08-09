module Tools.TimestampOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Time


type alias Options =
    { extent : ExtentOption
    , desiredRangeStartOffsetSeconds : Int
    , startMilliseconds : Int
    , desiredRangeEndOffsetSeconds : Int
    , endMilliseconds : Int
    , desiredTickIntervalMillis : Int
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
