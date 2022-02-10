module Tools.BezierOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.


type alias Options =
    { bezierTension : Float
    , bezierTolerance : Float
    , bezierStyle : BezierStyle
    , extent : ExtentOption
    }


type BezierStyle
    = ThroughExisting
    | Approximated


type ExtentOption
    = ExtentIsRange
    | ExtentIsTrack
