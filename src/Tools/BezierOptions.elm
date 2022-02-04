module Tools.BezierOptions exposing (..)


type alias Options =
    { bezierTension : Float
    , bezierTolerance : Float
    , bezierStyle : BezierStyle
    }


type BezierStyle
    = ThroughExisting
    | Approximated
