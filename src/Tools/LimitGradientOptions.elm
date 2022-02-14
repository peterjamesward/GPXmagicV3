module Tools.LimitGradientOptions exposing (..)


type alias Options =
    { maximumAscent : Float
    , maximumDescent : Float
    , extent : ExtentOption
    }


type ExtentOption
    = ExtentIsRange
    | ExtentIsTrack
