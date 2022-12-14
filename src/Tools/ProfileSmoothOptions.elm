module Tools.ProfileSmoothOptions exposing (ExtentOption(..), Options, SmoothMethod(..))

import DomainModel exposing (PeteTree)


type alias Options =
    { extent : ExtentOption
    , previewData : Maybe PeteTree
    , smoothMethod : SmoothMethod
    , maximumAscent : Float
    , maximumDescent : Float
    , windowSize : Int
    , limitRedistributes : Bool
    , bumpiness : Float
    }


type SmoothMethod
    = MethodLimit
    | MethodGradients
    | MethodAltitudes
    | MethodUniform -- v1, v2 gradient smoother


type ExtentOption
    = ExtentIsRange
