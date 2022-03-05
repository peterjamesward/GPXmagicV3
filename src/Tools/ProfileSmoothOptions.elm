module Tools.ProfileSmoothOptions exposing (..)

import DomainModel exposing (PeteTree)
import Length


type alias Options =
    { extent : ExtentOption
    , previewData : Maybe PeteTree
    , smoothMethod : SmoothMethod
    , maximumAscent : Float
    , maximumDescent : Float
    , processNoise : Float
    , measurementNoise : Float
    , useDeltaSlope : Bool
    , windowSize : Int
    , limitRedistributes : Bool
    }


type SmoothMethod
    = MethodLimit
    | MethodGradients
    | MethodAltitudes
    | MethodGaussian


type ExtentOption
    = ExtentIsRange
    | ExtentIsTrack
