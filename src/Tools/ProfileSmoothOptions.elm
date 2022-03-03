module Tools.ProfileSmoothOptions exposing (..)

import DomainModel exposing (PeteTree)
import Length


type alias Options =
    { maximumAscent : Float
    , maximumDescent : Float
    , extent : ExtentOption
    , previewData : Maybe PeteTree
    }



type ExtentOption
    = ExtentIsRange
    | ExtentIsTrack
