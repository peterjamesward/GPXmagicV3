module Tools.MoveScaleRotateOptions exposing (..)

import Angle exposing (Angle)
import Length


type alias Options =
    { rotateAngle : Angle
    , desiredTrackLength : Length.Length
    }
