module Tools.MoveScaleRotateOptions exposing (Options)

import Angle exposing (Angle)
import Length


type alias Options =
    { rotateAngle : Angle
    , desiredTrackLength : Length.Length
    }
