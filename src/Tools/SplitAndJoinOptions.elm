module Tools.SplitAndJoinOptions exposing (..)

import Length


type alias Options =
    { splitLimit : Length.Length
    , addBuffers : Bool
    , applyAutofix : Bool
    }
