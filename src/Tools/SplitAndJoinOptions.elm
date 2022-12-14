module Tools.SplitAndJoinOptions exposing (Options)

import Length


type alias Options =
    { splitLimit : Length.Length
    , addBuffers : Bool
    , applyAutofix : Bool
    }
