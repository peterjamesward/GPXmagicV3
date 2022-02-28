module Tools.StartFinishTypes exposing (..)

import Length
import PreviewData exposing (PreviewPoint)


type Loopiness
    = NotALoop Length.Length
    | IsALoop
    | AlmostLoop Length.Length -- if, say, less than 200m back to start.


type alias Options =
    { loopiness : Loopiness
    , pointsToClose : List PreviewPoint
    }


type alias ClosingInfo =
    { originalEnd : Int
    , pointWasAdded : Bool
    , willClose : Bool
    }
