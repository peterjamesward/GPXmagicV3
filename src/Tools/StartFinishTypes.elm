module Tools.StartFinishTypes exposing (Loopiness(..), Options)

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
