module Tools.TreeSmootherOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import PreviewData exposing (PreviewPoint)


type alias Options =
    { depth : Int
    , mode : SmoothMode
    }


type SmoothMode
    = Bezier
    | Clothoid


type alias SmoothedBend =
    { nodes : List PreviewPoint
    , startIndex : Int -- Lead-in node that is NOT to be replaced
    , endIndex : Int -- ... and lead-out, not to be replaced.
    }
