module Tools.CentroidAverageOptions exposing (Extent(..), Options)

-- Putting these in a separate module means we can use with Action, without an import loop.


type alias Options =
    { weighting : Float
    , applyToAltitude : Bool
    , applyToPosition : Bool
    }


type Extent
    = ExtentTrack
