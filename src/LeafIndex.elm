module LeafIndex exposing (LeafIndex, LeafIndexEntry)

import Length
import LocalCoords exposing (LocalCoords)
import SpatialIndex



--Note this is local to track and is used primarily for click detection.
--There is a global (pan-track) leaf index in the Graph module also.
--Don't confuse them.


type alias LeafIndexEntry =
    { leafIndex : Int }


type alias LeafIndex =
    SpatialIndex.SpatialNode LeafIndexEntry Length.Meters LocalCoords
