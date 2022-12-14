module LeafIndex exposing (LeafIndex, LeafIndexEntry)

import Length
import LocalCoords exposing (LocalCoords)
import SpatialIndex


type alias LeafIndexEntry =
    { leafIndex : Int }


type alias LeafIndex =
    SpatialIndex.SpatialNode LeafIndexEntry Length.Meters LocalCoords
