module TrackLoaded exposing (..)

import DomainModel exposing (GPXSource, PeteTree)


type alias TrackLoaded =
    { currentPosition : Int
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , trackTree : PeteTree
    }
