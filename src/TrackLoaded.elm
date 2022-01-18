module TrackLoaded exposing (..)

import DomainModel exposing (GPXSource, PeteTree)


type alias TrackLoaded =
    { currentPosition : Int
    , markerPosition : Maybe Int
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , trackTree : PeteTree
    }
