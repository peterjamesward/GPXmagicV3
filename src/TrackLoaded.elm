module TrackLoaded exposing (..)

import DomainModel exposing (GPXSource, PeteTree, skipCount)


type alias TrackLoaded =
    { currentPosition : Int
    , markerPosition : Maybe Int
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , trackTree : PeteTree
    }


getRangeFromMarkers : TrackLoaded -> ( Int, Int )
getRangeFromMarkers track =
    let
        theLength =
            skipCount track.trackTree
    in
    case track.markerPosition of
        Just purple ->
            ( min track.currentPosition purple
            , min (theLength - track.currentPosition) (theLength - purple)
            )

        Nothing ->
            ( track.currentPosition, theLength - track.currentPosition )
