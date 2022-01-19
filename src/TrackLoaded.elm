module TrackLoaded exposing (..)

import Actions exposing (ToolAction)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, skipCount)


type alias TrackLoaded msg =
    { currentPosition : Int
    , markerPosition : Maybe Int
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , trackTree : PeteTree

    -- Experimental placement of undo/redo stacks.
    , undos : List (UndoEntry msg)
    , redos : List (UndoEntry msg)
    }


type alias UndoEntry msg =
    { action : ToolAction msg
    , originalPoints : List ( EarthPoint, GPXSource ) -- for reconstructing the original tree
    , fromStart : Int -- so we do not need to decode the action.
    , fromEnd : Int
    }


type alias Options msg =
    -- Do the stacks live here or in Model? That's a good one.
    { undos : List (UndoEntry msg)
    , redos : List (UndoEntry msg)
    }


getRangeFromMarkers : TrackLoaded msg -> ( Int, Int )
getRangeFromMarkers track =
    -- This helps all tools consistently to get `fromStart, fromEnd`
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


type MarkerColour
    = Orange
    | Purple


addToUndoStack :
    ToolAction msg
    -> Int
    -> Int
    -> List ( EarthPoint, GPXSource )
    -> TrackLoaded msg
    -> TrackLoaded msg
addToUndoStack action fromStart fromEnd oldPoints oldTrack =
    let
        undoEntry : UndoEntry msg
        undoEntry =
            { action = action
            , originalPoints = oldPoints
            , fromStart = fromStart
            , fromEnd = fromEnd
            }
    in
    { oldTrack
        | undos = undoEntry :: oldTrack.undos
    }


undoLastAction : TrackLoaded msg -> TrackLoaded msg
undoLastAction track =
    -- For now, just pop the stack
    --TODO: Stitch in the old points!!
    case track.undos of
        undo :: moreUndos ->
            let
                _ =
                    Debug.log "UNDO" undo.action

                newTree =
                    --TODO: Rewrite me. Properly. Clearly. Not hideously. Thanks.
            in
            case newTree of
                Just isTree ->
                    { track
                        | undos = moreUndos
                        , redos = undo :: track.redos
                        , trackTree = isTree
                    }

                Nothing ->
                    track

        _ ->
            track


whichMarkerIsNearestStart : TrackLoaded msg -> MarkerColour
whichMarkerIsNearestStart track =
    case track.markerPosition of
        Just purple ->
            if track.currentPosition <= purple then
                Orange

            else
                Purple

        Nothing ->
            Orange


useTreeWithRepositionedMarkers : Maybe PeteTree -> TrackLoaded msg -> TrackLoaded msg
useTreeWithRepositionedMarkers mTree oldTrack =
    case mTree of
        Just newTree ->
            internalUseTree newTree oldTrack

        Nothing ->
            oldTrack


internalUseTree : PeteTree -> TrackLoaded msg -> TrackLoaded msg
internalUseTree newTree oldTrack =
    let
        firstMarker =
            whichMarkerIsNearestStart oldTrack

        changeInTrackLength =
            skipCount newTree - skipCount oldTrack.trackTree

        newOrange =
            case firstMarker of
                Orange ->
                    oldTrack.currentPosition

                Purple ->
                    max 0 <| oldTrack.currentPosition + changeInTrackLength

        newPurple =
            case ( oldTrack.markerPosition, firstMarker ) of
                ( Just purple, Orange ) ->
                    Just <| max 0 <| purple + changeInTrackLength

                ( Just purple, Purple ) ->
                    Just purple

                _ ->
                    Nothing
    in
    { oldTrack
        | trackTree = newTree
        , currentPosition = newOrange
        , markerPosition = newPurple
    }
