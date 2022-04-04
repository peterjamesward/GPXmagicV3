module TrackLoaded exposing (..)

import Actions exposing (ToolAction)
import DomainModel exposing (..)
import Json.Encode as E
import LandUseDataTypes
import Length exposing (inMeters)
import Point3d
import PreviewData exposing (PreviewPoint)
import Quantity


type alias TrackLoaded msg =
    { currentPosition : Int
    , markerPosition : Maybe Int
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , trackTree : PeteTree
    , trackName : Maybe String
    , undos : List (UndoEntry msg)
    , redos : List (UndoEntry msg)
    , lastMapClick : ( Float, Float )
    , landUseData : LandUseDataTypes.LandUseData
    }


type alias UndoEntry msg =
    { action : ToolAction msg
    , originalPoints : List GPXSource -- for reconstructing the original tree
    , fromStart : Int -- so we do not need to decode the action.
    , fromEnd : Int
    , currentPosition : Int
    , markerPosition : Maybe Int
    }


type alias Options msg =
    -- Do the stacks live here or in Model? That's a good one.
    { undos : List (UndoEntry msg)
    , redos : List (UndoEntry msg)
    }


newTrackFromTree : GPXSource -> PeteTree -> TrackLoaded msg
newTrackFromTree refLonLat newTree =
    { currentPosition = 0
    , markerPosition = Nothing
    , referenceLonLat = refLonLat
    , renderDepth = 10
    , trackTree = newTree
    , trackName = Nothing
    , undos = []
    , redos = []
    , lastMapClick = ( 0, 0 )
    , landUseData = LandUseDataTypes.emptyLandUse
    }


removeAdjacentDuplicates : List GPXSource -> List GPXSource
removeAdjacentDuplicates gpxs =
    -- Just removing stationary points fixes so many problems.
    let
        areSame : GPXSource -> GPXSource -> Bool
        areSame a b =
            a.latitude == b.latitude && a.longitude == b.longitude

        helper inputs outputs =
            -- Conses non-stationary points on to outputs.
            -- Note that outputs therefore also has last point at its head.
            case ( inputs, outputs ) of
                ( [], _ ) ->
                    outputs

                ( firstInput :: moreInputs, [] ) ->
                    helper moreInputs [ firstInput ]

                ( finalInput :: [], previousOutput :: _ ) ->
                    if areSame finalInput previousOutput then
                        outputs

                    else
                        finalInput :: outputs

                ( someInput :: moreInputs, previousOutput :: _ ) ->
                    if areSame someInput previousOutput then
                        helper moreInputs outputs

                    else
                        helper moreInputs (someInput :: outputs)
    in
    List.reverse <| helper gpxs []


trackFromPoints : String -> List GPXSource -> Maybe (TrackLoaded msg)
trackFromPoints trackName gpxTrack =
    let
        landuse =
            LandUseDataTypes.emptyLandUse
    in
    case treeFromSourcePoints <| removeAdjacentDuplicates gpxTrack of
        Just aTree ->
            Just
                { trackTree = aTree
                , currentPosition = 0
                , markerPosition = Nothing
                , renderDepth = 10
                , referenceLonLat = DomainModel.gpxPointFromIndex 0 aTree
                , undos = []
                , redos = []
                , trackName = Just trackName
                , lastMapClick = ( 0, 0 )
                , landUseData = { landuse | status = LandUseDataTypes.LandUseWaitingOSM }
                }

        Nothing ->
            Nothing


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
            -- Hmm. Some want current point, some want whole track.
            -- This choice gives the offset of orange from either end.
            ( track.currentPosition, theLength - track.currentPosition )


type MarkerColour
    = Orange
    | Purple


addToUndoStack :
    ToolAction msg
    -> Int
    -> Int
    -> List GPXSource
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
            , currentPosition = oldTrack.currentPosition
            , markerPosition = oldTrack.markerPosition
            }
    in
    { oldTrack
        | undos = undoEntry :: oldTrack.undos
        , redos = []
    }


undoLastAction : TrackLoaded msg -> TrackLoaded msg
undoLastAction track =
    case track.undos of
        undo :: moreUndos ->
            let
                newTree =
                    DomainModel.replaceRange
                        undo.fromStart
                        undo.fromEnd
                        track.referenceLonLat
                        undo.originalPoints
                        track.trackTree
            in
            case newTree of
                Just isTree ->
                    { track
                        | undos = moreUndos
                        , redos = undo :: track.redos
                        , trackTree = isTree
                        , currentPosition = undo.currentPosition
                        , markerPosition = undo.markerPosition
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
        newLength =
            skipCount newTree

        firstMarker =
            whichMarkerIsNearestStart oldTrack

        changeInTrackLength =
            newLength - skipCount oldTrack.trackTree

        newOrange =
            clamp 0 newLength <|
                case firstMarker of
                    Orange ->
                        oldTrack.currentPosition

                    Purple ->
                        max 0 <| oldTrack.currentPosition + changeInTrackLength

        newPurple =
            case ( oldTrack.markerPosition, firstMarker ) of
                ( Just purple, Orange ) ->
                    Just <| clamp 0 newLength <| purple + changeInTrackLength

                ( Just purple, Purple ) ->
                    Just <| clamp 0 newLength purple

                _ ->
                    Nothing
    in
    { oldTrack
        | trackTree = newTree
        , currentPosition = newOrange
        , markerPosition = newPurple
    }



-- Rendering a track for output, whether GPX or JSON.
-- Hmm. We need a context-aware tree traversal to actually give us a list of distances from start.
-- Let's put the traversal in the domain model, and simple JSON output here.
-- We need for GPX: lon, lat, alt; for profile JSON: distance, alt, gradient.
-- In both cases, it's point-based, not RoadSection.
-- Welcome back, TrackPoint as a hybrid of EarthPoint and GPXSource.


profilePointEncode : TrackPoint -> E.Value
profilePointEncode tp =
    E.object
        [ ( "distance", E.float <| inMeters tp.distanceFromStart )
        , ( "altitude", E.float <| inMeters tp.altitude )
        , ( "gradient", E.float tp.gradient )
        ]


jsonProfileData : TrackLoaded msg -> String
jsonProfileData track =
    -- Get profile data from domain model
    -- Make JSON
    -- Convert to string
    let
        points =
            List.reverse <|
                DomainModel.trackPointsForOutput track.trackTree

        json =
            E.list identity <| List.map profilePointEncode points

        output =
            E.encode 0 json
    in
    -- Nicely indented, why not.
    output


getAsPreviewPoint : PeteTree -> Int -> PreviewPoint
getAsPreviewPoint tree index =
    { earthPoint = earthPointFromIndex index tree
    , gpx = gpxPointFromIndex index tree
    }


buildPreview : List Int -> PeteTree -> List PreviewPoint
buildPreview indices tree =
    -- Helper for tool that need to highlight a non-contiguous set of points.
    List.map (getAsPreviewPoint tree) indices


previewFromTree : PeteTree -> Int -> Int -> Int -> List PreviewPoint
previewFromTree tree start end depthLimit =
    let
        endDistance =
            distanceFromIndex end tree

        internalFoldFn :
            RoadSection
            -> ( Length.Length, List PreviewPoint )
            -> ( Length.Length, List PreviewPoint )
        internalFoldFn road ( descendingDistance, accum ) =
            ( descendingDistance |> Quantity.minus road.trueLength
            , { earthPoint = road.endPoint
              , gpx = Tuple.second road.sourceData
              }
                :: accum
            )

        ( _, endPoints ) =
            --foldOverRouteRL internalFoldFn tree ( endDistance, [] )
            foldOverRouteRLwithDepthLimit depthLimit internalFoldFn tree ( endDistance, [] )
    in
    getAsPreviewPoint tree 0 :: endPoints


asPreviewPoints : TrackLoaded msg -> Length.Length -> List EarthPoint -> List PreviewPoint
asPreviewPoints track startDistance earths =
    let
        foldFn earth ( distance, mLastGpx, outputs ) =
            let
                thisGpx =
                    DomainModel.gpxFromPointWithReference track.referenceLonLat earth

                thisDistance =
                    case mLastGpx of
                        Just lastGpx ->
                            DomainModel.gpxDistance lastGpx thisGpx

                        Nothing ->
                            -- Avoid divide by zero
                            Length.centimeter

                thisPreview =
                    { earthPoint = earth
                    , gpx = thisGpx
                    }
            in
            ( distance |> Quantity.plus thisDistance
            , Just thisGpx
            , thisPreview :: outputs
            )

        ( _, _, reversed ) =
            List.foldl foldFn ( startDistance, Nothing, [] ) earths
    in
    List.reverse reversed


adjustAltitude : Length.Length -> EarthPoint -> EarthPoint
adjustAltitude alt pt =
    Point3d.xyz
        (Point3d.xCoordinate pt)
        (Point3d.yCoordinate pt)
        alt
