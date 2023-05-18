module Tools.Annealing exposing
    ( Msg(..)
    , apply
    , defaultOptions
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import CommonToolStyles exposing (noTrackMessage)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters)
import SystemSettings exposing (SystemSettings)
import Tools.AnnealingOptions exposing (..)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (..)


toolId =
    "annealing"


defaultOptions : Options
defaultOptions =
    { weightSamePosition = 1.0
    , weightSameAltitude = 1.0
    , weightSameGradient = 1.0
    , weightSameDirection = 1.0
    , weightDirectionDelta = 1.0
    , weightMinRadius = 1.0
    , weightMaxGradientDelta = 1.0
    , weightMaxGradient = 1.0
    , minRadius = Length.meters 5
    , maxGradient = 15
    , maxDeltaGradient = 1
    }


type Msg
    = Search
    | Apply
    | StopSearching


apply : Options -> TrackLoaded msg -> TrackLoaded msg
apply options track =
    let
        newTree =
            Just track.trackTree
    in
    case newTree of
        Just isTree ->
            let
                pointerReposition =
                    DomainModel.preserveDistanceFromStart track.trackTree isTree

                ( newOrange, newPurple ) =
                    ( pointerReposition track.currentPosition
                    , Maybe.map pointerReposition track.markerPosition
                    )
            in
            { track
                | trackTree = Maybe.withDefault track.trackTree newTree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            let
                newOptions =
                    options
            in
            ( newOptions, [] )

        _ ->
            ( options, [] )


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        Apply ->
            ( options, [] )

        Search ->
            ( options, [] )

        StopSearching ->
            ( options, [] )


view :
    SystemSettings
    -> (Msg -> msg)
    -> Options
    -> Maybe (TrackLoaded msg)
    -> Element msg
view settings wrapper options track =
    let
        i18n =
            I18N.text settings.location toolId

        searchButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Search
                , label = i18n "search"
                }
    in
    case track of
        Just _ ->
            column (CommonToolStyles.toolContentBoxStyle settings)
                [ el [ centerX ] <| searchButton
                ]

        Nothing ->
            noTrackMessage settings


type
    PointContext
    -- We need four road sections to derive all the factors impacted by a single point move.
    -- These types could be used in a fold over the track to get whole-track scores, similar to DirectionChanges.
    -- Note that start and end of route (or section of route) are interesting.
    = IsFirstPoint Int RoadSection RoadSection -- can only look forwards
    | IsSecondPoint Int RoadSection RoadSection RoadSection -- can look back one
    | IsGeneral Int RoadSection RoadSection RoadSection RoadSection -- not near either end
    | IsPenultimate Int RoadSection RoadSection RoadSection -- Only one forward
    | IsUltimate Int RoadSection RoadSection -- At end of route section


scoreFromContext : Options -> PointContext -> Float
scoreFromContext options context =
    -- Note we don't even attempt SA without sufficient points to play with.
    case context of
        IsFirstPoint index f1 f2 ->
            0

        IsSecondPoint index b1 f1 f2 ->
            0

        IsGeneral index b2 b1 f1 f2 ->
            0

        IsPenultimate index b2 b1 f1 ->
            0

        IsUltimate index b2 b1 ->
            0


pointContextFromIndex : Int -> TrackLoaded msg -> PointContext
pointContextFromIndex indexOfMovablePoint track =
    -- Please don't use this for folding over the track.
    if indexOfMovablePoint <= 0 then
        IsFirstPoint 0
            (DomainModel.leafFromIndex 0 track)
            (DomainModel.leafFromIndex 1 track)

    else if indexOfMovablePoint == 1 then
        IsSecondPoint 1
            (DomainModel.leafFromIndex 0 track)
            (DomainModel.leafFromIndex 1 track)
            (DomainModel.leafFromIndex 2 track)

    else if indexOfMovablePoint == DomainModel.skipCount track.trackTree - 1 then
        --TODO: Return partial context near track ends.
        IsPenultimate indexOfMovablePoint
            (DomainModel.leafFromIndex <| (indexOfMovablePoint - 2) track)
            (DomainModel.leafFromIndex <| (indexOfMovablePoint - 1) track)
            (DomainModel.leafFromIndex <| (indexOfMovablePoint + 0) track)

    else if indexOfMovablePoint >= DomainModel.skipCount track.trackTree then
        --TODO: Return partial context near track ends.
        IsUltimate indexOfMovablePoint
            (DomainModel.leafFromIndex <| (indexOfMovablePoint - 2) track)
            (DomainModel.leafFromIndex <| (indexOfMovablePoint - 1) track)

    else
        IsGeneral
            (DomainModel.leafFromIndex <| (indexOfMovablePoint - 2) track)
            (DomainModel.leafFromIndex <| (indexOfMovablePoint - 1) track)
            (DomainModel.leafFromIndex <| (indexOfMovablePoint + 0) track)
            (DomainModel.leafFromIndex <| (indexOfMovablePoint + 1) track)


deltaScoreForMovingPoint : Int -> Vector3d Meters LocalCoords -> TrackLoaded msg -> Options -> Float
deltaScoreForMovingPoint pointIndex moveVector track options =
    -- Don't worry, yet, about computing base score many times.
    case pointContextFromIndex pointIndex track of
        Just (GotFour back2 back1 forward0 forward1) ->
            -- Get some from leaves, some from points, some we need to compute.
            let
                baseScore =
                    scoreFromContext options back2 back1 forward0 forward1

                perturbedPoint =
                    forward0.startPoint.space |> Point3d.translateBy moveVector

                perturbedAsGpx =
                    DomainModel.gpxFromPointWithReference track.referenceLonLat perturbedPoint

                newRouteletteGpx =
                    [ Tuple.first back2.sourceData
                    , Tuple.first back1.sourceData
                    , perturbedAsGpx
                    , Tuple.second forward0.sourceData
                    , Tuple.second forward1.sourceData
                    ]

                miniTree =
                    -- Cunningly make a minimal tree from perturbed inputs.
                    DomainModel.treeFromSourcesWithExistingReference
                        track.referenceLonLat
                        newRouteletteGpx

                contextAfterPerturbation =
                    pointContextFromIndex 2 { track | trackTree = miniTree }
            in
            case contextAfterPerturbation of
                Just (GotFour newBack2 newBack1 newForward0 newForward1) ->
                    scoreFromContext options newBack2 newBack1 newForward0 newForward1 - baseScore

                _ ->
                    baseScore

        _ ->
            0
