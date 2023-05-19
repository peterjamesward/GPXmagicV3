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
import Angle
import CommonToolStyles exposing (noTrackMessage)
import Direction2d exposing (Direction2d, random)
import Direction3d exposing (..)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (translateBy)
import PreviewData exposing (..)
import Quantity
import Random
import Random.Float
import SketchPlane3d exposing (..)
import SystemSettings exposing (SystemSettings)
import Tools.AnnealingOptions exposing (..)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (..)
import Vector2d exposing (..)
import Vector3d exposing (..)
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
    , currentTrack = Nothing
    , iterationsToRun = 1000
    , maxIterations = 10000
    , scoreHistory = []
    , currentIndex = 0
    , searching = False
    , lastPerturbation = Nothing
    }


randomMove : Int -> Random.Generator Perturbation
randomMove maxPoint =
    Random.map5
        Perturbation
        (Random.int 0 maxPoint)
        Direction2d.random
        Random.Float.standardNormal
        Random.Float.standardNormal
        (Random.float 0 1)


type Msg
    = Search
    | Apply
    | StopSearching
    | Perturb Perturbation
    | Tick


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


apply :
    Options
    -> TrackLoaded msg
    -> TrackLoaded msg
apply options track =
    let
        ( fromStart, fromEnd ) =
            --TODO: Allow for use over range.
            if track.markerPosition /= Nothing then
                TrackLoaded.getRangeFromMarkers track

            else
                ( 0, 0 )
    in
    let
        pointerReposition =
            identity

        ( newOrange, newPurple ) =
            ( pointerReposition track.currentPosition
            , Maybe.map pointerReposition track.markerPosition
            )
    in
    case options.currentTrack of
        Just saTrack ->
            { track
                | trackTree = saTrack.tree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves saTrack.tree
            }

        Nothing ->
            track


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track wrapper =
    let
        requestPerturbation =
            ExternalCommand <|
                Random.generate
                    (wrapper << Perturb)
                    (randomMove (DomainModel.skipCount track.trackTree))

        previewFromDual ( earth, gpx ) =
            PreviewPoint earth gpx

        preview latest =
            case latest of
                Just something ->
                    ShowPreview
                        { tag = toolId
                        , shape = PreviewCircle
                        , colour = previewColour
                        , points = List.map previewFromDual <| DomainModel.extractPointsInRange 0 0 something.tree
                        }

                Nothing ->
                    Actions.NoAction
    in
    case msg of
        Apply ->
            ( { options | searching = False }
            , [ WithUndo Actions.AnnealingApply
              , Actions.AnnealingApply
              , TrackHasChanged
              ]
            )

        Search ->
            let
                newOptions =
                    { options
                        | currentTrack = Just { tree = track.trackTree, reference = track.referenceLonLat }
                        , searching = True
                        , iterationsToRun = options.maxIterations
                    }
            in
            ( newOptions
            , [ requestPerturbation ]
            )

        StopSearching ->
            ( { options | searching = False }
            , []
            )

        Perturb perturbation ->
            let
                newOptions =
                    { options
                        | currentIndex = perturbation.pointIndex
                        , lastPerturbation = Just perturbation
                        , currentTrack =
                            Maybe.map
                                (applyPerturbationRegardless options track.trackTree perturbation)
                                options.currentTrack
                        , iterationsToRun = options.iterationsToRun - 1
                        , searching = options.iterationsToRun > 1
                    }
            in
            ( newOptions
            , [ preview newOptions.currentTrack
              , DelayMessage 1 (wrapper Tick)
              ]
            )

        Tick ->
            ( options
            , if options.searching then
                [ requestPerturbation ]

              else
                []
            )


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

        stopButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper StopSearching
                , label = i18n "stop"
                }

        applyButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Apply
                , label = i18n "adopt"
                }

        labels =
            [ "pointIndex"
            , "direction"
            , "distance"
            , "altitude"
            , "probably"
            ]

        accessors : List (Perturbation -> String)
        accessors =
            [ .pointIndex >> String.fromInt
            , .direction >> Direction2d.toAngle >> UtilsForViews.showAngle
            , .distance >> UtilsForViews.showDecimal2
            , .altitude >> UtilsForViews.showDecimal2
            , .p >> UtilsForViews.showDecimal2
            ]

        accessing : Perturbation -> (Perturbation -> String) -> String
        accessing thing with =
            with thing

        showLastPerturbation =
            case options.lastPerturbation of
                Just perturb ->
                    row [ spacing 5, padding 5 ]
                        [ column [ spacing 5 ] <| List.map text labels
                        , column [ spacing 5 ] <| List.map (text << accessing perturb) accessors
                        ]

                Nothing ->
                    none
    in
    case track of
        Just _ ->
            column (CommonToolStyles.toolContentBoxStyle settings)
                [ el [ centerX ] <| text <| String.fromInt options.iterationsToRun
                , el [ centerX ] <|
                    if options.searching then
                        stopButton

                    else
                        searchButton
                , showLastPerturbation
                , applyButton
                ]

        Nothing ->
            noTrackMessage settings


applyPerturbationRegardless : Options -> PeteTree -> Perturbation -> MinimalTrack -> MinimalTrack
applyPerturbationRegardless options baselineTree perturbation currentTrack =
    {-
       SA requires that we assess the impact of each perturbation.
       There are variations; no easy way to know which is best for any situation.
       This is my plan.
       Find current score for target point and any immediate neighbours.
       Find new score for these points.
       If new score is lower than the current track, accept the perturbation.
       When new score is higher, accept if the `p` value is less than `temperature`,
       where temperature is `iterationsToRun / maxIterations'.
       ** Should we be more likely to accept smaller changes??
    -}
    let
        vector =
            Vector2d.withLength (Length.meters perturbation.distance) perturbation.direction
                |> Vector3d.on SketchPlane3d.xy
                |> Vector3d.plus
                    (Vector3d.withLength (Length.meters perturbation.altitude) Direction3d.z)

        basePoint =
            DomainModel.earthPointFromIndex perturbation.pointIndex currentTrack.tree

        newPoint =
            { basePoint | space = basePoint.space |> Point3d.translateBy vector }

        proposedNewTree =
            DomainModel.updateEarthPointByIndexInSitu
                perturbation.pointIndex
                newPoint
                currentTrack.reference
                currentTrack.tree

        pointScoreInCurrentTree =
            scorePoint perturbation.pointIndex options baselineTree currentTrack.tree

        pointScoreInProposedTree =
            scorePoint perturbation.pointIndex options baselineTree proposedNewTree

        lowerScore =
            pointScoreInProposedTree < pointScoreInCurrentTree

        allowHigherScore =
            toFloat options.iterationsToRun / toFloat options.maxIterations

        acceptProposal =
            lowerScore || perturbation.p < allowHigherScore
    in
    if acceptProposal then
        { currentTrack | tree = proposedNewTree }

    else
        currentTrack


scorePoint : Int -> Options -> PeteTree -> PeteTree -> Float
scorePoint index options baselineTree currentTree =
    {-
       Can delay this no longer. Score for track is sum of all point scores.
       High scores are worse in this game.
       Without weightings, components of point score are:
       * Scalar distance from baseline;
       * Deviation from baseline direction;
       * Deviation from baseline gradient;
       * Extent to which gradient exceeds threshold;
       * Extent to which gradient change at point exceeds threshold;
       * Extent to which curvature at point exceeds threshold;
       There is no more we can do at one point.
       Note that a perturbation affects three points (except at boundary).
       To be clear, `baseline` is the original, not the latest, track.
    -}
    let
        ( baselineLeafFromPoint, currentLeafFromPoint ) =
            ( DomainModel.leafFromIndex index baselineTree |> DomainModel.asRecord
            , DomainModel.leafFromIndex index currentTree |> DomainModel.asRecord
            )

        ( baselinePriorLeaf, currentPriorLeaf ) =
            ( DomainModel.leafFromIndex (index - 1) baselineTree |> DomainModel.asRecord
            , DomainModel.leafFromIndex (index - 1) currentTree |> DomainModel.asRecord
            )

        scalarShift =
            Length.inMeters <|
                Point3d.distanceFrom
                    baselineLeafFromPoint.startPoint.space
                    currentLeafFromPoint.startPoint.space

        directionDifference =
            Angle.inRadians <|
                Direction2d.angleFrom
                    baselineLeafFromPoint.directionAtStart
                    currentLeafFromPoint.directionAtStart

        gradientDifference =
            abs <| baselineLeafFromPoint.gradientAtStart - currentLeafFromPoint.gradientAtStart

        gradientExceedsThreshold =
            baselineLeafFromPoint.gradientAtStart - options.maxGradient

        gradientChangeAtPointAboveThreshold =
            (abs <| currentLeafFromPoint.gradientAtStart - currentPriorLeaf.gradientAtStart)
                - options.maxDeltaGradient

        effectiveDistance =
            Quantity.plus currentPriorLeaf.trueLength currentLeafFromPoint.trueLength
                |> Quantity.half
                |> Length.inMeters

        curvatureAtPoint =
            Direction2d.angleFrom currentPriorLeaf.directionAtStart currentLeafFromPoint.directionAtStart
                |> Angle.inRadians
                |> abs

        curvatureExceedingThreshold =
            curvatureAtPoint - (Length.inMeters options.minRadius * effectiveDistance)
    in
    scalarShift
        + directionDifference
        + gradientDifference
        + gradientExceedsThreshold
        + gradientChangeAtPointAboveThreshold
        + curvatureExceedingThreshold
