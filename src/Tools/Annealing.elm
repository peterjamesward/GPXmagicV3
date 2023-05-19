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
import Direction2d exposing (Direction2d, random)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Random
import SystemSettings exposing (SystemSettings)
import Tools.AnnealingOptions exposing (..)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (..)
import Vector3d exposing (..)
import ViewPureStyles exposing (..)


toolId =
    "annealing"


defaultOptions : Options msg
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
    , saTrack = Nothing
    , iterationsToRun = 1000
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
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


type Msg
    = Search
    | Apply
    | StopSearching
    | Perturb Perturbation
    | Tick


apply : Options msg -> TrackLoaded msg -> TrackLoaded msg
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
    -> Options msg
    -> Maybe (TrackLoaded msg)
    -> ( Options msg, List (ToolAction msg) )
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
    -> Options msg
    -> Element.Color
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options msg, List (ToolAction msg) )
update msg options previewColour track wrapper =
    let
        requestPerturbation =
            ExternalCommand <|
                Random.generate
                    (wrapper << Perturb)
                    (randomMove (DomainModel.skipCount track.trackTree))
    in
    case msg of
        Apply ->
            ( options, [] )

        Search ->
            ( { options
                | saTrack = Just track
                , searching = True
              }
            , [ requestPerturbation ]
            )

        StopSearching ->
            ( { options | searching = False }
            , []
            )

        Perturb perturbation ->
            ( { options
                | currentIndex = perturbation.pointIndex
                , lastPerturbation = Just perturbation
              }
            , [ DelayMessage 10 (wrapper Tick) ]
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
    -> Options msg
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

        labels =
            [ "pointIndex"
            , "direction"
            , "distance"
            , "altitude"
            , "p"
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
                [ el [ centerX ] <|
                    if options.searching then
                        stopButton

                    else
                        searchButton
                , showLastPerturbation
                ]

        Nothing ->
            noTrackMessage settings


saOneMove : Options msg -> Int -> Vector3d Meters LocalCoords -> TrackLoaded msg -> TrackLoaded msg
saOneMove options pointIndex displacement baseTrack =
    {-
       We have a clone of the track (tree) that we update piecemeal in our Options to endure over several cycles & updates.
       Will use in-situ tree updates and should be easy enough to derive scores based on metrics from baseline.
       Locally assess impact of each perturbation (at worst extends two points each side).
    -}
    baseTrack
