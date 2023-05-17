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
