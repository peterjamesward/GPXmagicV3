module Tools.Tracks exposing
    ( Msg(..)
    , Options
    , addTrack
    , defaultOptions
    , getActiveTrack
    , setTrack
    , toolId
    , update
    , updateActiveTrack
    , view
    )

import Actions
import Angle
import CommonToolStyles
import Direction2d
import DomainModel exposing (GPXSource)
import Element exposing (..)
import Element.Input as Input
import FeatherIcons
import List.Extra
import Quantity
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (useIcon)


toolId =
    "tracks"


type alias Options msg =
    --TODO: This will develop, taking track stuff from Main, and perhaps subsume Graph.
    { nextTrackNumber : Int
    , tracks : List (TrackLoaded msg)
    , activeTrackIndex : Maybe Int
    , commonReferenceGPX : Maybe GPXSource -- from where we derive (X,Y) by map projection.
    }


defaultOptions : Options msg
defaultOptions =
    { nextTrackNumber = 1
    , tracks = []
    , activeTrackIndex = Nothing
    , commonReferenceGPX = Nothing
    }


type Msg
    = SelectActiveTrack Int


update : Msg -> Options msg -> ( Options msg, List (Actions.ToolAction msg) )
update msg options =
    case msg of
        SelectActiveTrack index ->
            ( options, [ Actions.SetActiveTrack index ] )


updateActiveTrack : TrackLoaded msg -> Options msg -> ( TrackLoaded msg, Options msg )
updateActiveTrack newTrack options =
    case options.activeTrackIndex of
        Just index ->
            ( newTrack
            , { options
                | tracks =
                    List.Extra.setAt index newTrack options.tracks
              }
            )

        Nothing ->
            ( newTrack, options )


view : SystemSettings -> (Msg -> msg) -> Options msg -> Element msg
view settings wrapper options =
    let
        helper =
            I18N.text settings.location toolId
    in
    el (CommonToolStyles.toolContentBoxStyle settings) <|
        column [ spacing 5 ] <|
            List.indexedMap
                (\index entry ->
                    displayTrackInfo index entry wrapper
                )
                options.tracks


displayTrackInfo : Int -> TrackLoaded msg -> (Msg -> msg) -> Element msg
displayTrackInfo index track wrapper =
    row [ spacing 5 ]
        [ Input.button
            []
            { label = useIcon FeatherIcons.edit
            , onPress = Just <| wrapper (SelectActiveTrack index)
            }
        , text track.trackName
        ]


addTrack : TrackLoaded msg -> Options msg -> Options msg
addTrack track options =
    --If this is not the first track, we must adjust its reference point.
    --That may be inefficient but we can absorb the cost at load time.
    --If not, we (I) will have to change it.
    let
        unambiguousName =
            case
                List.Extra.find
                    (\t -> t.trackName == track.trackName)
                    options.tracks
            of
                Just _ ->
                    track.trackName ++ "-" ++ String.fromInt options.nextTrackNumber

                Nothing ->
                    track.trackName

        trackWithCommonReference =
            case options.commonReferenceGPX of
                Just commonReference ->
                    TrackLoaded.changeReferencePoint commonReference track

                Nothing ->
                    track
    in
    { options
        | tracks = { track | trackName = unambiguousName } :: options.tracks
        , nextTrackNumber = options.nextTrackNumber + 1
        , activeTrackIndex = Just 0
        , commonReferenceGPX =
            case options.commonReferenceGPX of
                Just common ->
                    Just common

                Nothing ->
                    Just <| TrackLoaded.getReferencePoint track
    }


setTrack : Int -> Options msg -> ( Maybe (TrackLoaded msg), Options msg )
setTrack index options =
    ( List.Extra.getAt index options.tracks
    , { options | activeTrackIndex = Just index }
    )


getActiveTrack : Options msg -> Maybe (TrackLoaded msg)
getActiveTrack options =
    case options.activeTrackIndex of
        Just index ->
            List.Extra.getAt index options.tracks

        Nothing ->
            Nothing
