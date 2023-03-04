module Tools.Tracks exposing
    ( Msg(..)
    , Options
    , addTrack
    , defaultOptions
    , getActiveTrack
    , mapOverInvisibleTracks
    , mapOverVisibleTracks
    , setTrack
    , toolId
    , update
    , updateActiveTrack
    , view
    )

import Actions
import CommonToolStyles
import DomainModel exposing (GPXSource)
import Element exposing (..)
import Element.Input as Input
import FeatherIcons
import List.Extra
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
    | ToggleVisibility Int


update : Msg -> Options msg -> ( Options msg, List (Actions.ToolAction msg) )
update msg options =
    case msg of
        SelectActiveTrack index ->
            ( options, [ Actions.SetActiveTrack index ] )

        ToggleVisibility index ->
            case List.Extra.getAt index options.tracks of
                Just found ->
                    let
                        updatedTrack =
                            { found | visible = not found.visible }
                    in
                    ( { options
                        | tracks =
                            List.Extra.updateAt
                                index
                                (always updatedTrack)
                                options.tracks
                      }
                    , [ Actions.SetActiveTrack <| Maybe.withDefault 0 options.activeTrackIndex ]
                    )

                Nothing ->
                    ( options, [] )


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
                    displayTrackInfo index entry wrapper options
                )
                options.tracks


displayTrackInfo : Int -> TrackLoaded msg -> (Msg -> msg) -> Options msg -> Element msg
displayTrackInfo index track wrapper options =
    row [ spacing 5 ]
        [ Input.button
            []
            { label = useIcon FeatherIcons.edit
            , onPress = Just <| wrapper (SelectActiveTrack index)
            }
        , if Just index == options.activeTrackIndex then
            -- Can't change visibility of active track.
            none

          else
            Input.button []
                { label =
                    useIcon <|
                        if track.visible then
                            FeatherIcons.eyeOff

                        else
                            FeatherIcons.eye
                , onPress = Just <| wrapper (ToggleVisibility index)
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
        | tracks = { trackWithCommonReference | trackName = unambiguousName } :: options.tracks
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
    case List.Extra.getAt index options.tracks of
        Just found ->
            let
                visibleTrack =
                    { found | visible = True }
            in
            ( Just visibleTrack
            , { options
                | activeTrackIndex = Just index
                , tracks = List.Extra.updateAt index (always visibleTrack) options.tracks
              }
            )

        Nothing ->
            ( Nothing, options )


getActiveTrack : Options msg -> Maybe (TrackLoaded msg)
getActiveTrack options =
    case options.activeTrackIndex of
        Just index ->
            List.Extra.getAt index options.tracks

        Nothing ->
            Nothing


mapOverVisibleTracks : (TrackLoaded msg -> Bool -> a) -> Options msg -> List a
mapOverVisibleTracks f options =
    options.tracks
        |> List.filter .visible
        |> List.indexedMap
            (\i track -> f track (Just i == options.activeTrackIndex))


mapOverInvisibleTracks : (TrackLoaded msg -> Bool -> a) -> Options msg -> List a
mapOverInvisibleTracks f options =
    options.tracks
        |> List.filter (not << .visible)
        |> List.indexedMap
            (\i track -> f track (Just i == options.activeTrackIndex))
