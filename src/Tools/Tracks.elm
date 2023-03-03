module Tools.Tracks exposing
    ( Msg(..)
    , Options
    , addTrack
    , defaultOptions
    , toolId
    , update
    , view
    )

import Actions
import CommonToolStyles
import Element exposing (..)
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)


toolId =
    "tracks"


type alias Options msg =
    --TODO: This will develop, taking track stuff from Main, and perhaps subsume Graph.
    { nextTrackNumber : Int
    , tracks : List (TrackLoaded msg)
    }


defaultOptions : Options msg
defaultOptions =
    { nextTrackNumber = 1
    , tracks = []
    }


type Msg
    = SelectActiveTrack Int


update : Msg -> Options msg -> ( Options msg, List (Actions.ToolAction msg) )
update msg options =
    case msg of
        SelectActiveTrack mode ->
            ( options, [ Actions.SetActiveTrack 0 ] )


view : SystemSettings -> (Msg -> msg) -> Options msg -> Element msg
view settings wrapper options =
    let
        helper =
            I18N.text settings.location toolId
    in
    column (CommonToolStyles.toolContentBoxStyle settings) <|
        List.map displayTrackInfo options.tracks


displayTrackInfo : TrackLoaded msg -> Element msg
displayTrackInfo track =
    text track.trackName


addTrack : TrackLoaded msg -> Options msg -> Options msg
addTrack track options =
    { options
        | tracks = track :: options.tracks
        , nextTrackNumber = options.nextTrackNumber + 1
    }
