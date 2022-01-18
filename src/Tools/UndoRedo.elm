module Tools.UndoRedo exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (neatToolsBorder)


type alias UndoEntry =
    { fromStart : Int
    , fromEnd : Int
    --, command : TBD -- could this be the Msg from the button click?
    --, options : TBD -- relevant tool settings?
    , originalPoints : List ( GPXSource, EarthPoint ) -- for reconstructing the original tree
    }


type alias Options =
    { undos : List UndoEntry
    , redos : List UndoEntry
    }


undoLimit =
    --artitrarily
    20


defaultOptions =
    { undos = []
    , redos = []
    }


type
    Msg
    --TODO: Decide on (start, finish), (start, count), or (fromStart, fromEnd).
    --Shall we start by considering the single point deletion?
    = Undo
    | Redo


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    ( options, [] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, Undo ) ->
            ( options
            , []
            )

        ( Just track, Redo ) ->
            ( options
            , []
            )

        _ ->
            ( options, [] )


view : (Msg -> msg) -> Options -> Element msg
view msgWrapper options =
    column [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ]
        [ el [ centerX, padding 4, spacing 4, height <| px 50 ] <|
            case options.undos of
                [] ->
                    Input.button (centerY :: neatToolsBorder)
                        { onPress = Nothing
                        , label = text "Nothing to Undo"
                        }

                something ->
                    Input.button (centerY :: neatToolsBorder)
                        { onPress = Just (msgWrapper Undo)
                        , label = text "Undo something"
                        }
        , el [ centerX, padding 4, spacing 4, height <| px 50 ] <|
            case options.undos of
                [] ->
                    Input.button (centerY :: neatToolsBorder)
                        { onPress = Nothing
                        , label = text "Nothing to Redo"
                        }

                something ->
                    Input.button (centerY :: neatToolsBorder)
                        { onPress = Just (msgWrapper Undo)
                        , label = text "Redo something"
                        }
        ]
