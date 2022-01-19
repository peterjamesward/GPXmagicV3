module Tools.UndoRedo exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (neatToolsBorder)


undoLimit =
    --artitrarily
    20


type alias Options =
    { dummy : Int }


defaultOptions =
    Options 0


type Msg
    = Undo
    | Redo


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    ( options, [] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
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


view : (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view msgWrapper options mTrack =
    case mTrack of
        Just track ->
            viewWithTrack msgWrapper options track

        Nothing ->
            column [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ]
                [ el [ centerX, padding 4, spacing 4, height <| px 50 ] none
                , el [ centerX, padding 4, spacing 4, height <| px 50 ] none
                ]


viewWithTrack : (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
viewWithTrack msgWrapper options track =
    column [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ]
        [ el [ centerX, padding 4, spacing 4, height <| px 50 ] <|
            case track.undos of
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
            case track.redos of
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
