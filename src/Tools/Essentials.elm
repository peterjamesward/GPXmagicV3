module Tools.Essentials exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure)
import ViewPureStyles exposing (neatToolsBorder, noTrackMessage, useIcon)


type alias Options =
    { orange : Int
    , purple : Maybe Int
    }


defaultOptions =
    { orange = 0
    , purple = Nothing
    }


type Msg
    = PointerForwardOne
    | PointerBackwardOne
    | PointerFastForward
    | PointerRewind
    | DropMarker
    | LiftMarker
    | MarkerForwardOne
    | MarkerBackwardOne
    | Undo
    | Redo
    | DisplayInfo String String


toolID : String
toolID =
    "essentials"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Essentials" )
        , ( "info", infoText )
        ]
    )


infoText =
    """Most of the editing tools require either a single point or a range of points to work on.

The top buttons in this tool allow you to move an Orange marker along the track to select a single point.

There is a button to place a Purple marker at the current position, and then to move the Purple marker.
This defines a range which you can then use for tools that require it.

Below the pointer controls are the Undo and Redo buttons. These let you go back and forward over the
previous ten edits. Once you make a different change, you can only Undo.
"""


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( { options
                | orange = theTrack.currentPosition
                , purple = theTrack.markerPosition
              }
            , []
            )

        _ ->
            -- Hide preview
            ( options, [] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case hasTrack of
        Nothing ->
            ( options, [] )

        Just track ->
            let
                restrictToTrack value increment =
                    clamp 0 (skipCount track.trackTree) <| value + increment

                orange =
                    -- Track has definitive pointer position.
                    track.currentPosition
            in
            case msg of
                PointerForwardOne ->
                    let
                        position =
                            restrictToTrack orange 1
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position, PointerChange ]
                    )

                PointerBackwardOne ->
                    let
                        position =
                            restrictToTrack orange -1
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position, PointerChange ]
                    )

                PointerFastForward ->
                    let
                        position =
                            restrictToTrack orange (skipCount track.trackTree // 20)
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position, PointerChange ]
                    )

                PointerRewind ->
                    let
                        position =
                            restrictToTrack orange (0 - skipCount track.trackTree // 20)
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position, PointerChange ]
                    )

                DropMarker ->
                    ( { options | purple = Just orange }
                    , [ SetMarker <| Just orange, PointerChange ]
                    )

                LiftMarker ->
                    ( { options | purple = Nothing }
                    , [ SetMarker Nothing, PointerChange ]
                    )

                MarkerForwardOne ->
                    let
                        position =
                            case options.purple of
                                Just something ->
                                    Just <| restrictToTrack something 1

                                Nothing ->
                                    Nothing
                    in
                    ( { options
                        | purple = position
                      }
                    , [ SetMarker position, PointerChange ]
                    )

                MarkerBackwardOne ->
                    let
                        position =
                            case options.purple of
                                Just something ->
                                    Just <| restrictToTrack something -1

                                Nothing ->
                                    Nothing
                    in
                    ( { options
                        | purple = position
                      }
                    , [ SetMarker position, PointerChange ]
                    )

                Undo ->
                    ( options
                    , [ UndoLastAction, TrackHasChanged ]
                    )

                Redo ->
                    ( options
                    , [ RedoUndoneAction, TrackHasChanged ]
                    )

                DisplayInfo tool tag ->
                    ( options, [ Actions.DisplayInfo tool tag ] )


positionDescription : Bool -> Int -> PeteTree -> String
positionDescription imperial pos track =
    "Point "
        ++ String.fromInt pos
        ++ ", at "
        ++ (showLongMeasure imperial <| DomainModel.distanceFromIndex pos track)


view : Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view imperial msgWrapper options isTrack =
    column
        [ width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        case isTrack of
            Just track ->
                [ viewPointers imperial msgWrapper options track
                , viewUndoRedo msgWrapper track
                ]

            Nothing ->
                [ noTrackMessage ]


viewPointers : Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
viewPointers imperial msgWrapper options track =
    let
        purpleStyle =
            [ Border.color FlatColors.AussiePalette.blurple
            , Border.rounded 4
            , Border.width 4
            , padding 6
            , centerY
            , centerX
            ]

        orangeStyle =
            [ Border.color FlatColors.AussiePalette.quinceJelly
            , Border.rounded 4
            , Border.width 4
            , padding 6
            , centerY
            , centerX
            ]
    in
    column
        [ centerX
        , padding 4
        , spacing 6
        , width fill

        --, height <| px 150
        ]
        [ el [ centerX ] <|
            text <|
                positionDescription imperial options.orange track.trackTree
        , row
            [ centerX
            , spacing 10
            , Font.color FlatColors.AussiePalette.quinceJelly
            ]
            [ Input.button orangeStyle
                { label = useIcon FeatherIcons.chevronsLeft
                , onPress = Just <| msgWrapper <| PointerRewind
                }
            , Input.button orangeStyle
                { label = useIcon FeatherIcons.chevronLeft
                , onPress = Just <| msgWrapper <| PointerBackwardOne
                }
            , Input.button orangeStyle
                { label = useIcon FeatherIcons.chevronRight
                , onPress = Just <| msgWrapper <| PointerForwardOne
                }
            , Input.button orangeStyle
                { label = useIcon FeatherIcons.chevronsRight
                , onPress = Just <| msgWrapper <| PointerFastForward
                }
            ]
        , el [ centerX, width fill ] <|
            case options.purple of
                Just something ->
                    el
                        [ Background.color FlatColors.AussiePalette.blurple
                        , Font.color FlatColors.AussiePalette.coastalBreeze
                        , width fill
                        , height <| px 34
                        , centerX
                        , centerY
                        ]
                    <|
                        Input.button
                            [ Background.color FlatColors.AussiePalette.blurple
                            , Border.color FlatColors.AussiePalette.coastalBreeze
                            , Border.rounded 4
                            , Border.width 2
                            , padding 8
                            , centerY
                            , centerX
                            ]
                            { label = text "Lift purple marker"
                            , onPress = Just <| msgWrapper <| LiftMarker
                            }

                Nothing ->
                    el
                        [ width fill
                        , height <| px 34
                        , centerX
                        , centerY
                        ]
                    <|
                        Input.button
                            purpleStyle
                            { label = text "Drop purple marker"
                            , onPress = Just <| msgWrapper <| DropMarker
                            }
        , case options.purple of
            Just something ->
                row
                    [ centerX
                    , spacing 10
                    , Font.color FlatColors.AussiePalette.blurple
                    ]
                    [ Input.button purpleStyle
                        { label = useIcon FeatherIcons.chevronLeft
                        , onPress = Just <| msgWrapper <| MarkerBackwardOne
                        }
                    , Input.button purpleStyle
                        { label = useIcon FeatherIcons.chevronRight
                        , onPress = Just <| msgWrapper <| MarkerForwardOne
                        }
                    ]

            Nothing ->
                row
                    [ centerX
                    , spacing 10
                    , Font.color FlatColors.AussiePalette.coastalBreeze
                    ]
                    [ Input.button purpleStyle
                        { label = useIcon FeatherIcons.chevronLeft
                        , onPress = Nothing
                        }
                    , Input.button purpleStyle
                        { label = useIcon FeatherIcons.chevronRight
                        , onPress = Nothing
                        }
                    ]
        , el [ centerX ] <|
            case options.purple of
                Just something ->
                    text <| positionDescription imperial something track.trackTree

                Nothing ->
                    text "Use Orange and Purple markers\nto select track for editing."
        ]


viewUndoRedo : (Msg -> msg) -> TrackLoaded msg -> Element msg
viewUndoRedo msgWrapper track =
    el [ centerX ] <|
        wrappedRow
            [ width fill, padding 20, spacing 10, centerX ]
            [ case track.undos of
                [] ->
                    Input.button neatToolsBorder
                        { onPress = Nothing
                        , label = text "Nothing to Undo"
                        }

                undo :: _ ->
                    Input.button (alignRight :: neatToolsBorder)
                        { onPress = Just (msgWrapper Undo)
                        , label = text <| "Undo " ++ Actions.interpretAction undo.action
                        }
            , case track.redos of
                [] ->
                    Input.button neatToolsBorder
                        { onPress = Nothing
                        , label = text "Nothing to Redo"
                        }

                redo :: _ ->
                    Input.button (alignRight :: neatToolsBorder)
                        { onPress = Just (msgWrapper Redo)
                        , label = text <| "Redo " ++ Actions.interpretAction redo.action
                        }
            ]
