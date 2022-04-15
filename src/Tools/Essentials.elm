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
import String.Interpolate
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure)
import ViewPureStyles exposing (neatToolsBorder, noTrackMessage, useIcon)


toolId =
    "essentials"


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


positionDescription : I18NOptions.Location -> Bool -> Int -> PeteTree -> String
positionDescription location imperial pos track =
    let
        localString =
            I18N.localisedString location toolId "point"
    in
    String.Interpolate.interpolate
        localString
        [ String.fromInt pos
        , showLongMeasure imperial <| DomainModel.distanceFromIndex pos track
        ]


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial msgWrapper options isTrack =
    column
        [ width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        case isTrack of
            Just track ->
                [ viewPointers location imperial msgWrapper options track
                , viewUndoRedo location msgWrapper track
                ]

            Nothing ->
                [ noTrackMessage location ]


viewPointers : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
viewPointers location imperial msgWrapper options track =
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
                positionDescription location imperial options.orange track.trackTree
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
                            { label = I18N.text location toolId "lift"
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
                            { label = I18N.text location toolId "drop"
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
                    text <| positionDescription location imperial something track.trackTree

                Nothing ->
                    I18N.text location toolId "note"
        ]


viewUndoRedo : I18NOptions.Location -> (Msg -> msg) -> TrackLoaded msg -> Element msg
viewUndoRedo location msgWrapper track =
    el [ centerX ] <|
        wrappedRow
            [ width fill, padding 20, spacing 10, centerX ]
            [ case track.undos of
                [] ->
                    Input.button neatToolsBorder
                        { onPress = Nothing
                        , label = I18N.text location toolId "noundo"
                        }

                undo :: _ ->
                    Input.button (alignRight :: neatToolsBorder)
                        { onPress = Just (msgWrapper Undo)
                        , label =
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "undo")
                                    [ Actions.interpretAction location undo.action ]
                        }
            , case track.redos of
                [] ->
                    Input.button neatToolsBorder
                        { onPress = Nothing
                        , label = I18N.text location toolId "noredo"
                        }

                redo :: _ ->
                    Input.button (alignRight :: neatToolsBorder)
                        { onPress = Just (msgWrapper Redo)
                        , label =
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "redo")
                                    [ Actions.interpretAction location redo.action ]
                        }
            ]
