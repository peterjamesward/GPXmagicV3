module Tools.Essentials exposing (Msg(..), Options, defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import CommonToolStyles exposing (noTrackMessage)
import DomainModel exposing (PeteTree, skipCount)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure)
import ViewPureStyles exposing (neatToolsBorder, sliderThumb, useIcon)


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
    | PointerPosition Int


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
                PointerPosition position ->
                    ( { options | orange = position }
                    , [ SetCurrent position, PointerChange ]
                    )

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
                            restrictToTrack orange -(skipCount track.trackTree // 20)
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


positionDescription : SystemSettings -> Int -> PeteTree -> String
positionDescription settings pos track =
    let
        localString =
            I18N.localisedString settings.location toolId "point"
    in
    String.Interpolate.interpolate
        localString
        [ String.fromInt pos
        , showLongMeasure settings.imperial <| DomainModel.distanceFromIndex pos track
        ]


view : SystemSettings -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view settings msgWrapper options isTrack =
    column (CommonToolStyles.toolContentBoxStyle settings) <|
        case isTrack of
            Just track ->
                [ viewPointers settings msgWrapper options track
                , viewUndoRedo settings msgWrapper track
                ]

            Nothing ->
                [ noTrackMessage settings ]


viewPointers : SystemSettings -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
viewPointers settings msgWrapper options track =
    let
        purpleStyle =
            [ Border.color FlatColors.AussiePalette.blurple
            , Border.rounded 4
            , Border.width 4
            , padding 6
            , centerY
            , centerX
            , height <| px 40
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
        (CommonToolStyles.toolContentBoxStyle settings)
        [ el [ centerX ] <|
            text <|
                positionDescription settings options.orange track.trackTree
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
        , el [ centerX ] <|
            Input.slider
                ViewPureStyles.mediumSliderStyles
                { onChange = round >> PointerPosition >> msgWrapper
                , value = toFloat track.currentPosition
                , label = Input.labelHidden "position"
                , min = 0
                , max = toFloat <| skipCount track.trackTree
                , step = Just 1
                , thumb = sliderThumb
                }
        , el [ centerX, width fill ] <|
            case options.purple of
                Just _ ->
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
                            { label = I18N.text settings.location toolId "lift"
                            , onPress = Just <| msgWrapper <| LiftMarker
                            }
                        , Input.button purpleStyle
                            { label = useIcon FeatherIcons.chevronRight
                            , onPress = Just <| msgWrapper <| MarkerForwardOne
                            }
                        ]

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
                            { label = I18N.text settings.location toolId "drop"
                            , onPress = Just <| msgWrapper <| DropMarker
                            }
        , el [ centerX, padding 5 ] <|
            case options.purple of
                Just something ->
                    text <| positionDescription settings something track.trackTree

                Nothing ->
                    I18N.text settings.location toolId "note"
        ]


viewUndoRedo : SystemSettings -> (Msg -> msg) -> TrackLoaded msg -> Element msg
viewUndoRedo settings msgWrapper track =
    el [ centerX ] <|
        wrappedRow
            [ width fill, padding 20, spacing 10, centerX ]
            [ case track.undos of
                [] ->
                    Input.button neatToolsBorder
                        { onPress = Nothing
                        , label = I18N.text settings.location toolId "noundo"
                        }

                undo :: _ ->
                    Input.button (alignRight :: neatToolsBorder)
                        { onPress = Just (msgWrapper Undo)
                        , label =
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString settings.location toolId "undo")
                                    [ Actions.actionTextForUndo settings.location undo.action ]
                        }
            , case track.redos of
                [] ->
                    Input.button neatToolsBorder
                        { onPress = Nothing
                        , label = I18N.text settings.location toolId "noredo"
                        }

                redo :: _ ->
                    Input.button (alignRight :: neatToolsBorder)
                        { onPress = Just (msgWrapper Redo)
                        , label =
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString settings.location toolId "redo")
                                    [ Actions.actionTextForUndo settings.location redo.action ]
                        }
            ]
