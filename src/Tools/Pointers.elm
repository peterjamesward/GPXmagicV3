module Tools.Pointers exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import List.Extra
import Quantity
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showLongMeasure)
import ViewPureStyles exposing (neatToolsBorder, sliderThumb, useIcon)


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
    | MarferBackwardOne


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            --TODO: Interesting. Could use preview.
            ( options
            , []
            )

        _ ->
            -- Hide preview
            ( options, [] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case hasTrack of
        Nothing ->
            ( options, [] )

        Just track ->
            let
                restrictToTrack value increment =
                    clamp 0 (skipCount track.trackTree) <| value + increment
            in
            case msg of
                PointerForwardOne ->
                    let
                        position =
                            restrictToTrack options.orange 1
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position ]
                    )

                PointerBackwardOne ->
                    let
                        position =
                            restrictToTrack options.orange -1
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position ]
                    )

                PointerFastForward ->
                    let
                        position =
                            restrictToTrack options.orange (skipCount track.trackTree // 20)
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position ]
                    )

                PointerRewind ->
                    let
                        position =
                            restrictToTrack options.orange (0 - skipCount track.trackTree // 20)
                    in
                    ( { options | orange = position }
                    , [ SetCurrent position ]
                    )

                DropMarker ->
                    ( { options | purple = Just options.orange }
                    , [ SetMarker <| Just options.orange ]
                    )

                LiftMarker ->
                    ( { options | purple = Nothing }
                    , [ SetMarker Nothing ]
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
                    , [ SetMarker position ]
                    )

                MarferBackwardOne ->
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
                    , [ SetMarker position ]
                    )


positionDescription : Int -> PeteTree -> String
positionDescription pos track =
    "Point "
        ++ String.fromInt pos
        ++ ", at "
        ++ (showLongMeasure False <| DomainModel.distanceFromIndex pos track)
        ++ "m"


view : (Msg -> msg) -> Options -> Maybe TrackLoaded -> Element msg
view msgWrapper options isTrack =
    case isTrack of
        Nothing ->
            el
                [ width fill
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                , height <| px 140
                ]
                none

        Just track ->
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column
                    [ centerX
                    , padding 4
                    , spacing 6

                    --, height <| px 150
                    ]
                    [ el [ centerX ] <|
                        text <|
                            positionDescription options.orange track.trackTree
                    , row
                        [ centerX
                        , spacing 10
                        , Font.color FlatColors.AussiePalette.quinceJelly
                        ]
                        [ Input.button neatToolsBorder
                            { label = useIcon FeatherIcons.chevronsLeft
                            , onPress = Just <| msgWrapper <| PointerRewind
                            }
                        , Input.button neatToolsBorder
                            { label = useIcon FeatherIcons.chevronLeft
                            , onPress = Just <| msgWrapper <| PointerBackwardOne
                            }
                        , Input.button neatToolsBorder
                            { label = useIcon FeatherIcons.chevronRight
                            , onPress = Just <| msgWrapper <| PointerForwardOne
                            }
                        , Input.button neatToolsBorder
                            { label = useIcon FeatherIcons.chevronsRight
                            , onPress = Just <| msgWrapper <| PointerFastForward
                            }
                        ]
                    , el [ centerX ] <|
                        case options.purple of
                            Just something ->
                                Input.button (padding 8 :: neatToolsBorder)
                                    { label = text "Lift purple marker"
                                    , onPress = Just <| msgWrapper <| LiftMarker
                                    }

                            Nothing ->
                                Input.button (padding 8 :: neatToolsBorder)
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
                                [ Input.button neatToolsBorder
                                    { label = useIcon FeatherIcons.chevronLeft
                                    , onPress = Just <| msgWrapper <| MarferBackwardOne
                                    }
                                , Input.button neatToolsBorder
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
                                [ Input.button neatToolsBorder
                                    { label = useIcon FeatherIcons.chevronLeft
                                    , onPress = Just <| msgWrapper <| MarferBackwardOne
                                    }
                                , Input.button neatToolsBorder
                                    { label = useIcon FeatherIcons.chevronRight
                                    , onPress = Just <| msgWrapper <| MarkerForwardOne
                                    }
                                ]
                    , el [ centerX ] <|
                        case options.purple of
                            Just something ->
                                text <| positionDescription something track.trackTree

                            Nothing ->
                                text "---"
                    ]
