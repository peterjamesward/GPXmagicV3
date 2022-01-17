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
import UtilsForViews exposing (showAngle)
import ViewPureStyles exposing (neatToolsBorder, sliderThumb, useIcon)


type alias Options =
    { dummy : Int
    }


defaultOptions =
    { dummy = 0
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
    case msg of
        PointerForwardOne ->
            ( options, [] )

        PointerBackwardOne ->
            ( options, [] )

        PointerFastForward ->
            ( options, [] )

        PointerRewind ->
            ( options, [] )

        DropMarker ->
            ( options, [] )

        LiftMarker ->
            ( options, [] )

        MarkerForwardOne ->
            ( options, [] )

        MarferBackwardOne ->
            ( options, [] )


view : (Msg -> msg) -> Options -> Element msg
view msgWrapper options =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        column
            [ centerX
            , padding 4
            , spacing 6
            --, height <| px 150
            ]
            [ el [ centerX ] <|
                text <|
                    "About the Orange pointer ..."
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
                Input.button (padding 8 :: neatToolsBorder)
                    { label = text "Drop purple marker"
                    , onPress = Just <| msgWrapper <| DropMarker
                    }
            , row
                [ centerX
                , spacing 10
                , Font.color FlatColors.AussiePalette.blurple
                ]
                [ Input.button neatToolsBorder
                    { label = useIcon FeatherIcons.chevronLeft
                    , onPress = Just <| msgWrapper <| MarferBackwardOne
                    }
                , Input.button neatToolsBorder
                    { label = useIcon FeatherIcons.chevronsRight
                    , onPress = Just <| msgWrapper <| MarkerForwardOne
                    }
                ]
            , el [ centerX ] <|
                text <|
                    "About the Purple marker ..."
            ]
