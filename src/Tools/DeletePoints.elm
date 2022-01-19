module Tools.DeletePoints exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, safeJoinReplacingEndPointsWithNewLeaf, skipCount, takeFromLeft, takeFromRight)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (neatToolsBorder)


type alias Options =
    { dummy : Int
    }


defaultOptions =
    { dummy = 0
    }


type
    Msg
    --TODO: Decide on (start, finish), (start, count), or (fromStart, fromEnd).
    --Shall we start by considering the single point deletion?
    = Delete --Int Int


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            -- Make sure we have up to date breaches and preview is shown.
            ( options
            , [ ShowPreview
                    { tag = "delete"
                    , shape = PreviewCircle
                    , colour = colour
                    , points =
                        --TODO: list of points between markers
                        DomainModel.buildPreview
                            [ theTrack.currentPosition ]
                            theTrack.trackTree
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "delete" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, Delete ) ->
            ( options
            , [ DeleteSinglePoint track.currentPosition
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : (Msg -> msg) -> Options -> Element msg
view msgWrapper options =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        el [ centerX, padding 4, spacing 4, height <| px 50 ] <|
            Input.button (centerY :: neatToolsBorder)
                { onPress = Just (msgWrapper Delete)
                , label = text "Delete point"
                }


deleteSinglePoint : Int -> PeteTree -> Maybe PeteTree
deleteSinglePoint index treeNode =
    -- Implement with takeFromLeft|Right, should generalise trivially.
    let
        ( leftWithOverlap, rightWithOverlap ) =
            -- These include the track points to be deleted, when we
            -- join the two sides, we create a new leaf that omits these.
            ( takeFromLeft index treeNode
            , takeFromRight (skipCount treeNode - index) treeNode
            )
    in
    safeJoinReplacingEndPointsWithNewLeaf leftWithOverlap rightWithOverlap

