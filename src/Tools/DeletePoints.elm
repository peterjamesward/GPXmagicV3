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
    { singlePoint : Bool
    }


defaultOptions =
    { singlePoint = True
    }


type Msg
    = DeletePointRange -- deletes track between and including markers.


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            -- Make sure we have up to date breaches and preview is shown.
            ( { options | singlePoint = theTrack.markerPosition == Nothing }
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
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, DeletePointRange ) ->
            let
                ( fromStart, fromEnd ) =
                    TrackLoaded.getRangeFromMarkers track

                action =
                    -- Curious semantics here. If no marker, delete single point (hence inclusive, explicitly).
                    -- but with marker, more sensible if the markers themselves are not deletes (hence, exclusive).
                    -- This attempts to be explicit.
                    if track.markerPosition == Nothing then
                        DeletePointsIncluding fromStart fromEnd

                    else
                        DeletePointsBetween fromStart fromEnd
            in
            ( options
            , [ action
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
                { onPress = Just (msgWrapper DeletePointRange)
                , label =
                    if options.singlePoint then
                        text "Delete single point"

                    else
                        text "Delete between markers"
                }


deletePointRange : Int -> Int -> PeteTree -> ( Maybe PeteTree, List ( EarthPoint, GPXSource ) )
deletePointRange fromStart fromEnd treeNode =
    -- Deletes, if possible, inclusive of the markers. We're counting road segments.
    let
        ( leftWithOverlap, rightWithOverlap ) =
            -- These include the track points to be deleted, when we
            -- join the two sides, we create a new leaf that omits these.
            ( takeFromLeft fromStart treeNode
            , takeFromRight fromEnd treeNode
            )

        oldPoints =
            DomainModel.extractPointsInRange
                fromStart
                fromEnd
                treeNode
    in
    ( safeJoinReplacingEndPointsWithNewLeaf leftWithOverlap rightWithOverlap
    , oldPoints
    )
