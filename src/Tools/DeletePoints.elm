module Tools.DeletePoints exposing (..)

import Actions exposing (ToolAction(..))
import BoundingBox3d
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, earthPointFromIndex, getDualCoords, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import PreviewData exposing (PreviewShape(..))
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (fullDepthRenderingBoxSize)
import ViewPureStyles exposing (neatToolsBorder)


toolId =
    "delete"


type alias Options =
    { singlePoint : Bool
    , pointsToBeDeleted : List Int
    }


defaultOptions : Options
defaultOptions =
    { singlePoint = True
    , pointsToBeDeleted = []
    }


type Msg
    = DeletePointOrPoints -- One button serves both cases.
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
            let
                fullRenderingZone =
                    BoundingBox3d.withDimensions
                        ( fullDepthRenderingBoxSize
                        , fullDepthRenderingBoxSize
                        , fullDepthRenderingBoxSize
                        )
                        (startPoint <| leafFromIndex theTrack.currentPosition theTrack.trackTree)

                ( fromStart, fromEnd ) =
                    TrackLoaded.getRangeFromMarkers theTrack

                distanceToPreview =
                    DomainModel.distanceFromIndex fromStart theTrack.trackTree

                depthFunction : RoadSection -> Maybe Int
                depthFunction road =
                    if road.boundingBox |> BoundingBox3d.intersects fullRenderingZone then
                        Nothing

                    else
                        Just 10

                foldFn : RoadSection -> List EarthPoint -> List EarthPoint
                foldFn road accum =
                    road.startPoint
                        :: accum

                previews =
                    case theTrack.markerPosition of
                        Just _ ->
                            List.drop 1 <|
                                List.reverse <|
                                    traverseTreeBetweenLimitsToDepth
                                        fromStart
                                        (skipCount theTrack.trackTree - fromEnd)
                                        depthFunction
                                        0
                                        theTrack.trackTree
                                        foldFn
                                        []

                        Nothing ->
                            [ earthPointFromIndex fromStart theTrack.trackTree ]
            in
            ( { options | singlePoint = theTrack.markerPosition == Nothing }
            , [ ShowPreview
                    { tag = "delete"
                    , shape = PreviewCircle
                    , colour = colour
                    , points = TrackLoaded.asPreviewPoints theTrack distanceToPreview previews
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
        ( Just track, DeletePointOrPoints ) ->
            let
                ( fromStart, fromEnd ) =
                    TrackLoaded.getRangeFromMarkers track

                action =
                    -- Curious semantics here. If no marker, delete single point (hence inclusive, explicitly).
                    -- but with marker, more sensible if the markers themselves are not deletes (hence, exclusive).
                    -- This attempts to be explicit.
                    if track.markerPosition == Nothing then
                        DeleteSinglePoint fromStart fromEnd

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


view : (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view msgWrapper options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        wholeTrackIsSelected =
            fromStart == 0 && fromEnd == 0
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        el [ centerX, padding 4, spacing 4, height <| px 50 ] <|
            if wholeTrackIsSelected then
                el [ padding 5, centerX, centerY ] <|
                    text "Sorry, I can't let you do that."

            else
                Input.button (centerY :: neatToolsBorder)
                    { onPress = Just (msgWrapper DeletePointOrPoints)
                    , label =
                        if options.singlePoint then
                            text "Delete single point"

                        else
                            text "Delete between and including markers"
                    }



-- This function finally does the deed, driven by the Action interpreter in Main.


deleteSinglePoint : Int -> Int -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
deleteSinglePoint fromStart fromEnd track =
    -- Clearer to deal with this case separately.
    -- If they are combined later, I'd be happy with that also.
    let
        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                []
                track.trackTree

        oldPoints =
            [ DomainModel.gpxPointFromIndex track.currentPosition track.trackTree ]
    in
    ( newTree
    , oldPoints
    )


deletePointsBetween : Int -> Int -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
deletePointsBetween fromStart fromEnd track =
    let
        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                []
                track.trackTree

        oldPoints =
            -- The Nothing here means no depth limit, so we get all the points.
            -- Note we have to reverse them.
            DomainModel.extractPointsInRange
                (fromStart - 1)
                (fromEnd - 1)
                track.trackTree
    in
    ( newTree
    , oldPoints |> List.map Tuple.second
    )
