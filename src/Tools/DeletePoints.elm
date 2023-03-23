module Tools.DeletePoints exposing
    ( Msg(..)
    , Options
    ,  defaultOptions
       --, deletePointsBetween

    , deleteMarkedRange
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import BoundingBox3d
import CommonToolStyles
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, earthPointFromIndex, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import PreviewData exposing (PreviewShape(..))
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
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
    = DeletePointOrPoints


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
                        (.space <|
                            startPoint <|
                                leafFromIndex theTrack.currentPosition theTrack.trackTree
                        )

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
    -> ( Options, ToolAction msg )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, DeletePointOrPoints ) ->
            ( options
            , UpdateActiveTrack "delete" (deleteMarkedRange track)
            )

        _ ->
            ( options, Actions.NoAction )


view : SystemSettings -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view settings msgWrapper options track =
    let
        i18n =
            I18N.text settings.location toolId

        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        wholeTrackIsSelected =
            fromStart == 0 && fromEnd == 0
    in
    el (CommonToolStyles.toolContentBoxStyle settings) <|
        el [ centerX, padding 4, spacing 4, height <| px 50 ] <|
            if wholeTrackIsSelected then
                el [ padding 5, centerX, centerY ] <|
                    i18n "sorry"

            else
                Input.button (centerY :: neatToolsBorder)
                    { onPress = Just (msgWrapper DeletePointOrPoints)
                    , label =
                        if options.singlePoint then
                            i18n "single"

                        else
                            i18n "many"
                    }



-- This function finally does the deed, driven by the Action interpreter in Main.
-- Markers should preserve their offset from the track ends, according to their disposition
-- relative to the deleted part.


deleteMarkedRange : TrackLoaded msg -> TrackLoaded msg
deleteMarkedRange track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                []
                track.trackTree
    in
    case newTree of
        Just isNewTree ->
            let
                ( newOrange, newPurple ) =
                    case track.markerPosition of
                        Nothing ->
                            -- Only orange, single point, just check for track end.
                            ( min track.currentPosition (DomainModel.skipCount track.trackTree)
                            , Nothing
                            )

                        Just purple ->
                            if track.currentPosition <= purple then
                                ( track.currentPosition
                                , Just <| DomainModel.skipCount isNewTree - fromEnd
                                )

                            else
                                ( DomainModel.skipCount isNewTree - fromEnd
                                , Just purple
                                )
            in
            { track
                | trackTree = isNewTree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves isNewTree
            }

        Nothing ->
            track
