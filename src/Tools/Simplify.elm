module Tools.Simplify exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import Length exposing (Meters)
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity, Squared)
import String.Interpolate
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import ViewPureStyles exposing (neatToolsBorder, noTrackMessage)


toolId =
    "simplify"


type alias Options =
    { pointsToRemove : Dict Int Int }


defaultOptions =
    { pointsToRemove = Dict.empty }


type Msg
    = Seek
    | Apply
    | DisplayInfo String String
    | FlushUndo


findSimplifications : Options -> PeteTree -> Options
findSimplifications options tree =
    -- This function called when track changes, or we call it when threshold is changed.
    -- We search the tree. At worst, fold over the whole darn tree. Optimize if needed.
    let
        foldFn :
            RoadSection
            -> ( Int, Maybe RoadSection, List ( Int, Quantity Float (Squared Meters) ) )
            -> ( Int, Maybe RoadSection, List ( Int, Quantity Float (Squared Meters) ) )
        foldFn road ( index, previousIfAny, outputs ) =
            -- Fold gives us the areas of each wee triangle defined by adjacent segments.
            case previousIfAny of
                Nothing ->
                    -- Wait for next one
                    ( 1, Just road, [] )

                Just previous ->
                    ( index + 1
                    , Just road
                    , ( index
                      , Triangle3d.area <|
                            Triangle3d.from
                                previous.startPoint.space
                                road.startPoint.space
                                road.endPoint.space
                      )
                        :: outputs
                    )

        ( _, _, triangleInfo ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount tree)
                (always Nothing)
                0
                tree
                foldFn
                ( 0, Nothing, [] )

        selectSmallestAreas : List ( Int, Quantity Float (Squared Meters) )
        selectSmallestAreas =
            -- Find smallest 20%; number removed usually reduced by adjacency test.
            triangleInfo
                |> List.sortWith
                    (\( idx1, area1 ) ( idx2, area2 ) ->
                        if area1 |> Quantity.lessThanOrEqualTo area2 then
                            LT

                        else
                            GT
                    )
                |> List.take (List.length triangleInfo // 5)

        nonAdjacentEntries : Dict Int Int
        nonAdjacentEntries =
            -- Using a dict here just removes need to sort again by index.
            List.foldl
                (\( idx, area ) outputs ->
                    if Dict.member (idx + 1) outputs || Dict.member (idx - 1) outputs then
                        outputs

                    else
                        Dict.insert idx idx outputs
                )
                Dict.empty
                selectSmallestAreas
    in
    { options | pointsToRemove = nonAdjacentEntries }


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    -- Deleting arbitrary collection of non-adjacent points implies rebuild.
    let
        originalCourse : Dict Int GPXSource
        originalCourse =
            DomainModel.getAllGPXPointsInDict track.trackTree

        newCourse : Dict Int GPXSource
        newCourse =
            Dict.foldl
                (\k v out -> Dict.remove k out)
                originalCourse
                options.pointsToRemove

        newTree : Maybe PeteTree
        newTree =
            DomainModel.treeFromSourcesWithExistingReference track.referenceLonLat <|
                Dict.values newCourse

        oldPoints : List GPXSource
        oldPoints =
            -- All the points.
            Dict.values originalCourse
    in
    ( newTree
    , oldPoints
    )


simplifyFor1CQF : TrackLoaded msg -> PeteTree
simplifyFor1CQF track =
    let
        options =
            findSimplifications defaultOptions track.trackTree

        ( outputTree, oldPoints ) =
            apply options track
    in
    outputTree |> Maybe.withDefault track.trackTree


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
            let
                populatedOptions =
                    findSimplifications options theTrack.trackTree
            in
            ( populatedOptions
            , actions colour populatedOptions theTrack
            )

        _ ->
            -- Hide preview
            ( { options | pointsToRemove = Dict.empty }
            , [ HidePreview "simplify" ]
            )


actions : Color -> Options -> TrackLoaded msg -> List (ToolAction msg)
actions colour options track =
    [ ShowPreview
        { tag = "simplify"
        , shape = PreviewCircle
        , colour = colour
        , points =
            TrackLoaded.buildPreview
                (Dict.keys options.pointsToRemove)
                track.trackTree
        }
    ]


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        Seek ->
            let
                newOptions =
                    findSimplifications options track.trackTree
            in
            ( newOptions, actions previewColour newOptions track )

        Apply ->
            ( options, [ Actions.ApplySimplify, TrackHasChanged ] )

        FlushUndo ->
            ( options, [ Actions.FlushUndo ] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


view : I18NOptions.Location -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location msgWrapper options isTrack =
    let
        i18n =
            I18N.text location toolId
    in
    case isTrack of
        Just track ->
            column
                [ width fill
                , padding 10
                , spacing 10
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ el [ centerX ] <|
                    Input.button neatToolsBorder <|
                        case Dict.size options.pointsToRemove of
                            0 ->
                                { onPress = Just <| msgWrapper Seek
                                , label = i18n "search"
                                }

                            quantity ->
                                { onPress = Just <| msgWrapper Apply
                                , label =
                                    paragraph [] <|
                                        [ text <|
                                            String.Interpolate.interpolate
                                                (I18N.localisedString location toolId "remove")
                                                [ String.fromInt quantity ]
                                        ]
                                }
                , el [ centerX ] <|
                    Input.button neatToolsBorder
                        { onPress = Just <| msgWrapper FlushUndo
                        , label =
                            paragraph [] <|
                                [ i18n "flush" ]
                        }
                ]

        Nothing ->
            noTrackMessage location


guidanceText =
    """Intended mainly for recorded real-life rides, this tool searches
for points with the least contribution to the overall shape, avoiding
removing adjacent points.
Mostly, this removes noise, but there's no way to remove the 
possibility of removing or eliding features.
"""
