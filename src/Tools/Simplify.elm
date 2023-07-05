module Tools.Simplify exposing
    ( Msg(..)
    , Options
    , applyToWholeTrack
    , defaultOptions
    , findSimplifications
    , fingerpaintHelper
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import CommonToolStyles exposing (noTrackMessage)
import Dict exposing (Dict)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Input as Input
import Length exposing (Meters)
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity, Squared)
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import ViewPureStyles exposing (neatToolsBorder)


toolId =
    "simplify"


type alias Options =
    { pointsToRemove : Dict Int Int
    , range : Maybe ( Int, Int )
    }


defaultOptions =
    { pointsToRemove = Dict.empty
    , range = Nothing
    }


type Msg
    = Seek
    | Apply
    | FlushUndo


fingerpaintHelper : TrackLoaded msg -> TrackLoaded msg
fingerpaintHelper track =
    -- Applies between markers given by track and resets markers at end.
    let
        orange =
            track.currentPosition

        withMarkers =
            { defaultOptions
                | range =
                    case track.markerPosition of
                        Just purple ->
                            Just ( min orange purple, max orange purple )

                        Nothing ->
                            Nothing
            }
    in
    applyToWholeTrack
        (findSimplifications withMarkers track.trackTree)
        track


findSimplifications : Options -> PeteTree -> Options
findSimplifications options tree =
    -- This function called when track changes, or we call it when threshold is changed.
    -- We search the tree. At worst, fold over the whole darn tree. Optimize if needed.
    let
        ( startingAt, endingAt ) =
            case options.range of
                Just ( orange, purple ) ->
                    ( min orange purple, max orange purple )

                Nothing ->
                    ( 0, skipCount tree )

        foldFn :
            RoadSection
            -> ( Int, Maybe RoadSection, List ( Int, Quantity Float (Squared Meters) ) )
            -> ( Int, Maybe RoadSection, List ( Int, Quantity Float (Squared Meters) ) )
        foldFn road ( index, previousIfAny, outputs ) =
            -- Fold gives us the areas of each wee triangle defined by adjacent segments.
            case previousIfAny of
                Nothing ->
                    -- Wait for next one
                    ( index + 1, Just road, [] )

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
                startingAt
                endingAt
                (always Nothing)
                0
                tree
                foldFn
                ( startingAt, Nothing, [] )

        selectSmallestAreas : List ( Int, Quantity Float (Squared Meters) )
        selectSmallestAreas =
            -- Find smallest 20%; number removed usually reduced by adjacency test.
            triangleInfo
                |> List.sortWith
                    (\( _, area1 ) ( _, area2 ) ->
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
                (\( idx, _ ) outputs ->
                    if Dict.member (idx + 1) outputs || Dict.member (idx - 1) outputs then
                        outputs

                    else
                        Dict.insert idx idx outputs
                )
                Dict.empty
                selectSmallestAreas
    in
    { options | pointsToRemove = nonAdjacentEntries }


applyToWholeTrack : Options -> TrackLoaded msg -> TrackLoaded msg
applyToWholeTrack options track =
    -- Deleting arbitrary collection of non-adjacent points implies rebuild.
    --TODO: Optimise for application to range.
    let
        originalCourse : Dict Int GPXSource
        originalCourse =
            DomainModel.getAllGPXPointsInDict track.trackTree

        newCourse : Dict Int GPXSource
        newCourse =
            options.pointsToRemove
                |> Dict.foldl
                    (\k _ out -> Dict.remove k out)
                    originalCourse

        newTree : Maybe PeteTree
        newTree =
            DomainModel.treeFromSourcesWithExistingReference track.referenceLonLat <|
                Dict.values newCourse
    in
    case newTree of
        Just isTree ->
            let
                ( newOrange, newPurple ) =
                    case options.range of
                        Just ( startAt, endAt ) ->
                            if track.currentPosition == startAt then
                                ( track.currentPosition
                                , Just <| endAt - Dict.size options.pointsToRemove
                                )

                            else
                                ( track.currentPosition - Dict.size options.pointsToRemove
                                , Just startAt
                                )

                        Nothing ->
                            ( preserveDistanceFromStart track.trackTree isTree track.currentPosition
                            , Nothing
                            )
            in
            { track
                | trackTree = Maybe.withDefault track.trackTree newTree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


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
                optionsWithRange =
                    case theTrack.markerPosition of
                        Just purple ->
                            { options
                                | range =
                                    Just
                                        ( min theTrack.currentPosition purple
                                        , max theTrack.currentPosition purple
                                        )
                            }

                        Nothing ->
                            { options | range = Nothing }

                populatedOptions =
                    findSimplifications optionsWithRange theTrack.trackTree
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
            ( options
            , [ WithUndo Actions.ApplySimplify
              , Actions.ApplySimplify
              , TrackHasChanged
              ]
            )

        FlushUndo ->
            ( options, [ Actions.FlushUndo ] )


view : SystemSettings -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view settings msgWrapper options isTrack =
    case isTrack of
        Just _ ->
            let
                i18n =
                    I18N.text settings.location toolId
            in
            column
                (CommonToolStyles.toolContentBoxStyle settings)
            <|
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
                                                (I18N.localisedString settings.location toolId "remove")
                                                [ String.fromInt quantity ]
                                        ]
                                }

                --, el [ centerX ] <|
                --    Input.button neatToolsBorder
                --        { onPress = Just <| msgWrapper FlushUndo
                --        , label =
                --            paragraph [] <|
                --                [ i18n "flush" ]
                --        }
                ]

        Nothing ->
            noTrackMessage settings
