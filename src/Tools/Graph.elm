module Tools.Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions exposing (ToolAction)
import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d
import Axis3d
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Color
import Dict exposing (Dict)
import Direction2d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, asRecord, skipCount, trueLength)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.ChinesePalette
import Geometry101
import Length exposing (Length, Meters, inMeters, meters)
import LineSegment2d
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels
import Point2d
import Point3d
import Polyline3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity, zero)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Set exposing (Set)
import SketchPlane3d
import SpatialIndex exposing (SpatialContent)
import String.Interpolate
import ToolTip exposing (localisedTooltip, myTooltip, tooltip)
import Tools.GraphOptions exposing (..)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Nudge
import Tools.NudgeOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (flatBox, showDecimal2, showLongMeasure, showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


toolId =
    "graph"


defaultOptions : Options msg
defaultOptions =
    { graph = emptyGraph
    , matchingTolerance = Length.meters 0.5
    , centreLineOffset = Length.meters 0.0
    , minimumRadiusAtPlaces = Length.meters 3.0
    , boundingBox = BoundingBox3d.singleton Point3d.origin
    , selectedTraversal = 0
    , analyzed = False
    , originalTrack = Nothing
    , editingTrack = 0
    , undoGraph = Nothing
    , undoOriginalTrack = Nothing
    , clustersForPreview = []
    , perpsForPreview = []
    , suggestedNewTrack = Nothing
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset (Quantity Float Meters)
    | MinimumRadius (Quantity Float Meters)
    | ConvertFromGraph
    | HighlightTraversal Int
    | RemoveLastTraversal
    | DisplayInfo String String
    | FlipDirection Int
    | ClearRoute
    | RevertToTrack
    | SetTolerance (Quantity Float Meters)
    | AdoptNewTrack


emptyGraph : Graph msg
emptyGraph =
    { nodes = Dict.empty
    , edges = Dict.empty
    , userRoute = []
    , referenceLonLat =
        { latitude = Angle.degrees 0
        , longitude = Direction2d.positiveX
        , altitude = Quantity.zero
        }
    }


makeXY : EarthPoint -> XY
makeXY earth =
    -- As in v2, allowing a tolerance of one foot albeit rather crudely.
    let
        ( x, y, _ ) =
            Point3d.toTuple Length.inFeet earth

        ( xRounded, yRounded ) =
            ( toFloat <| round x, toFloat <| round y )

        ( useX, useY ) =
            Point2d.fromTuple Length.feet ( xRounded, yRounded )
                |> Point2d.toTuple Length.inMeters
    in
    ( useX, useY )


edgeCanBeAdded : Int -> Options msg -> Bool
edgeCanBeAdded newEdge options =
    -- Edge can be added if either node is same as final node of last traversal,
    -- or if there are no traversals.
    case
        ( List.Extra.last options.graph.userRoute
        , Dict.get newEdge options.graph.edges
        )
    of
        ( Just { edge, direction }, Just ( ( n1, n2, xy ), _ ) ) ->
            case Dict.get edge options.graph.edges of
                Just ( ( start, end, via ), _ ) ->
                    let
                        finalNode =
                            if direction == Natural then
                                end

                            else
                                start
                    in
                    finalNode == n1 || finalNode == n2

                Nothing ->
                    False

        ( Nothing, Just ( ( n1, n2, xy ), _ ) ) ->
            True

        _ ->
            False


loopCanBeAdded : Int -> Options msg -> Bool
loopCanBeAdded node options =
    -- Loop can be added if node is same as final node of last traversal.
    case
        List.Extra.last options.graph.userRoute
    of
        Just { edge, direction } ->
            case Dict.get edge options.graph.edges of
                Just ( ( start, end, via ), _ ) ->
                    let
                        finalNode =
                            if direction == Natural then
                                end

                            else
                                start
                    in
                    finalNode == node

                Nothing ->
                    False

        Nothing ->
            False


edgeCanBeFlipped : Int -> Options msg -> Bool
edgeCanBeFlipped newEdge options =
    -- Edge can be flipped if either is only traversal or is self-loop.
    case
        ( options.graph.userRoute
        , Dict.get newEdge options.graph.edges
        )
    of
        ( [ { edge, direction } ], Just ( ( n1, n2, xy ), _ ) ) ->
            -- Can flip because only edge (if its the same edge
            edge == newEdge

        ( t1 :: t2 :: tN, Just ( ( n1, n2, xy ), _ ) ) ->
            -- Can be flipped if self-loop
            n1 == n2

        _ ->
            False


addTraversal : Int -> Options msg -> Options msg
addTraversal newEdge options =
    let
        graph =
            options.graph
    in
    case
        ( List.Extra.last graph.userRoute
        , Dict.get newEdge graph.edges
        )
    of
        ( Just { edge, direction }, Just ( ( n1, n2, xy ), _ ) ) ->
            case Dict.get edge graph.edges of
                Just ( ( start, end, via ), _ ) ->
                    let
                        finalNode =
                            if direction == Natural then
                                end

                            else
                                start

                        newEdgeDirection =
                            if finalNode == n1 then
                                Natural

                            else
                                Reverse

                        newGraph =
                            { graph
                                | userRoute =
                                    graph.userRoute ++ [ { edge = newEdge, direction = newEdgeDirection } ]
                            }
                    in
                    { options
                        | graph = newGraph
                        , selectedTraversal = List.length newGraph.userRoute - 1
                    }

                Nothing ->
                    options

        ( Nothing, Just ( ( n1, n2, xy ), _ ) ) ->
            let
                newGraph =
                    { graph
                        | userRoute =
                            graph.userRoute ++ [ { edge = newEdge, direction = Natural } ]
                    }
            in
            { options
                | graph = newGraph
                , selectedTraversal = List.length newGraph.userRoute - 1
            }

        _ ->
            options


addSelfLoop : Int -> Options msg -> Options msg
addSelfLoop node options =
    let
        graph =
            options.graph
    in
    case
        List.Extra.last graph.userRoute
    of
        Just { edge, direction } ->
            case Dict.get edge graph.edges of
                Just ( ( start, end, via ), track ) ->
                    let
                        ( finalNode, edgeDirection, endPoint ) =
                            if direction == Natural then
                                ( end
                                , DomainModel.getLastLeaf track.trackTree |> .directionAtEnd
                                , DomainModel.earthPointFromIndex (skipCount track.trackTree) track.trackTree
                                )

                            else
                                ( start
                                , DomainModel.getFirstLeaf track.trackTree
                                    |> .directionAtStart
                                    |> Direction2d.reverse
                                , DomainModel.earthPointFromIndex 0 track.trackTree
                                )

                        loopOpposite =
                            endPoint
                                |> Point3d.translateBy
                                    (Vector3d.withLength
                                        (Quantity.twice options.minimumRadiusAtPlaces)
                                        (edgeDirection |> Direction3d.on SketchPlane3d.xy)
                                    )

                        loopCentre =
                            Point3d.midpoint endPoint loopOpposite

                        axis =
                            Axis3d.withDirection Direction3d.positiveZ loopCentre

                        ( arcStart, arcEnd ) =
                            ( endPoint |> Point3d.rotateAround axis (Angle.degrees 30)
                            , endPoint |> Point3d.rotateAround axis (Angle.degrees -30)
                            )

                        arc =
                            Arc3d.throughPoints arcStart loopOpposite arcEnd
                    in
                    case arc of
                        Just isArc ->
                            let
                                edgePoints =
                                    isArc
                                        |> Arc3d.approximate (Length.meters 0.1)
                                        |> Polyline3d.vertices
                                        |> List.map (DomainModel.gpxFromPointWithReference graph.referenceLonLat)

                                newEdgeKey =
                                    ( node, node, makeXY loopOpposite )

                                newEdgeTree =
                                    DomainModel.treeFromSourcesWithExistingReference
                                        track.referenceLonLat
                                        edgePoints

                                newEdgeTrack =
                                    Maybe.map (TrackLoaded.newTrackFromTree track.referenceLonLat)
                                        newEdgeTree

                                newEdgeIndex =
                                    Dict.size graph.edges

                                newGraph =
                                    case newEdgeTrack of
                                        Just newTrack ->
                                            { graph
                                                | edges =
                                                    Dict.insert
                                                        newEdgeIndex
                                                        ( newEdgeKey, newTrack )
                                                        graph.edges
                                            }

                                        Nothing ->
                                            graph
                            in
                            { options | graph = newGraph }

                        Nothing ->
                            options

                Nothing ->
                    options

        Nothing ->
            options


changeActiveTrack : Int -> Options msg -> Options msg
changeActiveTrack edge options =
    { options | editingTrack = edge }


getTrack : Int -> Options msg -> Maybe (TrackLoaded msg)
getTrack edge options =
    Dict.get edge options.graph.edges
        |> Maybe.map Tuple.second


toolStateChange :
    Bool
    -> Element.Color
    -> Options msg
    -> Maybe (TrackLoaded msg)
    -> ( Options msg, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            let
                graph =
                    options.graph

                lastTrack =
                    Dict.get options.editingTrack graph.edges

                newGraph =
                    -- Graph must always refer to the latest version of the active track.
                    { graph
                        | edges =
                            case lastTrack of
                                Just ( nodeStuff, _ ) ->
                                    Dict.insert
                                        options.editingTrack
                                        ( nodeStuff, theTrack )
                                        graph.edges

                                Nothing ->
                                    graph.edges
                    }
            in
            ( { options | graph = newGraph }, [] )

        _ ->
            -- Hide preview
            ( options, [ Actions.HidePreview "graph" ] )


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options msg -> Element msg
view location imperial wrapper options =
    let
        i18n =
            I18N.text location toolId

        offset =
            Length.inMeters options.centreLineOffset

        radius =
            Length.inMeters options.minimumRadiusAtPlaces

        analyseButton =
            row [ spacing 3 ]
                [ infoButton (wrapper <| DisplayInfo "graph" "info")
                , I.button neatToolsBorder
                    { onPress = Just (wrapper GraphAnalyse)
                    , label = i18n "find"
                    }
                ]

        adoptTrackButton =
            row [ spacing 3 ]
                [ infoButton (wrapper <| DisplayInfo "graph" "adoptInfo")
                , I.button neatToolsBorder
                    { onPress = Just (wrapper AdoptNewTrack)
                    , label = i18n "adopt"
                    }
                ]

        clearRouteButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper ClearRoute)
                , label = i18n "clear"
                }

        revertButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper RevertToTrack)
                , label = i18n "revert"
                }

        finishButton =
            if List.length options.graph.userRoute > 0 then
                row [ spacing 3 ]
                    [ infoButton (wrapper <| DisplayInfo "graph" "render")
                    , I.button
                        neatToolsBorder
                        { onPress = Just (wrapper ConvertFromGraph)
                        , label = i18n "convert"
                        }
                    ]

            else
                none

        toleranceSlider =
            row [ spacing 5 ]
                [ none
                , infoButton (wrapper <| DisplayInfo "graph" "tolerance")
                , I.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetTolerance << Length.meters
                    , label =
                        I.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "isTolerance")
                                    [ showShortMeasure imperial options.matchingTolerance ]
                    , min = 0.0
                    , max = 5.0
                    , step = Nothing
                    , value = Length.inMeters options.matchingTolerance
                    , thumb = I.defaultThumb
                    }
                ]

        offsetSlider =
            row [ spacing 5 ]
                [ none
                , infoButton (wrapper <| DisplayInfo "graph" "offset")
                , I.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << CentreLineOffset << Length.meters
                    , label =
                        I.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "isOffset")
                                    [ showDecimal2 <| abs offset
                                    , if offset < 0.0 then
                                        I18N.localisedString location toolId "left"

                                      else if offset > 0.0 then
                                        I18N.localisedString location toolId "right"

                                      else
                                        ""
                                    ]
                    , min = -5.0
                    , max = 5.0
                    , step = Just 0.25
                    , value = offset
                    , thumb = I.defaultThumb
                    }
                ]

        minRadiusSlider =
            row [ spacing 5 ]
                [ none
                , infoButton (wrapper <| DisplayInfo "graph" "radius")
                , I.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << MinimumRadius << Length.meters
                    , label =
                        I.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "isRadius")
                                    [ showDecimal2 <| abs radius ]
                    , min = 1.0
                    , max = 15.0
                    , step = Just 1.0
                    , value = radius
                    , thumb = I.defaultThumb
                    }
                ]

        traversals : List TraversalDisplay
        traversals =
            -- Display-ready version of the route.
            let
                graph =
                    options.graph
            in
            graph.userRoute
                |> List.map
                    (\{ edge, direction } ->
                        let
                            edgeInfo =
                                Dict.get edge graph.edges
                        in
                        case edgeInfo of
                            Nothing ->
                                { startPlace = -1
                                , road = -1
                                , endPlace = -1
                                , length = Quantity.zero
                                }

                            Just ( ( n1, n2, _ ), track ) ->
                                case direction of
                                    Natural ->
                                        { startPlace = n1
                                        , road = edge
                                        , endPlace = n2
                                        , length = trueLength track.trackTree
                                        }

                                    Reverse ->
                                        { startPlace = n2
                                        , road = edge
                                        , endPlace = n1
                                        , length = trueLength track.trackTree
                                        }
                    )

        totalLength =
            traversals |> List.map .length |> Quantity.sum

        dataStyles selected =
            if selected then
                [ Font.color FlatColors.ChinesePalette.antiFlashWhite
                , Font.bold
                , Background.color rgtPurple
                , padding 2
                ]

            else
                [ Font.color rgtDark, padding 2 ]

        traversalsTable : Element msg
        traversalsTable =
            let
                headerAttrs =
                    [ Font.bold
                    , Font.color rgtDark
                    , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                    , Border.color rgtPurple
                    ]

                footerAttrs =
                    [ Font.bold
                    , Font.color rgtDark
                    , Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 }
                    , Border.color rgtPurple
                    ]
            in
            column
                [ width <| maximum 500 fill
                , height <| px 300
                , spacing 10
                , padding 5
                , Border.width 2
                , Border.rounded 6
                , Border.color rgtDark
                ]
                [ row [ width fill ]
                    [ el ((width <| fillPortion 1) :: headerAttrs) <| i18n "blank"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "from"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "to"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "along"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "distance"
                    ]

                -- workaround for a bug: it's necessary to wrap `table` in an `el`
                -- to get table height attribute to apply
                , el [ width fill ] <|
                    indexedTable
                        [ width fill
                        , height <| px 220
                        , scrollbarY
                        , spacing 4
                        ]
                        { data = traversals
                        , columns =
                            [ { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        row
                                            [ spacing 2 ]
                                            [ if i + 1 == List.length traversals then
                                                I.button
                                                    [ alignRight
                                                    , tooltip below (localisedTooltip location toolId "remove")
                                                    ]
                                                    { onPress = Just <| wrapper RemoveLastTraversal
                                                    , label = useIcon FeatherIcons.delete
                                                    }

                                              else
                                                none
                                            , if
                                                List.length traversals
                                                    == 1
                                                    || t.startPlace
                                                    == t.endPlace
                                              then
                                                I.button
                                                    [ alignRight
                                                    , tooltip below (localisedTooltip location toolId "reverse")
                                                    ]
                                                    { onPress = Just <| wrapper <| FlipDirection i
                                                    , label = useIcon FeatherIcons.refreshCw
                                                    }

                                              else
                                                none
                                            , I.button
                                                [ alignRight ]
                                                { onPress = Just <| wrapper <| HighlightTraversal i
                                                , label = useIcon FeatherIcons.eye
                                                }
                                            ]
                              }
                            , { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        el (dataStyles (i == options.selectedTraversal)) <|
                                            text <|
                                                String.Interpolate.interpolate
                                                    (I18N.localisedString location toolId "place1")
                                                    [ String.fromInt t.startPlace ]
                              }
                            , { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        el (dataStyles (i == options.selectedTraversal)) <|
                                            text <|
                                                String.Interpolate.interpolate
                                                    (I18N.localisedString location toolId "place2")
                                                    [ String.fromInt t.endPlace ]
                              }
                            , { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        el (dataStyles (i == options.selectedTraversal)) <|
                                            text <|
                                                String.Interpolate.interpolate
                                                    (I18N.localisedString location toolId "road")
                                                    [ String.fromInt t.road ]
                              }
                            , { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        el (dataStyles (i == options.selectedTraversal)) <|
                                            text <|
                                                showLongMeasure False t.length
                              }
                            ]
                        }
                , row [ width fill ]
                    [ el ((width <| fillPortion 1) :: footerAttrs) <| text " "
                    , el ((width <| fillPortion 2) :: footerAttrs) <| text " "
                    , el ((width <| fillPortion 2) :: footerAttrs) <| text " "
                    , el ((width <| fillPortion 2) :: footerAttrs) <| text " "
                    , el ((width <| fillPortion 2) :: footerAttrs) <|
                        text <|
                            showLongMeasure False totalLength
                    ]
                ]

        traversalNext =
            I.button neatToolsBorder
                { onPress =
                    Just <|
                        wrapper <|
                            HighlightTraversal <|
                                min (List.length traversals - 1) (options.selectedTraversal + 1)
                , label = useIconWithSize 16 FeatherIcons.chevronRight
                }

        traversalPrevious =
            I.button neatToolsBorder
                { onPress =
                    Just <|
                        wrapper <|
                            HighlightTraversal <|
                                max 0 (options.selectedTraversal - 1)
                , label = useIconWithSize 16 FeatherIcons.chevronLeft
                }
    in
    el
        [ width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , padding 4
        ]
    <|
        if options.analyzed then
            column [ width fill, padding 4, spacing 10 ]
                [ row [ centerX, width fill, spacing 10 ]
                    [ traversalPrevious
                    , traversalNext
                    , clearRouteButton
                    , revertButton
                    ]
                , traversalsTable
                , wrappedRow [ spacing 5 ] [ offsetSlider, minRadiusSlider ]
                , finishButton
                ]

        else
            wrappedRow [ centerX, width fill, spacing 10 ]
                [ toleranceSlider
                , adoptTrackButton
                , analyseButton
                ]


type alias LeafIndexEntry =
    { leafIndex : Int }


type alias LeafIndex =
    SpatialIndex.SpatialNode LeafIndexEntry Length.Meters LocalCoords


type alias PointIndexEntry =
    { pointIndex : Int }


type alias PointIndex =
    SpatialIndex.SpatialNode PointIndexEntry Length.Meters LocalCoords


identifyPointsToBeMerged : Length.Length -> TrackLoaded msg -> ( List InsertedPointOnLeaf, PeteTree )
identifyPointsToBeMerged tolerance track =
    {-
       Data flow outline.
       1. Make spatial indexes of points and leaves, for quick but approximate nearness queries.
       2. For each point, look for leaves within tolerance.
           a. For each such leaf, get distance along leaf and foot of perpendicular.
       3. Collect these perpendicular "feet" by Leaf, sorted by `distanceAlong`.
       4. Update tree by converting each affected Leaf into a shrub (in situ perhaps?).
       Now have tree', enhanced by "virtual points" where leaves are close to points.
       5. For each point, find nearby points within tolerance.
       6. Sort "points with vicini" in descending order of vicini count.
       7. Work through this sorted list, forming groups (a set of previously unclaimed vicini).
       8. For each cluster, derive the centroid.
       9. For each point in all clusters, derive mapping to centroid.
       10. Apply mappings by updating points (in situ perhaps?).
       Now have tree'' which has adjusted points at cluster centroids.
       Proof of pudding awaited ...
    -}
    let
        addTolerance box =
            BoundingBox2d.expandBy tolerance box

        growBox =
            -- We just make our index area slightly larger but I forget why.
            addTolerance <|
                flatBox <|
                    DomainModel.boundingBox track.trackTree

        pointWithTolerance pt =
            addTolerance <|
                BoundingBox2d.singleton <|
                    Point3d.projectInto SketchPlane3d.xy pt

        emptyPointIndex : PointIndex
        emptyPointIndex =
            -- The last parameter here is not the quality, it
            -- only affects the index efficiency.
            SpatialIndex.empty growBox (Length.meters 100.0)

        emptyLeafIndex : LeafIndex
        emptyLeafIndex =
            -- The last parameter here is not the quality, it
            -- only affects the index efficiency.
            SpatialIndex.empty growBox (Length.meters 100.0)

        {-
           1. Make spatial indexes of points and leaves, for quick but approximate nearness queries.
        -}
        ( _, pointIndex ) =
            -- Pre-pop with first point so the fold can focus on the leaf end points.
            DomainModel.foldOverRoute
                indexPoint
                track.trackTree
                ( 1
                , SpatialIndex.add
                    { content = { pointIndex = 0 }
                    , box =
                        pointWithTolerance <|
                            DomainModel.earthPointFromIndex 0 track.trackTree
                    }
                    emptyPointIndex
                )

        ( _, leafIndex ) =
            -- Pre-pop with first point so the fold can focus on the leaf end points.
            DomainModel.foldOverRoute
                indexLeaf
                track.trackTree
                ( 0
                , SpatialIndex.add
                    { content = { leafIndex = 0 }
                    , box = localBounds <| DomainModel.getFirstLeaf track.trackTree
                    }
                    emptyLeafIndex
                )

        localBounds road =
            -- Use to form a query for each leaf.
            DomainModel.boundingBox (Leaf road)
                |> flatBox
                |> addTolerance

        indexPoint : RoadSection -> ( Int, PointIndex ) -> ( Int, PointIndex )
        indexPoint leaf ( pointNumber, indexBuild ) =
            ( pointNumber + 1
            , SpatialIndex.add
                { content = { pointIndex = pointNumber }
                , box =
                    BoundingBox2d.singleton <|
                        Point3d.projectInto SketchPlane3d.xy leaf.endPoint
                }
                indexBuild
            )

        indexLeaf : RoadSection -> ( Int, LeafIndex ) -> ( Int, LeafIndex )
        indexLeaf leaf ( leafNumber, indexBuild ) =
            ( leafNumber + 1
            , SpatialIndex.add
                { content = { leafIndex = leafNumber }
                , box = localBounds leaf
                }
                indexBuild
            )

        pointsNearPoint : EarthPoint -> List Int
        pointsNearPoint pt =
            -- Prelude to finding clusters of points.
            -- Use spatial point index then refine with geometry.
            let
                thisPoint2d =
                    Point3d.projectInto SketchPlane3d.xy pt

                results =
                    SpatialIndex.query pointIndex (pointWithTolerance pt)
                        |> List.map (.content >> .pointIndex)
            in
            results
                |> List.filter
                    (\ptidx ->
                        DomainModel.earthPointFromIndex ptidx track.trackTree
                            |> Point3d.projectInto SketchPlane3d.xy
                            |> Point2d.equalWithin tolerance thisPoint2d
                    )

        {-
           2. For each point, look for leaves within tolerance.
               a. For each such leaf, get distance along leaf and foot of perpendicular.
        -}
        findNearbyLeaves : Int -> List InsertedPointOnLeaf
        findNearbyLeaves pointNumber =
            -- Use spatial leaf index then refine with geometry.
            let
                pt =
                    DomainModel.earthPointFromIndex pointNumber track.trackTree

                thisPoint2d =
                    Point3d.projectInto SketchPlane3d.xy pt

                results =
                    SpatialIndex.query leafIndex (pointWithTolerance pt)
                        |> List.map (.content >> .leafIndex)
                        |> List.Extra.unique

                isThisLeafClose : Int -> Maybe InsertedPointOnLeaf
                isThisLeafClose leafNumber =
                    let
                        leaf =
                            asRecord <|
                                DomainModel.leafFromIndex leafNumber track.trackTree

                        axis2d =
                            Axis2d.through
                                (leaf.startPoint |> Point3d.projectInto SketchPlane3d.xy)
                                leaf.directionAtStart

                        axis3d =
                            Axis3d.throughPoints leaf.startPoint leaf.endPoint
                                |> Maybe.withDefault Axis3d.z

                        ( along, from, foot ) =
                            -- Proximity test in 2D as altitudes may vary greatly.
                            ( pt |> Point3d.signedDistanceAlong axis3d
                            , thisPoint2d |> Point2d.signedDistanceFrom axis2d
                            , pt |> Point3d.projectOntoAxis axis3d
                            )

                        isShortPerp =
                            from |> Quantity.lessThanOrEqualTo tolerance

                        isAfterStart =
                            along |> Quantity.greaterThanZero

                        isBeforeEnd =
                            along |> Quantity.lessThan (trueLength (Leaf leaf))

                        isNotConnected =
                            leafNumber /= pointNumber && leafNumber + 1 /= pointNumber
                    in
                    if isNotConnected && isShortPerp && isAfterStart && isBeforeEnd then
                        Just
                            { sourcePointNumber = pointNumber
                            , leafNumber = leafNumber
                            , distanceAlong = along
                            , earthPoint = foot
                            }

                    else
                        Nothing

                geometricallyClose =
                    results |> List.filterMap isThisLeafClose
            in
            geometricallyClose

        --|> List.Extra.uniqueBy .leafNumber
        findNearbyLeavesFoldFn :
            RoadSection
            -> ( Int, List InsertedPointOnLeaf )
            -> ( Int, List InsertedPointOnLeaf )
        findNearbyLeavesFoldFn road ( leafNumber, outputs ) =
            ( leafNumber + 1
            , case findNearbyLeaves (leafNumber + 1) of
                [] ->
                    outputs

                nearby ->
                    nearby ++ outputs
            )

        --findAllNearbyLeaves : ( Int, List ( Int, List InsertedPointOnLeaf ) )
        ( _, findAllNearbyLeaves ) =
            -- Want "nearby" for all points. Our model traverses leaves, so we
            -- preload the start point and use the end point of each leaf.
            DomainModel.foldOverRoute
                findNearbyLeavesFoldFn
                track.trackTree
                ( 0
                , findNearbyLeaves 0
                )

        _ =
            Debug.log "LEAVES" findAllNearbyLeaves

        {-
           We have the feet of the perpendiculars to nearby leaves.
           3. Collect these perpendicular "feet" by Leaf, sorted by `distanceAlong`.
        -}
        perpendicularFeetGroupedByLeaf : Dict Int (List InsertedPointOnLeaf)
        perpendicularFeetGroupedByLeaf =
            -- Can't see a suitable function in List.Extra, so do it by hand.
            let
                addToLeafDict newEntry dict =
                    case Dict.get newEntry.leafNumber dict of
                        Just prevEntries ->
                            Dict.insert
                                newEntry.leafNumber
                                (newEntry :: prevEntries)
                                dict

                        Nothing ->
                            Dict.insert
                                newEntry.leafNumber
                                [ newEntry ]
                                dict

                leafDictUnsorted =
                    List.foldl
                        addToLeafDict
                        Dict.empty
                        findAllNearbyLeaves

                sortEachLeafEntries : Int -> List InsertedPointOnLeaf -> List InsertedPointOnLeaf
                sortEachLeafEntries _ unsorted =
                    unsorted
                        |> List.Extra.uniqueBy (.distanceAlong >> Length.inMeters)
                        |> List.sortBy (.distanceAlong >> Length.inMeters)
            in
            Dict.map sortEachLeafEntries leafDictUnsorted

        {-
           4. Update tree by converting each affected Leaf into a "branch" (in situ perhaps?).
           Now have tree', enhanced by "virtual points" where leaves are close to points.
        -}
        insertPointsInLeaf : Int -> List InsertedPointOnLeaf -> PeteTree -> PeteTree
        insertPointsInLeaf leafNumber newPoints tree =
            let
                asGPX =
                    newPoints
                        |> List.map (.earthPoint >> DomainModel.gpxFromPointWithReference track.referenceLonLat)
            in
            DomainModel.insertPointsIntoLeaf
                leafNumber
                track.referenceLonLat
                asGPX
                track.trackTree

        treeWithAddedPoints : PeteTree
        treeWithAddedPoints =
            Dict.foldl
                insertPointsInLeaf
                track.trackTree
                perpendicularFeetGroupedByLeaf

        identifyMappings :
            RoadSection
            -> ( Int, List MappedPoint )
            -> ( Int, List MappedPoint )
        identifyMappings road ( subjectPointIndex, existingMappings ) =
            ( subjectPointIndex + 1, existingMappings )

        ( _, mappingsForRoute ) =
            DomainModel.foldOverRoute
                identifyMappings
                track.trackTree
                ( 0, [] )

        _ =
            Debug.log "MAPPINGS" mappingsForRoute
    in
    ( findAllNearbyLeaves
    , treeWithAddedPoints
    )


update :
    Msg
    -> Options msg
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options msg, List (Actions.ToolAction msg) )
update msg options track wrapper =
    case msg of
        AdoptNewTrack ->
            ( options, [ Actions.CombineNearbyPoints, Actions.TrackHasChanged ] )

        GraphAnalyse ->
            let
                newOptions =
                    { options
                        | graph = buildGraph track
                        , analyzed = True
                        , originalTrack = Just track
                    }
            in
            ( newOptions
            , if Dict.size newOptions.graph.nodes > skipCount track.trackTree // 10 then
                [ Actions.DisplayInfo "graph" "manyNodes" ]

              else
                []
            )

        RevertToTrack ->
            ( { options
                | graph =
                    case options.originalTrack of
                        Just original ->
                            trivialGraph original

                        Nothing ->
                            -- Oh dear!
                            options.graph
                , analyzed = False
                , originalTrack = Nothing
                , editingTrack = 0
                , selectedTraversal = 0
              }
            , [ Actions.ChangeActiveTrack 0, Actions.TrackHasChanged ]
            )

        HighlightTraversal traversal ->
            ( { options | selectedTraversal = traversal }, [] )

        RemoveLastTraversal ->
            let
                graph =
                    options.graph

                newGraph =
                    { graph
                        | userRoute =
                            List.take (List.length graph.userRoute - 1) graph.userRoute
                    }
            in
            ( { options
                | graph = newGraph
                , selectedTraversal = List.length newGraph.userRoute - 1
              }
            , []
            )

        FlipDirection i ->
            let
                graph =
                    options.graph

                newGraph =
                    { graph
                        | userRoute =
                            graph.userRoute
                                |> List.Extra.updateAt i
                                    (\t ->
                                        { t
                                            | direction =
                                                case t.direction of
                                                    Natural ->
                                                        Reverse

                                                    Reverse ->
                                                        Natural
                                        }
                                    )
                    }
            in
            ( { options
                | graph = newGraph
                , selectedTraversal = min options.selectedTraversal (List.length newGraph.userRoute - 1)
              }
            , []
            )

        SetTolerance tolerance ->
            let
                ( pointInformation, suggested ) =
                    identifyPointsToBeMerged options.matchingTolerance track
            in
            ( { options
                | matchingTolerance = tolerance
                , perpsForPreview = pointInformation
                , suggestedNewTrack = Just suggested
              }
            , [ Actions.ShowPreview
                    { tag = "graph"
                    , shape = PreviewToolSupplied <| showNewPoints pointInformation track
                    , colour = FlatColors.AmericanPalette.sourLemon
                    , points = []
                    }
              ]
            )

        CentreLineOffset float ->
            ( { options | centreLineOffset = float }, [] )

        MinimumRadius float ->
            ( { options | minimumRadiusAtPlaces = float }, [] )

        ConvertFromGraph ->
            ( options, [ Actions.MakeRouteFromGraph, Actions.TrackHasChanged ] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        ClearRoute ->
            let
                graph =
                    options.graph

                newGraph =
                    { graph | userRoute = [] }
            in
            ( { options
                | graph = newGraph
                , selectedTraversal = 0
              }
            , []
            )


type alias EdgeFinder msg =
    { startNodeIndex : Int
    , currentEdge : List ( EarthPoint, GPXSource )
    , edgeResolverDict : Dict ( Int, Int, XY ) ( Int, PeteTree )
    , edgesDict : Dict Int ( ( Int, Int, XY ), TrackLoaded msg )
    , traversals : List Traversal
    }


showNewPoints : List InsertedPointOnLeaf -> TrackLoaded msg -> List (Entity LocalCoords)
showNewPoints pointInfo track =
    let
        locations =
            pointInfo |> List.map .earthPoint

        material =
            Material.color Color.blue

        highlightPoint point =
            Scene3d.point { radius = Pixels.pixels 5 } material point

        showVector newPoint oldPoint =
            Scene3d.lineSegment
                material
                (LineSegment3d.from oldPoint newPoint.adjustedPoint)
    in
    List.map highlightPoint locations


buildGraph : TrackLoaded msg -> Graph msg
buildGraph track =
    {-
       As in v1 & 2, the only way I know is to see which track points have more than two neighbours.
       Hence build a Dict using XY and the entries being a list of points that share the location.
    -}
    let
        countNeighbours : RoadSection -> Dict XY (Set XY) -> Dict XY (Set XY)
        countNeighbours road countDict =
            -- Nicer than v2 thanks to use of road segments.
            -- Note we are interested in neighbours with distinct XYs.
            let
                ( startXY, endXY ) =
                    ( makeXY road.startPoint
                    , makeXY road.endPoint
                    )

                ( startNeighbours, endNeighbours ) =
                    ( Dict.get startXY countDict |> Maybe.withDefault Set.empty
                    , Dict.get endXY countDict |> Maybe.withDefault Set.empty
                    )
            in
            countDict
                |> Dict.insert startXY (Set.insert endXY startNeighbours)
                |> Dict.insert endXY (Set.insert startXY endNeighbours)

        pointNeighbours : Dict XY (Set XY)
        pointNeighbours =
            -- What neighbours hath each track point?
            -- Note that the List.head will be earliest in the route, hence preferred.
            DomainModel.foldOverRouteRL
                countNeighbours
                track.trackTree
                Dict.empty

        ( trackStartXY, trackEndXY ) =
            ( makeXY <| DomainModel.earthPointFromIndex 0 track.trackTree
            , makeXY <| DomainModel.earthPointFromIndex (skipCount track.trackTree) track.trackTree
            )

        nodes =
            -- Two neighbours is just an edge point, anything else is a node.
            -- But make sure the endpoints are there, as loops can cause a problem here.
            pointNeighbours
                |> Dict.filter
                    (\pt neighbours ->
                        Set.size neighbours
                            /= 2
                            || pt
                            == trackStartXY
                            || pt
                            == trackEndXY
                    )
                |> Dict.keys
                |> List.indexedMap Tuple.pair
                |> Dict.fromList

        swap ( a, b ) =
            ( b, a )

        inverseNodes : Dict XY Int
        inverseNodes =
            -- We need to lookup each point to see if it's a node.
            nodes |> Dict.toList |> List.map swap |> Dict.fromList

        ( firstPoint, firstGpx ) =
            DomainModel.getDualCoords track.trackTree 0

        finalEdgeFinder : EdgeFinder msg
        finalEdgeFinder =
            {-
               Walk the route again, but check each point against node index.
               If not a node, accrue a possible new edge.
               If a node, look into edge dict to see if we have already an Edge
               for the node pair, but note that that is not unique.
               The real test for an edge is whether all (or sample) of trackpoints
               coincide, forwards or backward. We can reduce the testing to one-way
               by convention of always putting lower node index first in dict lookup.
            -}
            DomainModel.foldOverRoute
                splitIntoEdges
                track.trackTree
                initialEdgeFinder

        initialEdgeFinder : EdgeFinder msg
        initialEdgeFinder =
            { startNodeIndex = Dict.get (makeXY firstPoint) inverseNodes |> Maybe.withDefault 0
            , currentEdge = [ ( firstPoint, firstGpx ) ]
            , edgeResolverDict = Dict.empty
            , edgesDict = Dict.empty
            , traversals = []
            }

        splitIntoEdges :
            RoadSection
            -> EdgeFinder msg
            -> EdgeFinder msg
        splitIntoEdges road inputState =
            let
                pointXY =
                    makeXY road.endPoint

                pointGpx =
                    Tuple.second road.sourceData
            in
            case Dict.get pointXY inverseNodes of
                Nothing ->
                    -- Not a node, just add to current edge.
                    { inputState | currentEdge = ( road.endPoint, pointGpx ) :: inputState.currentEdge }

                Just nodeIndex ->
                    -- At a node, completed an edge, but have we encountered this edge before?
                    let
                        newEdge : List ( EarthPoint, GPXSource )
                        newEdge =
                            ( road.endPoint, pointGpx ) :: inputState.currentEdge

                        orientedEdgeCouldBeLeaf : List ( EarthPoint, GPXSource )
                        orientedEdgeCouldBeLeaf =
                            if nodeIndex > inputState.startNodeIndex then
                                -- Conventional order, good, but must flip the edge
                                List.reverse newEdge

                            else
                                newEdge

                        orientedEdge : List ( EarthPoint, GPXSource )
                        orientedEdge =
                            -- Not good if no midpoints, as can't select.
                            case orientedEdgeCouldBeLeaf of
                                [ ( startEarth, startGpx ), ( endEarth, endGpx ) ] ->
                                    let
                                        midEarth =
                                            Point3d.midpoint startEarth endEarth

                                        midGpx =
                                            DomainModel.gpxFromPointWithReference
                                                track.referenceLonLat
                                                midEarth
                                    in
                                    [ ( startEarth, startGpx )
                                    , ( midEarth, midGpx )
                                    , ( endEarth, endGpx )
                                    ]

                                _ ->
                                    orientedEdgeCouldBeLeaf

                        discriminator : XY
                        discriminator =
                            -- As there can be more than one edge 'tween  two nodes,
                            -- we take the index 1 point to discriminate. That's why
                            -- the edge orientation matters.
                            List.Extra.getAt 1 orientedEdge
                                |> Maybe.map Tuple.first
                                |> Maybe.map makeXY
                                |> Maybe.withDefault pointXY

                        ( lowNode, highNode ) =
                            ( min inputState.startNodeIndex nodeIndex
                            , max inputState.startNodeIndex nodeIndex
                            )
                    in
                    case Dict.get ( lowNode, highNode, discriminator ) inputState.edgeResolverDict of
                        Just ( edgeIndex, edgeTree ) ->
                            -- So, we don't add this edge
                            -- but we record the traversal
                            let
                                traversal =
                                    { edge = edgeIndex
                                    , direction =
                                        if lowNode == inputState.startNodeIndex then
                                            Natural

                                        else
                                            Reverse
                                    }
                            in
                            { inputState
                                | startNodeIndex = nodeIndex
                                , currentEdge = [ ( road.endPoint, pointGpx ) ]
                                , traversals = traversal :: inputState.traversals
                            }

                        Nothing ->
                            -- We put this into the resolver dictionary to check for reuse,
                            -- and into the outputs dictionary,
                            -- _and_ we record the traversal.
                            let
                                newEdgeKey =
                                    ( lowNode, highNode, discriminator )

                                newEdgeTree =
                                    DomainModel.treeFromSourcesWithExistingReference
                                        track.referenceLonLat
                                        (List.map Tuple.second orientedEdge)
                                        |> Maybe.withDefault (Leaf road)

                                newEdgeTrack =
                                    TrackLoaded.newTrackFromTree
                                        track.referenceLonLat
                                        newEdgeTree

                                newEdgeIndex =
                                    Dict.size inputState.edgesDict

                                traversal =
                                    { edge = newEdgeIndex
                                    , direction =
                                        if lowNode == inputState.startNodeIndex then
                                            Natural

                                        else
                                            Reverse
                                    }
                            in
                            { startNodeIndex = nodeIndex
                            , currentEdge = [ ( road.endPoint, pointGpx ) ]
                            , edgeResolverDict =
                                Dict.insert
                                    newEdgeKey
                                    ( newEdgeIndex, newEdgeTree )
                                    inputState.edgeResolverDict
                            , edgesDict =
                                Dict.insert
                                    newEdgeIndex
                                    ( newEdgeKey, newEdgeTrack )
                                    inputState.edgesDict
                            , traversals = traversal :: inputState.traversals
                            }
    in
    { nodes = nodes
    , edges = finalEdgeFinder.edgesDict
    , userRoute = List.reverse finalEdgeFinder.traversals
    , referenceLonLat = track.referenceLonLat
    }


trivialGraph : TrackLoaded msg -> Graph msg
trivialGraph track =
    {-
       This just gives us the start and end points, maybe one node if track is looped.
       It's a good place to start and means we can then start visualising.
    -}
    let
        ( startNode, endNode, discriminator ) =
            ( DomainModel.earthPointFromIndex 0 track.trackTree
            , DomainModel.earthPointFromIndex (skipCount track.trackTree) track.trackTree
            , DomainModel.earthPointFromIndex 1 track.trackTree
            )

        nodes =
            Dict.fromList
                [ ( 0, makeXY startNode ), ( 2, makeXY endNode ) ]

        edges =
            Dict.fromList
                [ ( 0, ( ( 1, 2, makeXY discriminator ), track ) ) ]

        traversal =
            { edge = 0, direction = Natural }
    in
    { nodes = nodes
    , edges = edges
    , userRoute = [ traversal ]
    , referenceLonLat = track.referenceLonLat
    }


type alias Junction =
    { arc : Maybe (Arc3d Meters LocalCoords)
    , trim : Quantity Float Meters
    }


combineNearbyPoints : Options msg -> TrackLoaded msg -> PeteTree
combineNearbyPoints options track =
    options.suggestedNewTrack |> Maybe.withDefault track.trackTree


makeNewRoute : Options msg -> Options msg
makeNewRoute options =
    {-
       This will walk the route, apply offset, push the old track on the Undo stack
       and then become a "trivialGraph" of the new route.
       Also note we nudge down by 1cm any edges that are revisited.
       Don't forget to push the old points on Undo stack.

        Nope, not doing a fold at traversal level since it gets unduly complicated.
        We need to consider a traversal and both its neighbours, to work out any edge
        shortening, so a triple map is conceptually easier, and a simple recursive
        function more so.
    -}
    let
        graph =
            options.graph

        useNudgeTool nudgeOption track index =
            -- Simple wrapper to use internal operation in Nudge; not efficient!
            Tools.Nudge.nudgeTrackPoint
                nudgeOption
                1.0
                index
                track

        dummyJunction : Junction
        dummyJunction =
            { arc = Nothing, trim = Quantity.zero }

        junctions : List Junction
        junctions =
            -- Took me so long to see this. Getting old?
            -- There will be one fewer junctions than edges.
            List.map2
                computeJunction
                graph.userRoute
                (List.drop 1 graph.userRoute)

        trim =
            -- CHANGE. Actually prune the trees to get the right leaf.
            -- Will need to do this in the traversal rendering as well.
            -- This changes the "minimum radius" semantics.
            options.minimumRadiusAtPlaces

        renderedArcs : List (List EarthPoint)
        renderedArcs =
            List.map renderJunction junctions

        isNotFirstUseOfEdge : List Bool
        isNotFirstUseOfEdge =
            -- Good practice for RGT is to depress subsequent edge pass by 1cm; avoids flicker.
            let
                ( _, flags ) =
                    List.foldl
                        (\{ edge, direction } ( traversed, outputs ) ->
                            ( Set.insert edge traversed
                            , Set.member edge traversed :: outputs
                            )
                        )
                        ( Set.empty, [] )
                        graph.userRoute
            in
            List.reverse flags

        trimmedTraversals : List (List EarthPoint)
        trimmedTraversals =
            List.map4
                trimTraversal
                (dummyJunction :: junctions)
                graph.userRoute
                isNotFirstUseOfEdge
                (junctions ++ [ dummyJunction ])

        trimTraversal : Junction -> Traversal -> Bool -> Junction -> List EarthPoint
        trimTraversal preceding { edge, direction } repetition following =
            -- Emit offset points but not within the trim areas.
            case Dict.get edge graph.edges of
                Just ( ( n1, n2, via ), track ) ->
                    -- It's a real edge, offset flipped if reversed.
                    -- Compute offset points on unflipped edge then flip if reversed.
                    let
                        correctedOffset =
                            case direction of
                                Natural ->
                                    options.centreLineOffset

                                Reverse ->
                                    Quantity.negate options.centreLineOffset

                        ( firstOffsetIndex, lastOffsetIndex ) =
                            -- Other than start and end of route, trim back the edge to allow for the Place arc.
                            ( DomainModel.indexFromDistance trim track.trackTree + 1
                            , DomainModel.indexFromDistance
                                (trueLength track.trackTree |> Quantity.minus trim)
                                track.trackTree
                                - 1
                            )

                        defaultNudge =
                            Tools.Nudge.defaultOptions

                        nudgeOptions : Tools.NudgeOptions.Options
                        nudgeOptions =
                            { defaultNudge
                                | horizontal = correctedOffset
                                , vertical =
                                    if repetition then
                                        Length.centimeters -1

                                    else
                                        Quantity.zero
                            }

                        nudgedPoints =
                            List.range firstOffsetIndex lastOffsetIndex
                                |> List.map (useNudgeTool nudgeOptions track.trackTree)
                    in
                    case direction of
                        Natural ->
                            nudgedPoints

                        Reverse ->
                            List.reverse nudgedPoints

                Nothing ->
                    []

        newEarthPoints =
            List.take 1 trimmedTraversals
                ++ List.Extra.interweave renderedArcs (List.drop 1 trimmedTraversals)

        computeJunction : Traversal -> Traversal -> Junction
        computeJunction inbound outbound =
            -- This is the bit of new geometry. We need the "end" direction of the inbound edge
            -- (allowing for Direction) and the "start" direction of the outbound.
            -- We work out the arc needed to give the minimum radius at centre-line.
            -- We work out how much this impedes on the edges (the "trim").
            -- We compute the "arc" according to the direction and offset.
            -- Note we actually make a 2D arc, then interpolate altitudes to get 3D.
            -- First thing is all the necessary dictionary lookups.
            case ( Dict.get inbound.edge graph.edges, Dict.get outbound.edge graph.edges ) of
                ( Just ( _, inTrack ), Just ( _, outTrack ) ) ->
                    let
                        actualVertex =
                            case inbound.direction of
                                Natural ->
                                    DomainModel.earthPointFromIndex
                                        (skipCount inTrack.trackTree)
                                        inTrack.trackTree

                                Reverse ->
                                    DomainModel.earthPointFromIndex
                                        0
                                        inTrack.trackTree

                        ( inboundTrimIndex, inboundTrimPoint ) =
                            case inbound.direction of
                                Natural ->
                                    DomainModel.interpolateTrack
                                        (trueLength inTrack.trackTree |> Quantity.minus trim)
                                        inTrack.trackTree

                                Reverse ->
                                    DomainModel.interpolateTrack
                                        trim
                                        inTrack.trackTree

                        ( outboundTrimIndex, outboundTrimPoint ) =
                            case outbound.direction of
                                Natural ->
                                    DomainModel.interpolateTrack
                                        trim
                                        outTrack.trackTree

                                Reverse ->
                                    DomainModel.interpolateTrack
                                        (trueLength outTrack.trackTree |> Quantity.minus trim)
                                        outTrack.trackTree

                        ( inboundDirection, outboundDirection ) =
                            ( Direction3d.from inboundTrimPoint actualVertex
                                |> Maybe.withDefault Direction3d.positiveZ
                                |> Direction3d.projectInto planeFor2dArc
                                |> Maybe.withDefault Direction2d.positiveX
                            , Direction3d.from actualVertex outboundTrimPoint
                                |> Maybe.withDefault Direction3d.positiveZ
                                |> Direction3d.projectInto planeFor2dArc
                                |> Maybe.withDefault Direction2d.positiveX
                            )

                        ( inboundRoad, outboundRoad ) =
                            ( LineSegment3d.from inboundTrimPoint actualVertex
                            , LineSegment3d.from actualVertex outboundTrimPoint
                            )

                        turnAngle =
                            Direction2d.angleFrom inboundDirection outboundDirection

                        ( offsetVectorInbound, offsetVectorOutbound ) =
                            ( Vector2d.withLength options.centreLineOffset
                                (Direction2d.rotateClockwise inboundDirection)
                            , Vector2d.withLength options.centreLineOffset
                                (Direction2d.rotateClockwise outboundDirection)
                            )

                        meanHeight =
                            Quantity.half <|
                                Quantity.plus
                                    (Point3d.zCoordinate inboundTrimPoint)
                                    (Point3d.zCoordinate outboundTrimPoint)

                        -- If we now apply offset to the start and end (which we can), we
                        -- make the offset arc not the centre line arc here.
                        planeFor2dArc =
                            SketchPlane3d.xy
                                |> SketchPlane3d.translateBy (Vector3d.xyz Quantity.zero Quantity.zero meanHeight)

                        ( inboundTrim2d, outboundTrim2d ) =
                            ( inboundTrimPoint |> Point3d.projectInto planeFor2dArc
                            , outboundTrimPoint |> Point3d.projectInto planeFor2dArc
                            )

                        ( offsetInboundTrimPoint, offsetOutboundTrimPoint ) =
                            ( inboundTrim2d |> Point2d.translateBy offsetVectorInbound
                            , outboundTrim2d |> Point2d.translateBy offsetVectorOutbound
                            )

                        ( inboundRoad2d, outboundRoad2d ) =
                            ( inboundRoad |> LineSegment3d.projectInto planeFor2dArc
                            , outboundRoad |> LineSegment3d.projectInto planeFor2dArc
                            )

                        geometryPoint point =
                            Point2d.toRecord inMeters point

                        lineEquationFromSegment segment =
                            Geometry101.lineEquationFromTwoPoints
                                (geometryPoint <| LineSegment2d.startPoint segment)
                                (geometryPoint <| LineSegment2d.endPoint segment)

                        ( inboundLineEquation, outboundLineEquation ) =
                            ( lineEquationFromSegment inboundRoad2d
                            , lineEquationFromSegment outboundRoad2d
                            )

                        ( perpToInbound, perToOutbound ) =
                            ( Geometry101.linePerpendicularTo
                                inboundLineEquation
                                (geometryPoint inboundTrim2d)
                            , Geometry101.linePerpendicularTo
                                outboundLineEquation
                                (geometryPoint outboundTrim2d)
                            )

                        arcCentre =
                            Maybe.map (Point2d.fromRecord meters) <|
                                Geometry101.lineIntersection
                                    perpToInbound
                                    perToOutbound

                        arc : Maybe (Arc2d Meters LocalCoords)
                        arc =
                            case arcCentre of
                                Just centre ->
                                    Just <| Arc2d.sweptAround centre turnAngle offsetInboundTrimPoint

                                Nothing ->
                                    Nothing
                    in
                    -- We make a 3d arc through the same points.
                    case arc of
                        Just foundArc ->
                            { arc = Just <| Arc3d.on planeFor2dArc foundArc
                            , trim = trim
                            }

                        Nothing ->
                            dummyJunction

                _ ->
                    dummyJunction

        renderJunction : Junction -> List EarthPoint
        renderJunction junction =
            case junction.arc of
                Just arc ->
                    arc
                        |> Arc3d.approximate (Length.meters 0.1)
                        |> Polyline3d.vertices

                Nothing ->
                    []

        newTrack : Maybe (TrackLoaded msg)
        newTrack =
            newEarthPoints
                |> List.concat
                |> List.map (DomainModel.gpxFromPointWithReference graph.referenceLonLat)
                |> DomainModel.treeFromSourcesWithExistingReference graph.referenceLonLat
                |> Maybe.map (TrackLoaded.newTrackFromTree graph.referenceLonLat)
    in
    case ( options.originalTrack, newTrack ) of
        ( Just oldTrack, Just track ) ->
            -- All has worked.
            let
                trackWithUndo =
                    track
                        |> TrackLoaded.addToUndoStack
                            Actions.MakeRouteFromGraph
                            0
                            0
                            (DomainModel.getAllGPXPointsInNaturalOrder oldTrack.trackTree)
            in
            { options
                | graph = trivialGraph trackWithUndo
                , selectedTraversal = 0
                , analyzed = False
                , originalTrack = Nothing
                , editingTrack = 0
                , undoGraph = Just graph
                , undoOriginalTrack = options.originalTrack
            }

        _ ->
            -- Not so much worked.
            options


undoWalkRoute : Options msg -> Options msg
undoWalkRoute options =
    case options.undoGraph of
        Just undoGraph ->
            { options
                | graph = undoGraph
                , selectedTraversal = 0
                , analyzed = True
                , originalTrack = options.undoOriginalTrack
                , editingTrack = 0
                , undoGraph = Nothing
            }

        Nothing ->
            options



-- END
