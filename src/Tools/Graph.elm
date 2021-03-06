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
    , matchingTolerance = Length.meters 1.5
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
    , suggestedNewTree = Nothing
    , suggestedNewGraph = Nothing
    , graphUndos = []
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
    | UndoDeleteRoad


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
        ( Just lastTraversal, Just clickedEdge ) ->
            case Dict.get lastTraversal.edge options.graph.edges of
                Just currentLastEdge ->
                    let
                        finalNode =
                            if lastTraversal.direction == Natural then
                                currentLastEdge.highNode

                            else
                                currentLastEdge.lowNode
                    in
                    finalNode == clickedEdge.lowNode || finalNode == clickedEdge.highNode

                Nothing ->
                    False

        ( Nothing, Just _ ) ->
            -- Any edge can be the first edge used.
            True

        _ ->
            False


edgeCanBeDeleted : Int -> Options msg -> Bool
edgeCanBeDeleted edge options =
    -- Edge can be deleted if it's not the only edge and it's not used in the route.
    Dict.size options.graph.edges
        > 1
        && (not <| List.any (\traversal -> traversal.edge == edge) options.graph.userRoute)


loopCanBeAdded : Int -> Options msg -> Bool
loopCanBeAdded node options =
    -- Loop can be added if node is same as final node of last traversal.
    case
        List.Extra.last options.graph.userRoute
    of
        Just traversal ->
            case Dict.get traversal.edge options.graph.edges of
                Just finalEdge ->
                    let
                        finalNode =
                            if traversal.direction == Natural then
                                finalEdge.highNode

                            else
                                finalEdge.lowNode
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
        ( [ traversal ], Just edgeInfo ) ->
            -- Can flip because only edge (if it's the same edge
            traversal.edge == newEdge

        ( t1 :: t2 :: tN, Just edgeInfo ) ->
            -- Can be flipped if self-loop
            edgeInfo.lowNode == edgeInfo.highNode

        _ ->
            False


deleteEdge : Int -> Options msg -> Options msg
deleteEdge edge options =
    -- Check is not used in route.
    -- Remove edge from dictionary.
    -- If either end node has no other edges, remove them as well.
    let
        graph =
            options.graph
    in
    case Dict.get edge graph.edges of
        Just edgeInfo ->
            let
                newGraph =
                    { graph | edges = Dict.remove edge graph.edges }
                        |> pruneOrphanedNodes
                        |> removeIfRedundantPlace edgeInfo.lowNode
                        |> removeIfRedundantPlace edgeInfo.highNode
            in
            { options
                | graph = newGraph
                , graphUndos = graph :: options.graphUndos
            }

        Nothing ->
            options


pruneOrphanedNodes : Graph msg -> Graph msg
pruneOrphanedNodes graph =
    -- Deletion of an edge may leave unconnected nodes, remove them.
    let
        nodeHasEdge k v =
            not <| List.isEmpty <| combinedEdgesForNode k graph
    in
    { graph | nodes = Dict.filter nodeHasEdge graph.nodes }


joinTracks : TrackLoaded msg -> TrackLoaded msg -> TrackLoaded msg
joinTracks track1 track2 =
    let
        ( asGpx1, asGpx2 ) =
            ( DomainModel.getAllGPXPointsInNaturalOrder track1.trackTree
            , DomainModel.getAllGPXPointsInNaturalOrder track2.trackTree
            )
    in
    case
        DomainModel.treeFromSourcesWithExistingReference
            track1.referenceLonLat
            (asGpx1 ++ asGpx2)
    of
        Just tree ->
            { track1 | trackTree = tree }

        Nothing ->
            track1


reverseTrack : TrackLoaded msg -> TrackLoaded msg
reverseTrack track =
    let
        asGpx1 =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    case
        DomainModel.treeFromSourcesWithExistingReference
            track.referenceLonLat
            (List.reverse asGpx1)
    of
        Just tree ->
            { track | trackTree = tree }

        Nothing ->
            track


removeIfRedundantPlace : Int -> Graph msg -> Graph msg
removeIfRedundantPlace node graph =
    -- Deletion of edge may reduce end Places to having only two Roads, remove them.
    -- Details depend on whether this node is the low or high numbered edge end.
    -- `listEdgesForNode` now returns full edge details.
    --TOOD: Defuglification.
    case listEdgesForNode node graph of
        ( [ asLow1, asLow2 ], [] ) ->
            -- Combined edge is from lowest to highest of the "high ends", flipping the first.
            let
                ( edge1Index, edge1Info ) =
                    asLow1

                ( edge2Index, edge2Info ) =
                    asLow2
            in
            if edge1Info.highNode <= edge2Info.highNode then
                let
                    newEdge1 =
                        { lowNode = edge1Info.highNode
                        , highNode = edge2Info.highNode
                        , via = edge1Info.via
                        , track = joinTracks (reverseTrack edge1Info.track) edge2Info.track
                        , originalDirection = edge2Info.originalDirection
                        }
                in
                { graph
                    | edges =
                        graph.edges
                            |> Dict.remove edge1Index
                            |> Dict.remove edge2Index
                            |> Dict.insert edge1Index newEdge1
                    , nodes = Dict.remove node graph.nodes
                }

            else
                --if high2 < high1 then
                let
                    newEdge2 =
                        { lowNode = edge2Info.highNode
                        , highNode = edge1Info.highNode
                        , via = edge2Info.via
                        , track = joinTracks (reverseTrack edge2Info.track) edge1Info.track
                        , originalDirection = edge1Info.originalDirection
                        }
                in
                { graph
                    | edges =
                        graph.edges
                            |> Dict.remove edge1Index
                            |> Dict.remove edge2Index
                            |> Dict.insert edge1Index newEdge2
                    , nodes = Dict.remove node graph.nodes
                }

        ( [], [ asHigh1, asHigh2 ] ) ->
            let
                ( edge1Index, edge1Info ) =
                    asHigh1

                ( edge2Index, edge2Info ) =
                    asHigh2
            in
            if edge1Info.lowNode <= edge2Info.lowNode then
                let
                    newEdge1 =
                        { lowNode = edge1Info.lowNode
                        , highNode = edge2Info.lowNode
                        , via = edge1Info.via
                        , track = joinTracks edge1Info.track (reverseTrack edge2Info.track)
                        , originalDirection = edge1Info.originalDirection
                        }
                in
                { graph
                    | edges =
                        graph.edges
                            |> Dict.remove edge1Index
                            |> Dict.remove edge2Index
                            |> Dict.insert edge1Index newEdge1
                    , nodes = Dict.remove node graph.nodes
                }

            else
                --if low2 < low1 then
                let
                    newEdge2 =
                        { lowNode = edge2Info.lowNode
                        , highNode = edge1Info.lowNode
                        , via = edge2Info.via
                        , track = joinTracks edge2Info.track (reverseTrack edge1Info.track)
                        , originalDirection = edge1Info.originalDirection
                        }
                in
                { graph
                    | edges =
                        graph.edges
                            |> Dict.remove edge1Index
                            |> Dict.remove edge2Index
                            |> Dict.insert edge1Index newEdge2
                    , nodes = Dict.remove node graph.nodes
                }

        ( [ asLow ], [ asHigh ] ) ->
            let
                ( edge1Index, edge1Info ) =
                    asLow

                ( edge2Index, edge2Info ) =
                    asHigh

                newEdge =
                    { lowNode = edge1Info.lowNode
                    , highNode = edge2Info.lowNode
                    , via = edge1Info.via
                    , track = joinTracks edge2Info.track edge1Info.track
                    , originalDirection = edge1Info.originalDirection
                    }
            in
            { graph
                | edges =
                    graph.edges
                        |> Dict.remove edge1Index
                        |> Dict.remove edge2Index
                        |> Dict.insert edge1Index newEdge
                , nodes =
                    if node == edge1Info.highNode then
                        -- Don't remove if self-loop
                        graph.nodes

                    else
                        Dict.remove node graph.nodes
            }

        _ ->
            -- Not exactly two edges, do nothing.
            graph


listEdgesForNode : Int -> Graph msg -> ( List ( Int, Edge msg ), List ( Int, Edge msg ) )
listEdgesForNode node graph =
    let
        withLowNode =
            graph.edges
                |> Dict.filter
                    (\_ edgeInfo -> edgeInfo.lowNode == node)
                |> Dict.toList

        withHighNode =
            graph.edges
                |> Dict.filter
                    (\_ edgeInfo -> edgeInfo.highNode == node)
                |> Dict.toList
    in
    ( withLowNode, withHighNode )


combinedEdgesForNode : Int -> Graph msg -> List ( Int, Edge msg )
combinedEdgesForNode node graph =
    let
        ( asLow, asHigh ) =
            listEdgesForNode node graph
    in
    asLow ++ asHigh


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
        ( Just traversal, Just addedEdgeInfo ) ->
            case Dict.get traversal.edge graph.edges of
                Just lastEdgeInfo ->
                    let
                        finalNode =
                            if traversal.direction == Natural then
                                lastEdgeInfo.highNode

                            else
                                lastEdgeInfo.lowNode

                        newEdgeDirection =
                            -- Special case if added section is a loop.
                            if addedEdgeInfo.lowNode == addedEdgeInfo.highNode then
                                Natural

                            else if finalNode == addedEdgeInfo.lowNode then
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

        ( Nothing, Just addedEdgeInfo ) ->
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
        Just traversal ->
            case Dict.get traversal.edge graph.edges of
                Just edgeInfo ->
                    let
                        ( finalNode, edgeDirection, endPoint ) =
                            if traversal.direction == Natural then
                                ( edgeInfo.highNode
                                , DomainModel.getLastLeaf edgeInfo.track.trackTree |> .directionAtEnd
                                , DomainModel.earthPointFromIndex
                                    (skipCount edgeInfo.track.trackTree)
                                    edgeInfo.track.trackTree
                                )

                            else
                                ( edgeInfo.lowNode
                                , DomainModel.getFirstLeaf edgeInfo.track.trackTree
                                    |> .directionAtStart
                                    |> Direction2d.reverse
                                , DomainModel.earthPointFromIndex 0 edgeInfo.track.trackTree
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

                                newEdgeInfo =
                                    { lowNode = node
                                    , highNode = node
                                    , via = makeXY loopOpposite
                                    }

                                newEdgeTree =
                                    DomainModel.treeFromSourcesWithExistingReference
                                        edgeInfo.track.referenceLonLat
                                        edgePoints

                                newEdgeTrack =
                                    Maybe.map (TrackLoaded.newTrackFromTree edgeInfo.track.referenceLonLat)
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
                                                        { lowNode = newEdgeInfo.lowNode
                                                        , highNode = newEdgeInfo.highNode
                                                        , via = newEdgeInfo.via
                                                        , track = newTrack
                                                        , originalDirection = Natural
                                                        }
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
        |> Maybe.map .track


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
                                Just edgeInfo ->
                                    Dict.insert
                                        options.editingTrack
                                        { edgeInfo | track = theTrack }
                                        graph.edges

                                Nothing ->
                                    graph.edges
                    }

                newOptions =
                    { options | graph = newGraph }
            in
            if not options.analyzed then
                lookForClusters newOptions newOptions.matchingTolerance theTrack

            else
                ( newOptions, [ Actions.HidePreview "graph" ] )

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
            row [ spacing 3, width fill ]
                [ infoButton (wrapper <| DisplayInfo "graph" "info")
                , I.button neatToolsBorder
                    { onPress = Just (wrapper GraphAnalyse)
                    , label = i18n "find"
                    }
                ]

        adoptTrackButton =
            row [ spacing 3, width fill ]
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
            if not <| List.isEmpty options.graph.userRoute then
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

        undoButton =
            if not <| List.isEmpty options.graphUndos then
                row [ spacing 3 ]
                    [ I.button
                        neatToolsBorder
                        { onPress = Just (wrapper UndoDeleteRoad)
                        , label = i18n "undo"
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
                    (\traversal ->
                        case Dict.get traversal.edge graph.edges of
                            Nothing ->
                                { startPlace = -1
                                , road = -1
                                , endPlace = -1
                                , length = Quantity.zero
                                }

                            Just edgeInfo ->
                                case traversal.direction of
                                    Natural ->
                                        { startPlace = edgeInfo.lowNode
                                        , road = traversal.edge
                                        , endPlace = edgeInfo.highNode
                                        , length = trueLength edgeInfo.track.trackTree
                                        }

                                    Reverse ->
                                        { startPlace = edgeInfo.highNode
                                        , road = traversal.edge
                                        , endPlace = edgeInfo.lowNode
                                        , length = trueLength edgeInfo.track.trackTree
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

        guidanceText =
            row
                [ Background.color FlatColors.AmericanPalette.lightGreenishBlue
                , Border.rounded 5
                ]
                [ useIconWithSize 20 FeatherIcons.info
                , paragraph [ padding 4 ]
                    [ if options.analyzed then
                        if List.isEmpty options.graph.userRoute then
                            i18n "guidanceNoRoute"

                        else
                            i18n "guidanceAnalyzed"

                      else
                        i18n "guidanceNotAnalyzed"
                    ]
                ]
    in
    column
        [ width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , padding 4
        ]
        [ guidanceText
        , if options.analyzed then
            column [ width fill, padding 4, spacing 10 ]
                [ row [ centerX, width fill, spacing 10 ]
                    [ traversalPrevious
                    , traversalNext
                    , clearRouteButton
                    , revertButton
                    ]
                , traversalsTable
                , undoButton
                , wrappedRow [ spacing 5 ] [ offsetSlider, minRadiusSlider ]
                , finishButton
                ]

          else
            wrappedRow [ centerX, width fill, spacing 10 ]
                [ toleranceSlider
                , adoptTrackButton
                , analyseButton
                ]
        ]


type alias PointIndexEntry =
    { pointIndex : Int }


type alias PointIndex =
    SpatialIndex.SpatialNode PointIndexEntry Length.Meters LocalCoords


identifyPointsToBeMerged : Length.Length -> TrackLoaded msg -> ( List Cluster, PeteTree )
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
                    SpatialIndex.query track.leafIndex (pointWithTolerance pt)
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
                            , thisPoint2d |> Point2d.signedDistanceFrom axis2d |> Quantity.abs
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

        --_ =
        --    Debug.log "LEAVES" findAllNearbyLeaves
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
        -}
        insertPointsInLeaf : Int -> List InsertedPointOnLeaf -> PeteTree -> PeteTree
        insertPointsInLeaf leafNumber newPoints tree =
            let
                asGPX =
                    List.map
                        (.earthPoint >> DomainModel.gpxFromPointWithReference track.referenceLonLat)
                        newPoints
            in
            DomainModel.insertPointsIntoLeaf
                leafNumber
                track.referenceLonLat
                asGPX
                tree

        treeWithAddedPoints : PeteTree
        treeWithAddedPoints =
            --NOTE: Must add to highest numbered leaf first or leaf numbers confused!
            --NOTE: Dict.foldr not doing what I expect.
            Dict.foldr
                insertPointsInLeaf
                track.trackTree
                perpendicularFeetGroupedByLeaf

        {-
           Now have tree', enhanced by "virtual points" where leaves are close to points.
           5. For each point, find nearby points within tolerance.
        -}
        ( _, pointIndex ) =
            --NOTE: We index the revised tree so we pick up the extra points.
            -- Pre-pop with first point so the fold can focus on the leaf end points.
            DomainModel.foldOverRoute
                indexPoint
                treeWithAddedPoints
                ( 1
                , SpatialIndex.add
                    { content = { pointIndex = 0 }
                    , box =
                        pointWithTolerance <|
                            DomainModel.earthPointFromIndex 0 treeWithAddedPoints
                    }
                    emptyPointIndex
                )

        pointsNearPoint : Int -> PeteTree -> List Int
        pointsNearPoint pointNumber tree =
            -- Prelude to finding clusters of points.
            -- Use spatial point index then refine with geometry.
            --NOTE: This will NOW NOT include the query point.
            --NOTE: This MUST NOT include points that are merely close along the track; the
            -- intent is to find points separated along the track but close in space.
            --Hence we can filter using this.
            let
                pt =
                    DomainModel.earthPointFromIndex pointNumber tree

                thisPoint2d =
                    Point3d.projectInto SketchPlane3d.xy pt

                results =
                    SpatialIndex.query pointIndex (pointWithTolerance pt)
                        |> List.map (.content >> .pointIndex)

                thisPointTrackDistance =
                    DomainModel.distanceFromIndex pointNumber tree
            in
            results
                |> List.filter
                    (\ptidx ->
                        DomainModel.earthPointFromIndex ptidx tree
                            |> Point3d.projectInto SketchPlane3d.xy
                            |> Point2d.equalWithin tolerance thisPoint2d
                    )

        {-
           I thought it would be better to exclude points that are close by dint of being along the route.
           Turns out this is empirically less pleasing, adding more Nodes to the Graph.
                          |> List.filter
                              (\ptidx ->
                                  DomainModel.distanceFromIndex ptidx tree
                                      |> Quantity.minus thisPointTrackDistance
                                      |> Quantity.abs
                                      |> Quantity.greaterThan tolerance
                              )
        -}
        pointsNearPointFoldWrapper :
            RoadSection
            -> ( Int, List ( Int, List Int ) )
            -> ( Int, List ( Int, List Int ) )
        pointsNearPointFoldWrapper road ( leafNumber, collection ) =
            case pointsNearPoint (leafNumber + 1) treeWithAddedPoints of
                [] ->
                    ( leafNumber + 1, collection )

                notEmpty ->
                    ( leafNumber + 1, ( leafNumber + 1, notEmpty ) :: collection )

        --nearbyPointsForEachPoint : List ( Int, List Int )
        ( _, nearbyPointsForEachPoint ) =
            -- Injecting the point zero case is slightly clumsy.
            DomainModel.foldOverRoute
                pointsNearPointFoldWrapper
                treeWithAddedPoints
                ( 0
                , case
                    pointsNearPoint 0 treeWithAddedPoints
                  of
                    [] ->
                        []

                    notEmpty ->
                        [ ( 0, notEmpty ) ]
                )

        {-
           6. Sort "points with vicini" in descending order of vicini count.
           7. Work through this sorted list, forming groups (a set of previously unclaimed vicini).
        -}
        groupsOfNearbyPoints : List (List Int)
        groupsOfNearbyPoints =
            let
                groupsInDescendingSizeOrder =
                    -- Since the queries return DON'T the home point, we don't need the first Int.
                    nearbyPointsForEachPoint
                        |> List.map (\( home, others ) -> home :: others)
                        |> List.sortBy (List.length >> negate)

                retainUnclaimedGroupMembers :
                    List Int
                    -> ( Set Int, List (List Int) )
                    -> ( Set Int, List (List Int) )
                retainUnclaimedGroupMembers group ( claimed, retained ) =
                    let
                        remaining =
                            group |> List.filter (\i -> not <| Set.member i claimed)
                    in
                    case remaining of
                        -- Nothing left here, drop it.
                        pt1 :: pt2 :: _ ->
                            -- Only interested if not empty and not singleton
                            ( Set.fromList remaining |> Set.union claimed
                            , remaining :: retained
                            )

                        _ ->
                            ( claimed, retained )

                ( _, groupsWithPriorClaimsRemoved ) =
                    List.foldl
                        retainUnclaimedGroupMembers
                        ( Set.empty, [] )
                        groupsInDescendingSizeOrder
            in
            groupsWithPriorClaimsRemoved

        {-
           8. For each cluster, derive the centroid.
        -}
        clustersWithCentroids : List Cluster
        clustersWithCentroids =
            let
                makeProperCluster : List Int -> Cluster
                makeProperCluster pointNumbers =
                    { centroid =
                        case
                            pointNumbers
                                |> List.map (\pt -> DomainModel.earthPointFromIndex pt treeWithAddedPoints)
                        of
                            [] ->
                                --We already know this is a non-empty list.
                                Point3d.origin

                            pt1 :: more ->
                                Point3d.centroid pt1 more
                    , pointsToAdjust = pointNumbers
                    }
            in
            List.map makeProperCluster groupsOfNearbyPoints

        {-
           9. For each point in all clusters, derive mapping to centroid.
           (Step 9 is merely a restatement of Cluster, so will skip.)
           10. Apply mappings by updating points (`updatePointByIndexInSitu` perhaps?).
        -}
        treeWithCentroidsApplied : PeteTree
        treeWithCentroidsApplied =
            let
                mapCluster : Cluster -> PeteTree -> PeteTree
                mapCluster cluster outputTree =
                    --Each move modifies tree so must be a fold.
                    let
                        asGPS =
                            DomainModel.gpxFromPointWithReference track.referenceLonLat cluster.centroid
                    in
                    List.foldl
                        (movePoint asGPS)
                        outputTree
                        cluster.pointsToAdjust

                movePoint : GPXSource -> Int -> PeteTree -> PeteTree
                movePoint centroid pointNumber tree =
                    DomainModel.updatePointByIndexInSitu
                        pointNumber
                        centroid
                        track.referenceLonLat
                        tree
            in
            List.foldl
                mapCluster
                treeWithAddedPoints
                clustersWithCentroids

        {-
           Now have tree'' which has adjusted points at cluster centroids.
           Proof of pudding awaited ...
        -}
    in
    ( clustersWithCentroids
    , treeWithCentroidsApplied
    )


makePreview options track =
    Actions.ShowPreview
        { tag = "graph"
        , shape = PreviewToolSupplied <| showNewPoints options.clustersForPreview track
        , colour = FlatColors.AmericanPalette.sourLemon
        , points = []
        }


update :
    Msg
    -> Options msg
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options msg, List (Actions.ToolAction msg) )
update msg options track wrapper =
    case msg of
        AdoptNewTrack ->
            ( options
            , [ Actions.CombineNearbyPoints
              , Actions.TrackHasChanged
              ]
            )

        GraphAnalyse ->
            ( options
            , [ if options.matchingTolerance |> Quantity.greaterThanZero then
                    Actions.CombineNearbyPoints

                else
                    Actions.NoAction
              , Actions.StartRoutePlanning
              , Actions.HidePreview "graph"
              ]
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
            lookForClusters options tolerance track

        CentreLineOffset float ->
            ( { options | centreLineOffset = float }, [] )

        MinimumRadius float ->
            ( { options | minimumRadiusAtPlaces = float }, [] )

        ConvertFromGraph ->
            ( options
            , [ Actions.MakeRouteFromGraph
              , Actions.TrackHasChanged
              , Actions.ExitRoutePlanning
              , Actions.HidePreview "graph"
              ]
            )

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
                , selectedTraversal = -1
              }
            , []
            )

        UndoDeleteRoad ->
            case options.graphUndos of
                lastVersion :: olderVersions ->
                    ( { options
                        | graph = lastVersion
                        , graphUndos = olderVersions
                      }
                    , []
                    )

                _ ->
                    ( options, [] )


lookForClusters :
    Options msg
    -> Quantity Float Meters
    -> TrackLoaded msg
    -> ( Options msg, List (ToolAction msg) )
lookForClusters options tolerance track =
    let
        ( clusters, suggestedNewTree ) =
            identifyPointsToBeMerged tolerance track

        suggestedNewTrack =
            { track
                | trackTree =
                    --Absurd but experimenting.
                    suggestedNewTree
                        |> DomainModel.getAllGPXPointsInNaturalOrder
                        |> TrackLoaded.removeAdjacentDuplicates
                        |> DomainModel.treeFromSourcesWithExistingReference track.referenceLonLat
                        |> Maybe.withDefault suggestedNewTree
            }

        newOptions =
            { options
                | matchingTolerance = tolerance
                , clustersForPreview = clusters
                , suggestedNewTree = Just suggestedNewTree
                , suggestedNewGraph = Just <| buildGraph suggestedNewTrack
            }
    in
    ( newOptions
    , [ makePreview newOptions track ]
    )


type alias EdgeFinder msg =
    { startNodeIndex : Int
    , currentEdge : List ( EarthPoint, GPXSource )
    , edgeResolverDict : Dict ( Int, Int, XY ) ( Int, PeteTree )
    , edgesDict : Dict Int (Edge msg)
    , traversals : List Traversal
    }


showNewPoints : List Cluster -> TrackLoaded msg -> List (Entity LocalCoords)
showNewPoints pointInfo track =
    let
        locations =
            pointInfo |> List.map .centroid

        material =
            Material.color Color.white

        highlightPoint point =
            Scene3d.point { radius = Pixels.pixels 3 } material point

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
                            if nodeIndex >= inputState.startNodeIndex then
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
                            orientedEdge
                                |> List.Extra.getAt 1
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
                                    ( lowNode, highNode, discriminator )
                                    ( newEdgeIndex, newEdgeTree )
                                    inputState.edgeResolverDict
                            , edgesDict =
                                Dict.insert
                                    newEdgeIndex
                                    { lowNode = lowNode
                                    , highNode = highNode
                                    , via = discriminator
                                    , track = newEdgeTrack
                                    , originalDirection = traversal.direction
                                    }
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
                [ ( 0
                  , { lowNode = 1
                    , highNode = 2
                    , via = makeXY discriminator
                    , track = track
                    , originalDirection = Natural
                    }
                  )
                ]

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


enterRoutePlanningMode : Options msg -> TrackLoaded msg -> ( Options msg, PeteTree )
enterRoutePlanningMode options track =
    ( { options
        | graph = buildGraph track
        , analyzed = True
        , originalTrack = Just track
        , suggestedNewTree = Nothing
        , suggestedNewGraph = Nothing
        , graphUndos = []
      }
    , options.suggestedNewTree |> Maybe.withDefault track.trackTree
    )


combineNearbyPoints : Options msg -> TrackLoaded msg -> ( Options msg, PeteTree )
combineNearbyPoints options track =
    ( { options | suggestedNewTree = Nothing }
    , options.suggestedNewTree
        |> Maybe.withDefault track.trackTree
        |> DomainModel.getAllGPXPointsInNaturalOrder
        |> TrackLoaded.removeAdjacentDuplicates
        |> DomainModel.treeFromSourcesWithExistingReference track.referenceLonLat
        |> Maybe.withDefault track.trackTree
    )


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
                Just edgeInfo ->
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
                            ( DomainModel.indexFromDistance trim edgeInfo.track.trackTree + 1
                            , DomainModel.indexFromDistance
                                (trueLength edgeInfo.track.trackTree |> Quantity.minus trim)
                                edgeInfo.track.trackTree
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
                                |> List.map (useNudgeTool nudgeOptions edgeInfo.track.trackTree)
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
                ( Just inEdge, Just outEdge ) ->
                    let
                        actualVertex =
                            case inbound.direction of
                                Natural ->
                                    DomainModel.earthPointFromIndex
                                        (skipCount inEdge.track.trackTree)
                                        inEdge.track.trackTree

                                Reverse ->
                                    DomainModel.earthPointFromIndex
                                        0
                                        inEdge.track.trackTree

                        ( inboundTrimIndex, inboundTrimPoint ) =
                            case inbound.direction of
                                Natural ->
                                    DomainModel.interpolateTrack
                                        (trueLength inEdge.track.trackTree |> Quantity.minus trim)
                                        inEdge.track.trackTree

                                Reverse ->
                                    DomainModel.interpolateTrack
                                        trim
                                        inEdge.track.trackTree

                        ( outboundTrimIndex, outboundTrimPoint ) =
                            case outbound.direction of
                                Natural ->
                                    DomainModel.interpolateTrack
                                        trim
                                        outEdge.track.trackTree

                                Reverse ->
                                    DomainModel.interpolateTrack
                                        (trueLength outEdge.track.trackTree |> Quantity.minus trim)
                                        outEdge.track.trackTree

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
