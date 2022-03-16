module Tools.Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions
import Angle
import BoundingBox3d
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, skipCount, trueLength)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import FeatherIcons
import FlatColors.ChinesePalette
import Length exposing (Length, Meters, inMeters)
import List.Extra
import Point2d
import Point3d
import Quantity exposing (Quantity, zero)
import Set exposing (Set)
import SketchPlane3d
import ToolTip exposing (myTooltip, tooltip)
import Tools.GraphOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, infoButton, neatToolsBorder, rgtDark, rgtPurple, useIcon, useIconWithSize)


defaultOptions : Options
defaultOptions =
    { graph = emptyGraph
    , centreLineOffset = Length.meters 0.0
    , boundingBox = BoundingBox3d.singleton Point3d.origin
    , selectedTraversal = 0
    , analyzed = False
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset (Quantity Float Meters)
    | ConvertFromGraph
    | HighlightTraversal Int
    | RemoveLastTraversal
    | AddTraversalFromCurrent
    | SelectStartNode
    | DisplayInfo String String


emptyGraph : Graph
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


toolID : String
toolID =
    "graph"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Route builder" )
        , ( "info", infoText )
        , ( "offset", "About the offset setting" )
        ]
    )


infoText =
    """We follow the route looking for places and road sections that are used more than once.
This allows us to divide the route
 into a list of sections, where each section follows one piece of road from one place to
other (or the same place). Once we've done that, you'll be able to change
the route you take between places. Use the Route view to help construct a new route."""


makeXY : EarthPoint -> XY
makeXY earth =
    -- Rounding to one metre bakes in some tolerance.
    let
        { x, y, z } =
            Point3d.toRecord inMeters earth
    in
    ( x, y )


view : (Msg -> msg) -> Options -> Element msg
view wrapper options =
    let
        offset =
            Length.inMeters options.centreLineOffset

        analyseButton =
            if options.analyzed then
                none

            else
                row [ spacing 3 ]
                    [ infoButton (wrapper <| DisplayInfo "graph" "info")
                    , I.button neatToolsBorder
                        { onPress = Just (wrapper GraphAnalyse)
                        , label = text "Find key places"
                        }
                    ]

        finishButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper ConvertFromGraph)
                , label = text "Convert back into route"
                }

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
                                "Offset = "
                                    ++ (showDecimal2 <| abs offset)
                                    ++ "m "
                                    ++ (if offset < 0.0 then
                                            "left"

                                        else if offset > 0.0 then
                                            "right"

                                        else
                                            ""
                                       )
                    , min = -5.0
                    , max = 5.0
                    , step = Just 0.25
                    , value = offset
                    , thumb = I.defaultThumb
                    }
                ]

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
                                { startPlace = "Place "
                                , road = "road "
                                , endPlace = "place"
                                , length = Quantity.zero
                                }

                            Just ( ( n1, n2, _ ), tree ) ->
                                case direction of
                                    Natural ->
                                        { startPlace = "Place " ++ String.fromInt n1
                                        , road = "road " ++ String.fromInt edge
                                        , endPlace = "place " ++ String.fromInt n2
                                        , length = trueLength tree
                                        }

                                    Reverse ->
                                        { startPlace = "Place " ++ String.fromInt n2
                                        , road = "road " ++ String.fromInt edge
                                        , endPlace = "place " ++ String.fromInt n1
                                        , length = trueLength tree
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
                    [ el ((width <| fillPortion 1) :: headerAttrs) <| text "  "
                    , el ((width <| fillPortion 2) :: headerAttrs) <| text "From"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| text "To"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| text "Along"
                    , el ((width <| fillPortion 2) :: headerAttrs) <| text "Distance"
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
                                        row [ spacing 2, alignRight ]
                                            [ if i + 1 == List.length traversals then
                                                I.button [ alignRight ]
                                                    { onPress = Just <| wrapper RemoveLastTraversal
                                                    , label = useIcon FeatherIcons.delete
                                                    }

                                              else
                                                none
                                            , I.button
                                                [ alignRight
                                                , tooltip below (myTooltip "Remove")
                                                ]
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
                                            text t.startPlace
                              }
                            , { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        el (dataStyles (i == options.selectedTraversal)) <|
                                            text t.endPlace
                              }
                            , { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        el (dataStyles (i == options.selectedTraversal)) <|
                                            text t.road
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
        column [ width fill, padding 4, spacing 10 ]
            [ row [ centerX, width fill, spacing 10 ]
                [ analyseButton
                , traversalPrevious
                , traversalNext
                ]
            , traversalsTable
            , offsetSlider
            , finishButton
            ]


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track wrapper =
    case msg of
        GraphAnalyse ->
            ( { options
                | graph = buildGraph track
                , analyzed = True
              }
            , []
            )

        HighlightTraversal traversal ->
            ( { options | selectedTraversal = traversal }, [] )

        SelectStartNode ->
            ( options, [] )

        AddTraversalFromCurrent ->
            ( options, [] )

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
                , selectedTraversal = min options.selectedTraversal (List.length newGraph.userRoute - 1)
              }
            , []
            )

        CentreLineOffset float ->
            ( { options | centreLineOffset = float }, [] )

        ConvertFromGraph ->
            ( options, [] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


type alias EdgeFinder =
    { startNodeIndex : Int
    , currentEdge : List ( EarthPoint, GPXSource )
    , edgeResolverDict : Dict ( Int, Int, XY ) ( Int, PeteTree )
    , edgesDict : Dict Int ( ( Int, Int, XY ), PeteTree )
    , traversals : List Traversal
    }


buildGraph : TrackLoaded msg -> Graph
buildGraph track =
    {-
       As in v1 & 2, the only way I know is to see which track points have more than two neighbours.
       Hence build a Dict using XY and the entries being a list of points that share the location.
       We might then have a user interaction step to refine the node list.
       First, let's get to the point where we can display nodes.
    -}
    let
        countNeighbours : RoadSection -> Dict XY (Set XY) -> Dict XY (Set XY)
        countNeighbours road countDict =
            -- Nicer that v2 thanks to use of road segments.
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

        finalEdgeFinder : EdgeFinder
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

        initialEdgeFinder : EdgeFinder
        initialEdgeFinder =
            { startNodeIndex = Dict.get (makeXY firstPoint) inverseNodes |> Maybe.withDefault 0
            , currentEdge = [ ( firstPoint, firstGpx ) ]
            , edgeResolverDict = Dict.empty
            , edgesDict = Dict.empty
            , traversals = []
            }

        splitIntoEdges :
            RoadSection
            -> EdgeFinder
            -> EdgeFinder
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

                        orientedEdge : List ( EarthPoint, GPXSource )
                        orientedEdge =
                            if nodeIndex > inputState.startNodeIndex then
                                -- Conventional order, good, but must flip the edge
                                List.reverse newEdge

                            else
                                newEdge

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
                                    ( newEdgeKey, newEdgeTree )
                                    inputState.edgesDict
                            , traversals = traversal :: inputState.traversals
                            }
    in
    { nodes = nodes
    , edges = finalEdgeFinder.edgesDict
    , userRoute = List.reverse finalEdgeFinder.traversals
    , referenceLonLat = track.referenceLonLat
    }


trivialGraph : TrackLoaded msg -> Graph
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
                [ ( 1, makeXY startNode ), ( 2, makeXY endNode ) ]

        edges =
            Dict.fromList
                [ ( 1, ( ( 1, 2, makeXY discriminator ), track.trackTree ) ) ]

        traversal =
            { edge = 1, direction = Natural }
    in
    { nodes = nodes
    , edges = edges
    , userRoute = [ traversal ]
    , referenceLonLat = track.referenceLonLat
    }



-- END
