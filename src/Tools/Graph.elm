module Tools.Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions
import BoundingBox3d
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as I
import FlatColors.ChinesePalette
import Length exposing (Length, Meters, inMeters)
import List.Extra
import Point2d
import Point3d
import Quantity exposing (Quantity, zero)
import Set exposing (Set)
import SketchPlane3d
import Tools.GraphOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, infoButton, neatToolsBorder, useIcon)


defaultOptions : Options
defaultOptions =
    { graph = Nothing
    , pointTolerance = Length.meters 4.0
    , minimumEdgeLength = Length.meters 100
    , centreLineOffset = Length.meters 0.0
    , boundingBox = BoundingBox3d.singleton Point3d.origin
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset (Quantity Float Meters)
    | ConvertFromGraph
    | HighlightTraversal Traversal
    | RemoveLastTraversal
    | AddTraversalFromCurrent
    | SelectStartNode
    | SetPointTolerance (Quantity Float Meters)
    | SetMinimumEdge (Quantity Float Meters)
    | DisplayInfo String String


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
        , ( "tolerance", "About the tolerance setting" )
        , ( "offset", "About the offset setting" )
        , ( "minEdge", "About the minimum edge setting" )
        ]
    )


infoText =
    """
Here we find repeated sections of a route. You can then pick and choose which
sections to ride, making your own route based on the original. This will ensure that
each time you use a section, the altitudes will match and render well in RGT.
"""


toleranceText =
    """
Blah blah about the meaning of it all.
"""


makeXY : EarthPoint -> XY
makeXY earth =
    -- Rounding to one metre bakes in some tolerance.
    let
        { x, y, z } =
            Point3d.toRecord inMeters earth
    in
    ( toFloat <| round x, toFloat <| round y )


view : (Msg -> msg) -> Options -> Element msg
view wrapper options =
    let
        offset =
            Length.inMeters options.centreLineOffset

        analyseButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper GraphAnalyse)
                , label = text "Find route sections"
                }

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

        pointToleranceSlider =
            row [ spacing 5 ]
                [ none
                , infoButton (wrapper <| DisplayInfo "graph" "tolerance")
                , I.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetPointTolerance << Length.meters
                    , label =
                        I.labelBelow [] <|
                            text <|
                                "Consider points equal if within "
                                    ++ showShortMeasure False options.pointTolerance
                    , min = 1.0
                    , max = 10.0
                    , step = Just 1.0
                    , value = Length.inMeters options.pointTolerance
                    , thumb = I.defaultThumb
                    }
                ]

        minEdgeSlider =
            row [ spacing 5 ]
                [ none
                , infoButton (wrapper <| DisplayInfo "graph" "minEdge")
                , I.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetMinimumEdge << Length.meters
                    , label =
                        I.labelBelow [] <|
                            text <|
                                "Ignore sections shorter than "
                                    ++ showShortMeasure False options.minimumEdgeLength
                    , min = 10.0
                    , max = 100.0
                    , step = Just 5.0
                    , value = Length.inMeters options.minimumEdgeLength
                    , thumb = I.defaultThumb
                    }
                ]

        removeButton =
            --TODO: Put a trashcan icon on the last line.
            I.button neatToolsBorder
                { onPress = Just (wrapper RemoveLastTraversal)
                , label = text "Remove traversal\nlast in list"
                }
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case options.graph of
            Nothing ->
                column [ width fill, padding 20, spacing 20 ]
                    [ paragraph [] [ text infoText ]
                    , infoButton (wrapper <| DisplayInfo "graph" "info")
                    , pointToleranceSlider
                    , minEdgeSlider
                    , analyseButton
                    ]

            Just g ->
                column [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                    [ row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                        [ offsetSlider
                        , finishButton
                        ]

                    --, showTheRoute g wrapper
                    , row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ] <|
                        if List.length g.userRoute > 0 then
                            [ removeButton
                            ]

                        else
                            [ none ]
                    ]


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track wrapper =
    case msg of
        SetPointTolerance quantity ->
            ( { options | pointTolerance = quantity }, [] )

        SetMinimumEdge quantity ->
            ( { options | minimumEdgeLength = quantity }, [] )

        GraphAnalyse ->
            ( { options | graph = Just <| buildGraph track }, [] )

        HighlightTraversal traversal ->
            ( options, [] )

        SelectStartNode ->
            ( options, [] )

        AddTraversalFromCurrent ->
            ( options, [] )

        RemoveLastTraversal ->
            ( options, [] )

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

        nodes =
            -- Two neighbours is just an edge point, anything else is a node.
            pointNeighbours
                |> Dict.filter (\pt neighbours -> Set.size neighbours /= 2)
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
                                | currentEdge = newEdge
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
    , userRoute = finalEdgeFinder.traversals
    , selectedTraversal = Nothing
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
    , selectedTraversal = Nothing
    , referenceLonLat = track.referenceLonLat
    }



-- END
