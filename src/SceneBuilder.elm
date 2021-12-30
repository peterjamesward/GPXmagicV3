module SceneBuilder exposing (..)

-- In V3 there is only one 3d model, used for first and third views.
-- Plan and Profile are 2d drawings.

import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, lightOrange)
import ColourPalette exposing (gradientHue2)
import DomainModel exposing (PeteTree(..), endsAt, lngLatPair, mapStartAt, pointFromIndex, startsAt, trueLength)
import Json.Encode as E
import Length exposing (Meters)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Pixels
import Plane3d
import Point3d
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material


render3dView :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
    }
    -> List (Entity LocalCoords)
render3dView model =
    let
        gradientColourPastel : Float -> Color.Color
        gradientColourPastel slope =
            Color.hsl (gradientHue2 slope) 0.6 0.7

        floorPlane =
            case model.trackTree of
                Just (Node node) ->
                    Plane3d.xy
                        |> Plane3d.offsetBy
                            (BoundingBox3d.minZ node.nodeContent.boundingBox)

                _ ->
                    Plane3d.xy

        boxSide =
            --TODO: put box side in model
            Length.kilometers 4

        gradientFromNode treeNode =
            Quantity.ratio
                (Point3d.zCoordinate (endsAt treeNode)
                    |> Quantity.minus
                        (Point3d.zCoordinate (startsAt treeNode))
                )
                (trueLength treeNode)
                |> (*) 100.0

        gradientCurtain : PeteTree -> List (Entity LocalCoords)
        gradientCurtain node =
            let
                gradient =
                    gradientFromNode node

                roadAsSegment =
                    LineSegment3d.from (startsAt node) (endsAt node)

                curtainHem =
                    LineSegment3d.projectOnto floorPlane roadAsSegment
            in
            [ Scene3d.quad (Material.color <| gradientColourPastel gradient)
                (LineSegment3d.startPoint roadAsSegment)
                (LineSegment3d.endPoint roadAsSegment)
                (LineSegment3d.endPoint curtainHem)
                (LineSegment3d.startPoint curtainHem)
            ]

        makeVisibleSegment node =
            [ Scene3d.point { radius = Pixels.pixels 1 } (Material.color black) (startsAt node)
            , Scene3d.lineSegment (Material.color black) <| LineSegment3d.from (startsAt node) (endsAt node)
            ]
                ++ gradientCurtain node

        renderTree : Int -> PeteTree -> List (Entity LocalCoords) -> List (Entity LocalCoords)
        renderTree depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode ++ accum

                Node notLeaf ->
                    if depth <= 0 then
                        makeVisibleSegment someNode ++ accum

                    else
                        accum
                            |> renderTree (depth - 1) notLeaf.left
                            |> renderTree (depth - 1) notLeaf.right

        renderTreeSelectively :
            BoundingBox3d Meters LocalCoords
            -> Int
            -> PeteTree
            -> List (Entity LocalCoords)
            -> List (Entity LocalCoords)
        renderTreeSelectively box depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    if leafNode.boundingBox |> BoundingBox3d.intersects box then
                        makeVisibleSegment someNode ++ accum

                    else
                        accum

                Node notLeaf ->
                    if notLeaf.nodeContent.boundingBox |> BoundingBox3d.intersects box then
                        -- Ignore depth cutoff near or in the box
                        accum
                            |> renderTreeSelectively box (depth - 1) notLeaf.left
                            |> renderTreeSelectively box (depth - 1) notLeaf.right

                    else
                        -- Outside box, apply cutoff.
                        accum
                            |> renderTree (depth - 1) notLeaf.left
                            |> renderTree (depth - 1) notLeaf.right

        renderCurrentMarker : Int -> PeteTree -> List (Entity LocalCoords)
        renderCurrentMarker marker tree =
            [ Scene3d.point { radius = Pixels.pixels 10 }
                (Material.color lightOrange)
                (pointFromIndex marker tree)
            ]
    in
    case model.trackTree of
        Just tree ->
            let
                box =
                    BoundingBox3d.withDimensions ( boxSide, boxSide, boxSide )
                        (pointFromIndex model.currentPosition tree)
            in
            renderTreeSelectively box model.renderDepth tree []
                ++ renderCurrentMarker model.currentPosition tree

        Nothing ->
            []



--TODO: Factor the recursion away from the rendering and these two large routines combine.


renderMapJson :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
    }
    -> E.Value
renderMapJson model =
    let
        boxSide =
            --TODO: put box side in model
            Length.kilometers 4

        makeVisibleSegment : PeteTree -> E.Value
        makeVisibleSegment node =
            lngLatPair <| mapStartAt node

        renderCurrentMarker : Int -> PeteTree -> List (Entity LocalCoords)
        renderCurrentMarker marker tree =
            [ Scene3d.point { radius = Pixels.pixels 10 }
                (Material.color lightOrange)
                (pointFromIndex marker tree)
            ]

        renderTree : Int -> PeteTree -> List E.Value -> List E.Value
        renderTree depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode :: accum

                Node notLeaf ->
                    if depth <= 0 then
                        makeVisibleSegment someNode :: accum

                    else
                        accum
                            |> renderTree (depth - 1) notLeaf.left
                            |> renderTree (depth - 1) notLeaf.right

        renderTreeSelectively :
            BoundingBox3d Meters LocalCoords
            -> Int
            -> PeteTree
            -> List E.Value
            -> List E.Value
        renderTreeSelectively box depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    if leafNode.boundingBox |> BoundingBox3d.intersects box then
                        makeVisibleSegment someNode :: accum

                    else
                        accum

                Node notLeaf ->
                    if notLeaf.nodeContent.boundingBox |> BoundingBox3d.intersects box then
                        -- Ignore depth cutoff near or in the box
                        accum
                            |> renderTreeSelectively box (depth - 1) notLeaf.left
                            |> renderTreeSelectively box (depth - 1) notLeaf.right

                    else
                        -- Outside box, apply cutoff.
                        accum
                            |> renderTree (depth - 1) notLeaf.left
                            |> renderTree (depth - 1) notLeaf.right
    in
    case model.trackTree of
        Just tree ->
            let
                box =
                    BoundingBox3d.withDimensions ( boxSide, boxSide, boxSide )
                        (pointFromIndex model.currentPosition tree)

                geometry =
                    E.object
                        [ ( "type", E.string "LineString" )
                        , ( "coordinates", E.list identity coordinates )
                        ]

                coordinates =
                    renderTreeSelectively box model.renderDepth tree []

            in
            E.object
                [ ( "type", E.string "Feature" )
                , ( "properties", E.object [] )
                , ( "geometry", geometry )
                ]


        Nothing ->
            E.null
