module SceneBuilder exposing (..)

-- In V3 there is only one 3d model, used for first and third views.
-- Plan and Profile are 2d drawings.

import Angle exposing (Angle)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientHue, gradientHue2)
import Direction2d
import Direction3d
import DomainModel exposing (..)
import Json.Encode as E
import Length exposing (Meters)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Pixels
import Plane3d exposing (Plane3d)
import Point3d
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d
import Spherical
import TrackLoaded exposing (TrackLoaded)
import Vector3d


globe =
    Scene3d.sphere (Material.color darkGreen)
        (Sphere3d.withRadius (Length.meters Spherical.meanRadius) Point3d.origin)


render3dView :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
    }
    -> List (Entity LocalCoords)
render3dView model =
    let
        floorPlane =
            case model.trackTree of
                Just aTree ->
                    Plane3d.xy |> Plane3d.offsetBy (BoundingBox3d.minZ <| boundingBox aTree)

                Nothing ->
                    Plane3d.xy

        fullRenderingZone =
            case model.trackTree of
                Just aTree ->
                    BoundingBox3d.withDimensions ( boxSide, boxSide, boxSide )
                        (startPoint <| leafFromIndex model.currentPosition aTree)

                Nothing ->
                    BoundingBox3d.singleton Point3d.origin

        gradientColourPastel : Float -> Color.Color
        gradientColourPastel slope =
            Color.hsl (gradientHue slope) 0.6 0.7

        boxSide =
            --TODO: put box side in model
            Length.kilometers 4

        gradientFromNode treeNode =
            Quantity.ratio
                (Point3d.zCoordinate (endPoint treeNode)
                    |> Quantity.minus
                        (Point3d.zCoordinate (startPoint treeNode))
                )
                (trueLength treeNode)
                |> (*) 100.0

        gradientCurtain : PeteTree -> List (Entity LocalCoords)
        gradientCurtain node =
            let
                gradient =
                    gradientFromNode node

                roadAsSegment =
                    LineSegment3d.from (startPoint node) (endPoint node)

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
            [ Scene3d.point { radius = Pixels.pixels 1 }
                (Material.color black)
                (startPoint node)
            , Scene3d.lineSegment (Material.color black) <|
                LineSegment3d.from (startPoint node) (endPoint node)
            ]
                ++ gradientCurtain node

        renderTree :
            Int
            -> PeteTree
            -> List (Entity LocalCoords)
            -> List (Entity LocalCoords)
        renderTree depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode ++ accum

                Node unLeaf ->
                    if depth <= 0 then
                        makeVisibleSegment someNode ++ accum

                    else
                        accum
                            |> renderTree (depth - 1) unLeaf.left
                            |> renderTree (depth - 1) unLeaf.right

        renderTreeSelectively :
            Int
            -> PeteTree
            -> List (Entity LocalCoords)
            -> List (Entity LocalCoords)
        renderTreeSelectively depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode ++ accum

                Node unLeaf ->
                    if unLeaf.nodeContent.boundingBox |> BoundingBox3d.intersects fullRenderingZone then
                        -- Ignore depth cutoff near or in the box
                        accum
                            |> renderTreeSelectively (depth - 1) unLeaf.left
                            |> renderTreeSelectively (depth - 1) unLeaf.right

                    else
                        -- Outside box, apply cutoff.
                        accum
                            |> renderTree (depth - 1) unLeaf.left
                            |> renderTree (depth - 1) unLeaf.right

        renderCurrentMarker : Int -> PeteTree -> List (Entity LocalCoords)
        renderCurrentMarker marker tree =
            [ Scene3d.point { radius = Pixels.pixels 10 }
                (Material.color lightOrange)
                (pointFromIndex marker tree)
            ]
    in
    case model.trackTree of
        Just tree ->
            renderTreeSelectively model.renderDepth tree <|
                renderCurrentMarker model.currentPosition tree

        Nothing ->
            []



--TODO: Factor the recursion away from the rendering and these two large routines combine.


renderMapJson : TrackLoaded -> E.Value
renderMapJson track =
    let
        boxSide =
            --TODO: put box side in model
            Length.kilometers 4

        mapLocation : GPXSource -> ( Angle, Angle )
        mapLocation point =
            let
                { longitude, latitude, altitude } =
                    point
            in
            ( Direction2d.toAngle longitude, latitude )

        makeVisibleSegment : PeteTree -> E.Value
        makeVisibleSegment node =
            lngLatPair <| mapLocation <| Tuple.second <| sourceData node

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
                            |> renderTree (depth - 1) notLeaf.right
                            |> renderTree (depth - 1) notLeaf.left

        renderTreeSelectively :
            BoundingBox3d Meters LocalCoords
            -> Int
            -> PeteTree
            -> List E.Value
            -> List E.Value
        renderTreeSelectively box depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode :: accum

                Node notLeaf ->
                    if notLeaf.nodeContent.boundingBox |> BoundingBox3d.intersects box then
                        -- Ignore depth cutoff near or in the box
                        accum
                            |> renderTreeSelectively box (depth - 1) notLeaf.right
                            |> renderTreeSelectively box (depth - 1) notLeaf.left

                    else
                        -- Outside box, apply cutoff.
                        accum
                            |> renderTree (depth - 1) notLeaf.right
                            |> renderTree (depth - 1) notLeaf.left

        renderFirstPoint treeNode =
            lngLatPair <| mapLocation <| Tuple.first <| sourceData treeNode

        current =
            startPoint <| leafFromIndex track.currentPosition track.trackTree

        detailBox =
            BoundingBox3d.withDimensions ( boxSide, boxSide, boxSide ) current

        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            renderFirstPoint track.trackTree
                :: renderTreeSelectively detailBox track.renderDepth track.trackTree []
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]
