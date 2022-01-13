module SceneBuilder exposing (..)

-- In V3 there is only one 3d model, used for first and third views.
-- Plan and Profile are 2d drawings.

import Actions exposing (PreviewData, PreviewShape(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientHue, gradientHue2)
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (..)
import Element
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
import TrackLoaded exposing (TrackLoaded)
import Vector3d


render3dView : TrackLoaded -> List (Entity LocalCoords)
render3dView track =
    let
        floorPlane =
            Plane3d.xy |> Plane3d.offsetBy (BoundingBox3d.minZ <| boundingBox track.trackTree)

        fullRenderingZone =
            BoundingBox3d.withDimensions ( boxSide, boxSide, boxSide )
                (startPoint <| leafFromIndex track.currentPosition track.trackTree)

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
                (earthPointFromIndex marker tree)
            ]
    in
    renderTreeSelectively track.renderDepth track.trackTree <|
        renderCurrentMarker track.currentPosition track.trackTree



renderPreviews : Dict String PreviewData -> List (Entity LocalCoords)
renderPreviews previews =
    let
        onePreview :
            { tag : String
            , shape : PreviewShape
            , colour : Element.Color
            , points : List ( EarthPoint, GPXSource )
            }
            -> List (Entity LocalCoords)
        onePreview { tag, shape, colour, points } =
            case shape of
                PreviewCircle ->
                    previewAsPoints colour <| List.map Tuple.first points

                PreviewLine ->
                    previewAsLine colour <| List.map Tuple.first points
    in
    previews |> Dict.values |> List.concatMap onePreview


previewAsLine : Element.Color -> List EarthPoint -> List (Entity LocalCoords)
previewAsLine color points =
    let
        material =
            Material.matte <| Color.fromRgba <| Element.toRgb color

        preview p1 p2 =
            paintSomethingBetween
                (Length.meters 0.5)
                material
                p1
                p2
    in
    List.map2 preview points (List.drop 1 points) |> List.concat


previewAsPoints : Element.Color -> List EarthPoint -> List (Entity LocalCoords)
previewAsPoints color points =
    let
        material =
            Material.color <| Color.fromRgba <| Element.toRgb color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 7 } material p
    in
    List.map highlightPoint points


paintSomethingBetween width material pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from pt1 pt2

        halfWidth =
            Vector3d.from pt1 pt2
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.scaleTo width

        ( leftKerbVector, rightKerbVector ) =
            ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidth
            , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidth
            )

        ( leftKerb, rightKerb ) =
            ( LineSegment3d.translateBy leftKerbVector roadAsSegment
            , LineSegment3d.translateBy rightKerbVector roadAsSegment
            )
    in
    [ Scene3d.quad material
        (LineSegment3d.startPoint leftKerb)
        (LineSegment3d.endPoint leftKerb)
        (LineSegment3d.endPoint rightKerb)
        (LineSegment3d.startPoint rightKerb)
    ]
