module ViewGraph exposing (..)

{-
   This clone of ViewPlan is to be modified to draw SVG and add some interactive elements.
-}

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Arc2d
import Axis2d exposing (Axis2d)
import Camera3d exposing (Camera3d)
import Circle2d
import Dict
import Direction2d exposing (Direction2d)
import Direction3d exposing (negativeZ, positiveY, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette exposing (white)
import FlatColors.FlatUIPalette
import Frame2d
import Geometry.Svg as Svg
import Html.Attributes as Attributes
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Length, Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Polyline2d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity)
import Spherical exposing (metresPerPixel)
import Svg exposing (Svg)
import Svg.Attributes
import Tools.GraphOptions exposing (ClickDetect(..), Graph)
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import ViewPureStyles exposing (rgtDark, useIcon)
import Viewpoint3d exposing (Viewpoint3d)


type Msg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOp
    | ImageClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | PopupHide


type DragAction
    = DragNone
    | DragPan


type alias Context =
    { fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , clickPoint : Maybe ( Float, Float )
    , clickFeature : ClickDetect
    }


initialiseView :
    Int
    -> PeteTree
    -> Maybe Context
    -> Context
initialiseView current treeNode currentContext =
    case currentContext of
        Just context ->
            { context
                | fieldOfView = Angle.degrees 45
                , orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = 14.0
                , defaultZoomLevel = 14.0
                , focalPoint =
                    treeNode |> leafFromIndex current |> startPoint
                , waitingForClickDelay = False
            }

        Nothing ->
            { fieldOfView = Angle.degrees 45
            , orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 14.0
            , defaultZoomLevel = 14.0
            , focalPoint =
                treeNode |> leafFromIndex current |> startPoint
            , waitingForClickDelay = False
            , followSelectedPoint = False
            , clickPoint = Nothing
            , clickFeature = ClickNone
            }


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : (Msg -> msg) -> Context -> Element msg
zoomButtons msgWrapper context =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp >> msgWrapper)
        ]
        [ Input.button []
            { onPress = Just <| msgWrapper ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        ]


popup : (Msg -> msg) -> Context -> Element msg
popup msgWrapper context =
    let
        popupMenu =
            case context.clickFeature of
                ClickNone ->
                    none

                ClickNode node ->
                    text <| "Place " ++ String.fromInt node

                ClickEdge edge ->
                    text <| "Road " ++ String.fromInt edge
    in
    case context.clickPoint of
        Nothing ->
            none

        Just ( x, y ) ->
            row
                [ alignTop
                , alignLeft
                , moveDown y
                , moveRight x
                , Background.color rgtDark
                , Font.color FlatColors.ChinesePalette.antiFlashWhite
                , Font.size 14
                , padding 6
                , spacing 8
                , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp >> msgWrapper)
                , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp >> msgWrapper)
                , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp >> msgWrapper)
                , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp >> msgWrapper)
                ]
                [ popupMenu
                , Input.button []
                    { onPress = Just <| msgWrapper PopupHide
                    , label = useIcon FeatherIcons.x
                    }
                ]


onContextMenu : a -> Element.Attribute a
onContextMenu msg =
    HE.custom "contextmenu"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe Graph
    -> (Msg -> msg)
    -> Element msg
view context ( width, height ) mGraph msgWrapper =
    let
        dragging =
            context.dragAction

        camera =
            deriveCamera context

        -- Defines the shape of the 'screen' that we will be using when
        -- projecting 3D points into 2D
        screenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy (Quantity.toFloatQuantity width) (Quantity.toFloatQuantity height))

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        nodes2d =
            case mGraph of
                Just graph ->
                    graph.nodes
                        |> Dict.map (\k a -> a |> Point3d.toScreenSpace camera screenRectangle)

                Nothing ->
                    Dict.empty

        -- Create an SVG circle at each Node
        svgNodes =
            nodes2d
                |> Dict.map
                    (\index vertex ->
                        Svg.circle2d
                            [ Svg.Attributes.stroke "white"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "none"
                            ]
                            (Circle2d.withRadius (Pixels.float 6) vertex)
                    )
                |> Dict.values

        -- Create an SVG line for each Edge, approximated.
        svgEdges =
            case mGraph of
                Just graph ->
                    graph.edges
                        |> Dict.values
                        |> List.map
                            (\( node1, node2, edge ) ->
                                renderEdgeArc edge
                            )

                Nothing ->
                    []

        edgeAttributes colour =
            [ Svg.Attributes.stroke colour
            , Svg.Attributes.fill "none"
            , Svg.Attributes.strokeWidth "3"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            ]

        pointsAsPolyline : String -> List (Point2d.Point2d units coordinates) -> Svg msg
        pointsAsPolyline colour points =
            Svg.polyline2d
                (edgeAttributes colour)
                (Polyline2d.fromVertices points)

        edgeFold :
            RoadSection
            -> List (Point2d.Point2d Pixels coordinates)
            -> List (Point2d.Point2d Pixels coordinates)
        edgeFold road outputs =
            Point3d.toScreenSpace camera screenRectangle road.endPoint
                :: outputs

        renderEdge : PeteTree -> Svg msg
        renderEdge tree =
            let
                svgPoints =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        0
                        (skipCount tree)
                        (always <| Just 2)
                        0
                        tree
                        edgeFold
                        [ startPoint ]

                startPoint =
                    DomainModel.earthPointFromIndex 0 tree
                        |> Point3d.toScreenSpace camera screenRectangle
            in
            svgPoints |> pointsAsPolyline "black"

        renderEdgeArc : PeteTree -> Svg msg
        renderEdgeArc tree =
            -- If we can construct an arc, use it, otherwise just two lines.
            let
                ( start, mid, end ) =
                    ( DomainModel.earthPointFromIndex 0 tree
                        |> Point3d.toScreenSpace camera screenRectangle
                    , DomainModel.earthPointFromIndex (skipCount tree // 2) tree
                        |> Point3d.toScreenSpace camera screenRectangle
                    , DomainModel.earthPointFromIndex (skipCount tree) tree
                        |> Point3d.toScreenSpace camera screenRectangle
                    )
            in
            case Arc2d.throughPoints start mid end of
                Just arc ->
                    arc |> Svg.arc2d (edgeAttributes "black")

                Nothing ->
                    renderEdge tree

        textAttributes atPoint =
            [ Svg.Attributes.fill "rgb(250, 250, 250)"
            , Svg.Attributes.fontFamily "sans serif"
            , Svg.Attributes.fontSize "14px"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate atPoint) + 10))
            , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate atPoint)))
            ]

        -- Create text SVG labels beside each projected 2D point
        nodeLabels =
            nodes2d
                |> Dict.map
                    (\index vertex ->
                        Svg.text_
                            (textAttributes vertex)
                            [ Svg.text ("Place " ++ String.fromInt index) ]
                            -- Hack: flip the text upside down since our later
                            -- 'Svg.relativeTo topLeftFrame' call will flip it
                            -- back right side up
                            |> Svg.mirrorAcross (Axis2d.through vertex Direction2d.x)
                    )
                |> Dict.values

        -- Create text SVG labels beside each projected edge
        edgeLabels =
            case mGraph of
                Just graph ->
                    graph.edges
                        |> Dict.toList
                        |> List.map
                            (\( index, ( node1, node2, tree ) ) ->
                                let
                                    labelAt =
                                        earthPointFromIndex (skipCount tree // 2) tree
                                            |> Point3d.toScreenSpace camera screenRectangle
                                in
                                Svg.text_
                                    (textAttributes labelAt)
                                    [ Svg.text ("Road " ++ String.fromInt index) ]
                                    -- Hack: flip the text upside down since our later
                                    -- 'Svg.relativeTo topLeftFrame' call will flip it
                                    -- back right side up
                                    |> Svg.mirrorAcross (Axis2d.through labelAt Direction2d.x)
                            )

                Nothing ->
                    []

        -- Used for converting from coordinates relative to the bottom-left
        -- corner of the 2D drawing into coordinates relative to the top-left
        -- corner (which is what SVG natively works in)
        topLeftFrame =
            Frame2d.atPoint (Point2d.xy Quantity.zero (Quantity.toFloatQuantity height))
                |> Frame2d.reverseY

        -- Create an SVG element with the projected points, lines and
        -- associated labels
        svgElement =
            Svg.svg
                [ Attributes.width <| Pixels.inPixels width
                , Attributes.height <| Pixels.inPixels height
                ]
                [ Svg.relativeTo topLeftFrame
                    (Svg.g [] (svgNodes ++ svgEdges ++ nodeLabels ++ edgeLabels))
                ]
    in
    el
        [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
        , if dragging /= DragNone then
            htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

          else
            pointer
        , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
        , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
        , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
        , onContextMenu (msgWrapper ImageNoOp)
        , Element.width fill
        , Element.height fill
        , pointer
        , Border.width 0
        , Border.color FlatColors.ChinesePalette.peace
        , Background.color FlatColors.FlatUIPalette.emerald
        , inFront <| zoomButtons msgWrapper context
        , inFront <| popup msgWrapper context
        ]
    <|
        Element.html svgElement


deriveCamera :
    Context
    -> Camera3d Meters LocalCoords
deriveCamera context =
    let
        latitude =
            -- Not really important in this view.
            Angle.degrees 45

        lookingAt =
            context.focalPoint

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                lookingAt

        viewpoint =
            -- Fixing "up is North" so that 2-way drag works well.
            Viewpoint3d.lookAt
                { focalPoint = lookingAt
                , eyePoint = eyePoint
                , upDirection = Direction3d.positiveY
                }
    in
    Camera3d.orthographic
        { viewpoint = viewpoint
        , viewportHeight = Length.meters <| 1200.0 * metresPerPixel context.zoomLevel latitude
        }


update :
    Msg
    -> (Msg -> msg)
    -> Graph
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper graph area context =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            ( { context
                | orbiting = Just event.offsetPos
                , dragAction = DragPan
                , waitingForClickDelay = True
              }
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }
            , []
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( context.dragAction, context.orbiting ) of
                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            Vector3d.meters
                                (startX - dx)
                                (dy - startY)
                                0.0
                                |> Vector3d.scaleBy
                                    (1.0
                                        -- Empirical
                                        * Spherical.metresPerPixel
                                            context.zoomLevel
                                            (Angle.degrees 30)
                                    )
                    in
                    ( { context
                        | focalPoint =
                            context.focalPoint |> Point3d.translateBy shiftVector
                        , orbiting = Just ( dx, dy )
                      }
                    , []
                    )

                _ ->
                    ( context, [] )

        ImageRelease _ ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , []
            )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY
            in
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }
            , []
            )

        ImageClick event ->
            if context.waitingForClickDelay then
                ( { context
                    | clickPoint = Just <| event.offsetPos
                    , clickFeature = detectHit event graph area context
                  }
                , []
                )

            else
                ( context, [] )

        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            , []
            )

        ImageReset ->
            ( { context | zoomLevel = context.defaultZoomLevel }, [] )

        PopupHide ->
            ( { context | clickPoint = Nothing }, [] )

        ImageNoOp ->
            ( context, [] )


detectHit :
    Mouse.Event
    -> Graph
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ClickDetect
detectHit event graph ( w, h ) context =
    -- Need to see which edge is best.
    -- If we get a node 0 or `skipCount` from any edge, it's a Node.
    let
        ( x, y ) =
            event.offsetPos

        screenPoint =
            Point2d.pixels x y

        ( wFloat, hFloat ) =
            ( toFloatQuantity w, toFloatQuantity h )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            -- Must use same camera derivation as for the 3D model, else pointless!
            deriveCamera context

        ray =
            Camera3d.ray camera screenRectangle screenPoint

        candidates : List ( Int, ( Int, Bool, Quantity Float Pixels ) )
        candidates =
            graph.edges
                |> Dict.toList
                |> List.map
                    (\( edgeIndex, ( startNode, endNode, tree ) ) ->
                        let
                            thisEdgeNearestIndex =
                                nearestToRay ray tree

                            thisEdgeNearestPoint =
                                earthPointFromIndex thisEdgeNearestIndex tree
                                    |> Point3d.toScreenSpace camera screenRectangle
                        in
                        ( edgeIndex
                        , ( thisEdgeNearestIndex
                          , thisEdgeNearestIndex == skipCount tree
                          , Point2d.distanceFrom screenPoint thisEdgeNearestPoint
                          )
                        )
                    )

        bestCandidate =
            candidates
                |> List.Extra.minimumBy
                    (\( edge, ( point, isEnd, dist ) ) -> Pixels.inPixels dist)

        returnStartNode edgeIndex =
            case Dict.get edgeIndex graph.edges of
                Nothing ->
                    ClickNone

                Just ( startNode, endNode, tree ) ->
                    ClickNode startNode

        returnEndNode edgeIndex =
            case Dict.get edgeIndex graph.edges of
                Nothing ->
                    ClickNone

                Just ( startNode, endNode, tree ) ->
                    ClickNode endNode
    in
    case bestCandidate of
        Nothing ->
            ClickNone

        Just ( edgeIndex, ( pointIndex, isEnd, dist ) ) ->
            if pointIndex == 0 then
                returnStartNode edgeIndex

            else if isEnd then
                returnEndNode edgeIndex

            else
                ClickEdge edgeIndex
