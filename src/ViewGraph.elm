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
import Direction2d exposing (Direction2d, toAngle)
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
import Json.Encode as E
import Length exposing (Length, Meters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Polyline2d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import SketchPlane3d
import Spherical exposing (metresPerPixel)
import Svg exposing (Svg)
import Svg.Attributes
import ToolTip exposing (myTooltip, tooltip)
import Tools.Graph
import Tools.GraphOptions exposing (ClickDetect(..), Direction(..), Graph)
import UtilsForViews exposing (colourHexString, showShortMeasure, uiColourHexString)
import Vector3d
import ViewPureStyles exposing (rgtDark, rgtPurple, useIcon)
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
    | ToggleEdgeMode
    | AddTraversal Int
    | EditRoad Int
    | AddSelfLoop Int


type DragAction
    = DragNone
    | DragPan


type EdgeMode
    = EdgeArc
    | EdgeSketch


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
    , edgeMode : EdgeMode
    , haveDisplayedEditingReminder : Bool
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
            , edgeMode = EdgeSketch
            , haveDisplayedEditingReminder = False
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
        , Input.button
            [ tooltip onLeft (myTooltip "Change how\nroads are drawn")
            ]
            { onPress = Just <| msgWrapper ToggleEdgeMode
            , label = useIcon FeatherIcons.activity
            }
        ]


popup : (Msg -> msg) -> Context -> Tools.GraphOptions.Options msg -> Element msg
popup msgWrapper context options =
    let
        popupMenu =
            case context.clickFeature of
                ClickNone ->
                    []

                ClickNode node ->
                    [ text <| "Place " ++ String.fromInt node ++ "..."
                    , if Tools.Graph.loopCanBeAdded node options then
                        Input.button []
                            { onPress = Just <| msgWrapper <| AddSelfLoop node
                            , label =
                                text <|
                                    "Add "
                                        ++ showShortMeasure False options.minimumRadiusAtPlaces
                                        ++ " loop here"
                            }

                      else
                        none
                    , Input.button []
                        { onPress = Just <| msgWrapper PopupHide
                        , label = text "Close menu"
                        }
                    ]

                ClickEdge edge ->
                    [ text <| "Road " ++ String.fromInt edge ++ "..."
                    , if Tools.Graph.edgeCanBeAdded edge options then
                        Input.button []
                            { onPress = Just <| msgWrapper <| AddTraversal edge
                            , label = text "Add to route"
                            }

                      else
                        none
                    , Input.button []
                        { onPress = Just <| msgWrapper <| EditRoad edge
                        , label = text "Edit this road"
                        }
                    , Input.button []
                        { onPress = Just <| msgWrapper PopupHide
                        , label = text "Close menu"
                        }
                    ]
    in
    case context.clickPoint of
        Nothing ->
            none

        Just ( x, y ) ->
            column
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
                popupMenu


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
    -> Graph msg
    -> Tools.GraphOptions.Options msg
    -> (Msg -> msg)
    -> Element msg
view context ( width, height ) graph options msgWrapper =
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
            graph.nodes
                |> Dict.map
                    (\idx pt ->
                        pt
                            |> Point2d.fromTuple meters
                            |> Point3d.on SketchPlane3d.xy
                            |> Point3d.toScreenSpace camera screenRectangle
                    )

        -- Create an SVG circle at each Node
        svgNodes =
            nodes2d
                |> Dict.map
                    (\index vertex ->
                        Svg.circle2d
                            [ Svg.Attributes.stroke "red"
                            , Svg.Attributes.strokeWidth "3"
                            , Svg.Attributes.fill "none"
                            ]
                            (Circle2d.withRadius (Pixels.float 8) vertex)
                    )
                |> Dict.values

        -- Create an SVG line for each Edge, approximated.
        svgEdges =
            graph.edges
                |> Dict.toList
                |> List.map
                    (\( index, ( ( node1, node2, disc ), edge ) ) ->
                        case context.edgeMode of
                            EdgeArc ->
                                renderEdgeArc index edge.trackTree

                            EdgeSketch ->
                                renderEdge index edge.trackTree
                    )

        edgeToHighlight =
            List.Extra.getAt options.selectedTraversal graph.userRoute
                |> Maybe.map .edge
                |> Maybe.withDefault -1

        arrowAttributes atPoint =
            [ Svg.Attributes.fill <| uiColourHexString rgtPurple
            , Svg.Attributes.fontFamily "sans serif"
            , Svg.Attributes.fontSize "20px"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate atPoint) + 10))
            , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate atPoint) + 10))
            ]

        arrowsOnHighlightedEdge =
            case List.Extra.getAt options.selectedTraversal graph.userRoute of
                Nothing ->
                    []

                Just { edge, direction } ->
                    case Dict.get edge graph.edges of
                        Nothing ->
                            []

                        Just ( _, edgeTrack ) ->
                            let
                                midPoint =
                                    skipCount edgeTrack.trackTree // 2

                                midLeaf =
                                    asRecord <| leafFromIndex midPoint edgeTrack.trackTree

                                ( p1, p2 ) =
                                    ( midLeaf.startPoint |> Point3d.toScreenSpace camera screenRectangle
                                    , midLeaf.endPoint |> Point3d.toScreenSpace camera screenRectangle
                                    )

                                rotation =
                                    case direction of
                                        Natural ->
                                            toAngle midLeaf.directionAtStart

                                        Reverse ->
                                            toAngle <| Direction2d.reverse midLeaf.directionAtStart
                            in
                            [ Svg.text_
                                (arrowAttributes p1)
                                [ Svg.text ">>>>" ]
                                -- Hack: flip the text upside down since our later
                                -- 'Svg.relativeTo topLeftFrame' call will flip it
                                -- back right side up
                                |> Svg.mirrorAcross (Axis2d.through p1 Direction2d.x)
                                |> Svg.rotateAround p1 rotation
                            ]

        edgeAttributes edgeIndex =
            if edgeIndex == edgeToHighlight then
                [ Svg.Attributes.stroke <| uiColourHexString rgtPurple
                , Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "5"
                , Svg.Attributes.strokeLinecap "round"
                , Svg.Attributes.strokeLinejoin "round"
                ]

            else
                [ Svg.Attributes.stroke <| uiColourHexString FlatColors.FlatUIPalette.peterRiver
                , Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinecap "round"
                , Svg.Attributes.strokeLinejoin "round"
                ]

        pointsAsPolyline : Int -> List (Point2d.Point2d units coordinates) -> Svg msg
        pointsAsPolyline edgeIndex points =
            Svg.polyline2d
                (edgeAttributes edgeIndex)
                (Polyline2d.fromVertices points)

        edgeFold :
            RoadSection
            -> List (Point2d.Point2d Pixels coordinates)
            -> List (Point2d.Point2d Pixels coordinates)
        edgeFold road outputs =
            Point3d.toScreenSpace camera screenRectangle road.endPoint
                :: outputs

        renderEdge : Int -> PeteTree -> Svg msg
        renderEdge edgeIndex tree =
            let
                svgPoints =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        0
                        (skipCount tree)
                        (always <| Just 8)
                        0
                        tree
                        edgeFold
                        [ startPoint ]

                startPoint =
                    DomainModel.earthPointFromIndex 0 tree
                        |> Point3d.toScreenSpace camera screenRectangle
            in
            svgPoints |> pointsAsPolyline edgeIndex

        renderEdgeArc : Int -> PeteTree -> Svg msg
        renderEdgeArc edgeIndex tree =
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
                    arc |> Svg.arc2d (edgeAttributes edgeIndex)

                Nothing ->
                    renderEdge edgeIndex tree

        textAttributes atPoint =
            [ Svg.Attributes.fill "rgb(250, 250, 250)"
            , Svg.Attributes.fontFamily "sans serif"
            , Svg.Attributes.fontSize "16px"
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
            graph.edges
                |> Dict.toList
                |> List.map
                    (\( index, ( ( node1, node2, disc ), track ) ) ->
                        let
                            labelAt =
                                earthPointFromIndex (skipCount track.trackTree // 2) track.trackTree
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
                    (Svg.g []
                        (svgNodes
                            ++ svgEdges
                            ++ nodeLabels
                            ++ edgeLabels
                            ++ arrowsOnHighlightedEdge
                        )
                    )
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
        , Background.color FlatColors.FlatUIPalette.silver
        , inFront <| zoomButtons msgWrapper context
        , inFront <| popup msgWrapper context options
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
    -> Graph msg
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
            ( { context | clickPoint = Nothing, clickFeature = ClickNone }, [] )

        ImageNoOp ->
            ( context, [] )

        ToggleEdgeMode ->
            ( { context
                | edgeMode =
                    case context.edgeMode of
                        EdgeSketch ->
                            EdgeArc

                        EdgeArc ->
                            EdgeSketch
              }
            , []
            )

        AddTraversal edge ->
            ( { context | clickPoint = Nothing, clickFeature = ClickNone }
            , [ Actions.AddTraversal edge ]
            )

        AddSelfLoop node ->
            ( { context | clickPoint = Nothing, clickFeature = ClickNone }
            , [ Actions.AddSelfLoop node ]
            )

        EditRoad edge ->
            ( { context
                | clickPoint = Nothing
                , clickFeature = ClickNone
                , haveDisplayedEditingReminder = True
              }
            , [ Actions.ChangeActiveTrack edge
              , Actions.TrackHasChanged
              , Actions.StoreLocally "editmessagedisplayed" (E.bool True)
              , if context.haveDisplayedEditingReminder then
                    Actions.NoAction

                else
                    Actions.DisplayInfo "graph" "edit"
              ]
            )


detectHit :
    Mouse.Event
    -> Graph msg
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
                    (\( edgeIndex, ( ( startNode, endNode, disc ), track ) ) ->
                        let
                            thisEdgeNearestIndex =
                                nearestToRay ray track.trackTree

                            thisEdgeNearestPoint =
                                earthPointFromIndex thisEdgeNearestIndex track.trackTree
                                    |> Point3d.toScreenSpace camera screenRectangle
                        in
                        ( edgeIndex
                        , ( thisEdgeNearestIndex
                          , thisEdgeNearestIndex == skipCount track.trackTree
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

                Just ( ( startNode, endNode, disc ), tree ) ->
                    ClickNode startNode

        returnEndNode edgeIndex =
            case Dict.get edgeIndex graph.edges of
                Nothing ->
                    ClickNone

                Just ( ( startNode, endNode, disc ), tree ) ->
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
