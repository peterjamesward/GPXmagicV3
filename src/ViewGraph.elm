module ViewGraph exposing
    ( Msg(..)
    , initialiseView
    , update
    , view
    )

{-
   This clone of ViewPlan is to be modified to draw SVG and add some interactive elements.
-}

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis2d
import Camera3d exposing (Camera3d)
import Circle2d
import CommonToolStyles
import Dict
import Direction2d exposing (toAngle)
import Direction3d
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
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels, inPixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Point3d.Projection as Point3d
import Polyline2d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import SketchPlane3d
import Spherical exposing (metresPerPixel)
import Svg exposing (Svg)
import Svg.Attributes
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (myTooltip, tooltip)
import Tools.GraphOptions as Graph exposing (Edge, Graph)
import Tools.Tracks as Tracks
import Tools.TracksOptions as Tracks exposing (ClickDetect(..), Direction(..), GraphState(..))
import UtilsForViews exposing (showShortMeasure, uiColourHexString)
import Vector2d
import Vector3d
import ViewGraphContext exposing (DragAction(..), EdgeMode(..), GraphContext)
import ViewPureStyles exposing (rgtDark, rgtPurple, useIcon)
import Viewpoint3d


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
    | AddTraversal String
    | AddSelfLoop String


initialiseView :
    Int
    -> PeteTree
    -> Maybe GraphContext
    -> GraphContext
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
            , mouseHere = Point2d.origin
            }


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : SystemSettings -> (Msg -> msg) -> GraphContext -> Element msg
zoomButtons settings msgWrapper context =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Font.size 40
        , padding 6
        , spacing 8
        , Background.color (CommonToolStyles.themeBackground settings.colourTheme)
        , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
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


popupEditingMenu :
    (Msg -> msg)
    -> GraphContext
    -> Tracks.Options msg
    -> Element msg
popupEditingMenu msgWrapper context options =
    let
        popupMenu =
            case context.clickFeature of
                ClickNone ->
                    []

                ClickNode node ->
                    [ text <| node
                    , if Tracks.loopCanBeAdded node options then
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
                    [ text edge
                    , if Tracks.traversalCanBeAdded edge options then
                        Input.button []
                            { onPress = Just <| msgWrapper <| AddTraversal edge
                            , label = text "Add to route"
                            }

                      else
                        none
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
    SystemSettings
    -> GraphContext
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Tracks.Options msg
    -> (Msg -> msg)
    -> Element msg
view settings context ( width, height ) options msgWrapper =
    let
        graph =
            options.graph

        camera =
            deriveCamera context

        -- Defines the shape of the 'screen' that we will be using when
        -- projecting 3D points into 2D
        screenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy (Quantity.toFloatQuantity width) (Quantity.toFloatQuantity height))

        nodes2d =
            graph.nodes
                |> Dict.map
                    (\_ pt ->
                        pt.space
                            |> Point3d.projectOnto Plane3d.xy
                            |> Point3d.toScreenSpace camera screenRectangle
                    )

        -- Create an SVG circle at each Node
        svgNodes =
            nodes2d
                |> Dict.map
                    (\_ vertex ->
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
                    (\( index, edgeInfo ) ->
                        case context.edgeMode of
                            EdgeArc ->
                                renderEdge 3 index edgeInfo

                            EdgeSketch ->
                                renderEdge 8 index edgeInfo
                    )

        pointsAsPolyline : String -> List (Point2d.Point2d units coordinates) -> Svg msg
        pointsAsPolyline edgeIndex points =
            let
                edgeAttributes highlight =
                    if highlight then
                        [ Svg.Attributes.stroke <| uiColourHexString rgtPurple
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.strokeWidth "5"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        , Svg.Attributes.markerMid "url(#arrow)"
                        ]

                    else
                        [ Svg.Attributes.stroke <| uiColourHexString FlatColors.FlatUIPalette.peterRiver
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.strokeWidth "3"
                        , Svg.Attributes.strokeLinecap "round"
                        , Svg.Attributes.strokeLinejoin "round"
                        ]
            in
            case List.Extra.getAt options.selectedTraversal options.userRoute of
                Just selected ->
                    if selected.edge == edgeIndex then
                        -- Must get orientation correct.
                        if selected.direction == Reverse then
                            Svg.polyline2d
                                (edgeAttributes True)
                                (Polyline2d.fromVertices points)

                        else
                            Svg.polyline2d
                                (edgeAttributes True)
                                (Polyline2d.fromVertices <| List.reverse points)

                    else
                        Svg.polyline2d
                            (edgeAttributes False)
                            (Polyline2d.fromVertices <| List.reverse points)

                Nothing ->
                    Svg.polyline2d
                        (edgeAttributes False)
                        (Polyline2d.fromVertices <| List.reverse points)

        edgeFold :
            RoadSection
            -> List (Point2d.Point2d Pixels coordinates)
            -> List (Point2d.Point2d Pixels coordinates)
        edgeFold road outputs =
            Point3d.toScreenSpace camera screenRectangle road.endPoint.space
                :: outputs

        renderEdge : Int -> String -> Edge msg -> Svg msg
        renderEdge depth edgeKey edge =
            let
                svgPoints =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        0
                        (skipCount edge.track.trackTree)
                        (always <| Just depth)
                        0
                        edge.track.trackTree
                        edgeFold
                        [ startPoint ]

                startPoint =
                    DomainModel.startPoint edge.track.trackTree
                        |> .space
                        |> Point3d.toScreenSpace camera screenRectangle
            in
            svgPoints |> pointsAsPolyline edgeKey

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
                            [ Svg.text index ]
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
                    (\( edgeKey, edgeInfo ) ->
                        let
                            labelAt =
                                DomainModel.midPoint edgeInfo.track.trackTree
                                    |> .space
                                    |> Point3d.toScreenSpace camera screenRectangle
                        in
                        Svg.text_
                            (textAttributes labelAt)
                            [ Svg.text edgeKey ]
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
                [ Svg.defs []
                    [ Svg.marker
                        [ Svg.Attributes.id "arrow"
                        , Svg.Attributes.viewBox "0 0 10 10"
                        , Svg.Attributes.refX "5"
                        , Svg.Attributes.refY "5"
                        , Svg.Attributes.markerWidth "3"
                        , Svg.Attributes.markerHeight "3"
                        , Svg.Attributes.orient "auto-start-reverse"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.d "M 0 0 L 10 5 L 0 10 z" ]
                            []
                        ]
                    ]
                , Svg.relativeTo topLeftFrame
                    (Svg.g []
                        (svgNodes
                            ++ svgEdges
                            ++ nodeLabels
                            ++ edgeLabels
                        )
                    )
                ]
    in
    el
        [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
        , -- if dragging /= DragNone then
          htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)
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
        , inFront <| zoomButtons settings msgWrapper context
        , inFront <|
            case options.graphState of
                GraphWithEdges ->
                    popupEditingMenu msgWrapper context options

                _ ->
                    none
        ]
    <|
        Element.html svgElement


deriveCamera :
    GraphContext
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
                lookingAt.space

        viewpoint =
            -- Fixing "up is North" so that 2-way drag works well.
            Viewpoint3d.lookAt
                { focalPoint = lookingAt.space
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
    -> Tracks.Options msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> GraphContext
    -> ( GraphContext, Tracks.Options msg, List (ToolAction msg) )
update msg msgWrapper tracks area context =
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
            , tracks
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }
            , tracks
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
                                    (Spherical.metresPerPixel
                                        context.zoomLevel
                                        (Angle.degrees 30)
                                    )
                    in
                    ( { context
                        | focalPoint =
                            context.focalPoint.space
                                |> Point3d.translateBy shiftVector
                                |> DomainModel.withoutTime
                        , orbiting = Just ( dx, dy )
                      }
                    , tracks
                    , []
                    )

                _ ->
                    -- Not dragging, just track mouse so we can use it to centre zooming.
                    ( { context | mouseHere = Point2d.fromTuple Pixels.pixels event.offsetPos }
                    , tracks
                    , []
                    )

        ImageRelease _ ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , tracks
            , []
            )

        ImageMouseWheel deltaY ->
            --1. Find the vector in model space from focal point to mouse before the zoom.
            --2. Change the zoom.
            --3. Reverse vector (1) from mouse to give new focal point.
            -- (This may simplify!)
            let
                increment =
                    -0.001 * deltaY

                newZoom =
                    clamp 0.0 22.0 <| context.zoomLevel + increment

                ( width, height ) =
                    area

                screenCentre =
                    Point2d.midpoint
                        Point2d.origin
                        (Point2d.xy (Quantity.toFloatQuantity width) (Quantity.toFloatQuantity height))

                shiftVectorBefore =
                    Vector2d.from context.mouseHere screenCentre
                        |> Vector2d.scaleBy
                            (Spherical.metresPerPixel context.zoomLevel (Angle.degrees 30))

                shiftVectorAfter =
                    Vector2d.from context.mouseHere screenCentre
                        |> Vector2d.scaleBy
                            (Spherical.metresPerPixel newZoom (Angle.degrees 30))

                shift =
                    shiftVectorAfter
                        |> Vector2d.minus shiftVectorBefore
                        |> Vector2d.toTuple inPixels
                        |> Vector2d.fromTuple meters
                        |> Vector2d.mirrorAcross Axis2d.x

                newFocalPoint =
                    context.focalPoint.space
                        |> Point3d.translateBy
                            (Vector3d.on SketchPlane3d.xy shift)
            in
            ( { context
                | zoomLevel = newZoom
                , focalPoint = DomainModel.withoutTime newFocalPoint
              }
            , tracks
            , []
            )

        ImageClick event ->
            if context.waitingForClickDelay then
                ( { context
                    | clickPoint = Just <| event.offsetPos
                    , clickFeature = detectHit event tracks.graph area context
                  }
                , tracks
                , []
                )

            else
                ( context, tracks, [] )

        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            , tracks
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            , tracks
            , []
            )

        ImageReset ->
            ( { context | zoomLevel = context.defaultZoomLevel }, tracks, [] )

        PopupHide ->
            ( { context | clickPoint = Nothing, clickFeature = ClickNone }, tracks, [] )

        ImageNoOp ->
            ( context, tracks, [] )

        ToggleEdgeMode ->
            ( { context
                | edgeMode =
                    case context.edgeMode of
                        EdgeSketch ->
                            EdgeArc

                        EdgeArc ->
                            EdgeSketch
              }
            , tracks
            , []
            )

        AddTraversal edge ->
            ( { context | clickPoint = Nothing, clickFeature = ClickNone }
            , Tracks.addTraversal edge tracks
            , []
            )

        AddSelfLoop node ->
            ( { context | clickPoint = Nothing, clickFeature = ClickNone }
            , Tracks.addSelfLoop node tracks
            , []
            )


detectHit :
    Mouse.Event
    -> Graph msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> GraphContext
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

        candidates : List ( String, ( Int, Bool, Quantity Float Pixels ) )
        candidates =
            graph.edges
                |> Dict.toList
                |> List.map
                    (\( edgeIndex, edgeInfo ) ->
                        let
                            thisEdgeNearestIndex =
                                nearestToRay
                                    ray
                                    edgeInfo.track.trackTree
                                    edgeInfo.track.leafIndex
                                    edgeInfo.track.currentPosition

                            thisEdgeNearestPoint =
                                earthPointFromIndex thisEdgeNearestIndex edgeInfo.track.trackTree
                                    |> .space
                                    |> Point3d.toScreenSpace camera screenRectangle
                        in
                        ( edgeIndex
                        , ( thisEdgeNearestIndex
                          , thisEdgeNearestIndex == skipCount edgeInfo.track.trackTree
                          , Point2d.distanceFrom screenPoint thisEdgeNearestPoint
                          )
                        )
                    )

        bestCandidate =
            candidates
                |> List.Extra.minimumBy
                    (\( _, ( _, _, dist ) ) -> Pixels.inPixels dist)

        returnStartNode edgeIndex =
            case Dict.get edgeIndex graph.edges of
                Nothing ->
                    ClickNone

                Just edgeInfo ->
                    ClickNode edgeInfo.lowNode

        returnEndNode edgeIndex =
            case Dict.get edgeIndex graph.edges of
                Nothing ->
                    ClickNone

                Just edgeInfo ->
                    ClickNode edgeInfo.highNode
    in
    case bestCandidate of
        Nothing ->
            ClickNone

        Just ( edgeIndex, ( pointIndex, isEnd, _ ) ) ->
            if pointIndex == 0 then
                returnStartNode edgeIndex

            else if isEnd then
                returnEndNode edgeIndex

            else
                ClickEdge edgeIndex
