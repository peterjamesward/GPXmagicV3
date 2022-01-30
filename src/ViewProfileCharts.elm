module ViewProfileCharts exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Chart as C
import Chart.Attributes as CA
import Circle2d
import Color
import Direction2d exposing (Direction2d)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Html exposing (Html)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Svg as Svg exposing (Svg, circle)
import Svg.Attributes as Attributes exposing (cx, cy, r)
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import ViewPureStyles exposing (useIcon)
import Viewpoint3d exposing (Viewpoint3d)



--TODO: Link into PaneLayoutManager.
--TODO: Make two view areas.
--TODO: Render altitude and gradient scene(s).
--TODO: Cameras, orthographic, centering, zoom.
--TODO: Tilt to compensate for zoom level.
--TODO: SVG scales overlay.


type ClickZone
    = ZoneAltitude
    | ZoneGradient


type Msg
    = ImageMouseWheel Float
    | ImageGrab ClickZone Mouse.Event
    | ImageDrag ClickZone Mouse.Event
    | ImageRelease ClickZone Mouse.Event
    | ImageNoOp
    | ImageClick ClickZone Mouse.Event
    | ImageDoubleClick ClickZone Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | ToggleFollowOrange


type DragAction
    = DragNone
    | DragPan


type alias Context =
    { altitudeCameraElevation : Angle -- Tilt to compensate for zoom, making for constant z-scale.
    , gradientCameraElevation : Angle -- Ditto
    , cameraDistance : Quantity Float Length.Meters -- Shared.
    , fieldOfView : Angle
    , dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
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
        , Border.width 1
        , Border.rounded 4
        , Border.color FlatColors.AussiePalette.blurple
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
        , Input.button []
            { onPress = Just <| msgWrapper ToggleFollowOrange
            , label =
                if context.followSelectedPoint then
                    useIcon FeatherIcons.lock

                else
                    useIcon FeatherIcons.unlock
            }
        ]


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context ( givenWidth, givenHeight ) track sceneAltitude sceneGradient msgWrapper =
    let
        dragging =
            context.dragAction

        splitProportion =
            0.5

        altitudePortion =
            -- Subtract pixels we use for padding around the scene view.
            ( givenWidth |> Quantity.minus (Pixels.pixels 20)
            , givenHeight
                |> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy splitProportion
                |> Quantity.truncate
            )

        gradientPortion =
            ( givenWidth |> Quantity.minus (Pixels.pixels 20)
            , givenHeight
                |> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy (1.0 - splitProportion)
                |> Quantity.truncate
            )
    in
    column
        [ htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
        , onContextMenu (msgWrapper ImageNoOp)
        , width fill
        , height fill
        , pointer
        , Border.width 0
        , Border.color FlatColors.ChinesePalette.peace
        , inFront <| zoomButtons msgWrapper context
        ]
        [ el
            [ htmlAttribute <| Mouse.onDown (ImageGrab ZoneAltitude >> msgWrapper)
            , if dragging /= DragNone then
                htmlAttribute <| Mouse.onMove (ImageDrag ZoneAltitude >> msgWrapper)

              else
                pointer
            , htmlAttribute <| Mouse.onUp (ImageRelease ZoneAltitude >> msgWrapper)
            , htmlAttribute <| Mouse.onClick (ImageClick ZoneAltitude >> msgWrapper)
            , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick ZoneAltitude >> msgWrapper)
            , padding 10
            , Background.color white
            , inFront <| html <| svgAltitudeScale altitudePortion context track
            ]
          <|
            html <|
                Scene3d.unlit
                    { camera = deriveAltitudeCamera track.trackTree context track.currentPosition
                    , dimensions = altitudePortion
                    , background = backgroundColor Color.white
                    , clipDepth = Length.meters 1
                    , entities = sceneAltitude
                    }
        , el
            [ htmlAttribute <| Mouse.onDown (ImageGrab ZoneGradient >> msgWrapper)
            , if dragging /= DragNone then
                htmlAttribute <| Mouse.onMove (ImageDrag ZoneGradient >> msgWrapper)

              else
                pointer
            , htmlAttribute <| Mouse.onUp (ImageRelease ZoneGradient >> msgWrapper)
            , htmlAttribute <| Mouse.onClick (ImageClick ZoneGradient >> msgWrapper)
            , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick ZoneGradient >> msgWrapper)
            , padding 10
            , Background.color white
            , inFront <| html <| svgGradientScale gradientPortion context track
            ]
          <|
            html <|
                Scene3d.unlit
                    { camera = deriveGradientCamera track.trackTree context track.currentPosition
                    , dimensions = gradientPortion
                    , background = backgroundColor Color.white
                    , clipDepth = Length.meters 1
                    , entities = sceneGradient
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


deriveAltitudeCamera : PeteTree -> Context -> Int -> Camera3d Meters LocalCoords
deriveAltitudeCamera treeNode context currentPosition =
    -- NOTE: SceneBuilder has exaggerated the scale by 5 times.
    -- Here, we will adjust camera elevation so that the full altitude range
    -- fits within the view regardless of zoom level. This approach because it
    -- avoids having to rebuild the scene.
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema <| boundingBox treeNode

        rangeOfY =
            -- The range we must fit within the viewport
            maxZ |> Quantity.minus minZ |> Quantity.multiplyBy 5.0

        viewportHeight =
            -- The vertical space available within the viewport, from the zoom level
            Length.meters <| 2 ^ (22 - context.zoomLevel)

        requiredReduction =
            if rangeOfY |> Quantity.greaterThan viewportHeight then
                Quantity.ratio viewportHeight rangeOfY

            else
                1.0

        elevationToReduce =
            Angle.radians <| acos requiredReduction

        altitudeLookingAt =
            if context.followSelectedPoint then
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    Quantity.zero

            else
                context.focalPoint

        altitudeViewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = altitudeLookingAt
                , azimuth = Direction2d.toAngle Direction2d.negativeY
                , elevation = elevationToReduce
                , distance = Length.kilometers 10
                }
    in
    Camera3d.orthographic
        { viewpoint = altitudeViewpoint
        , viewportHeight = viewportHeight
        }


deriveGradientCamera : PeteTree -> Context -> Int -> Camera3d Meters LocalCoords
deriveGradientCamera treeNode context currentPosition =
    let
        ( minZ, maxZ ) =
            -- Scene builder clamps to +/- 30%
            ( Length.meters -30, Length.meters 30 )

        rangeOfY =
            -- The range we must fit within the viewport.
            -- The times 25 is because I have no idea how this works.
            maxZ |> Quantity.minus minZ |> Quantity.multiplyBy 25.0

        viewportHeight =
            -- The vertical space available within the viewport, from the zoom level
            Length.meters <| 2 ^ (22 - context.zoomLevel)

        requiredReduction =
            if rangeOfY |> Quantity.greaterThan viewportHeight then
                Quantity.ratio viewportHeight rangeOfY

            else
                1.0

        elevationToReduce =
            Angle.radians <| acos requiredReduction

        gradientLookingAt =
            if context.followSelectedPoint then
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    Quantity.zero

            else
                Point3d.xyz
                    (Point3d.xCoordinate context.focalPoint)
                    Quantity.zero
                    Quantity.zero

        gradientViewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = gradientLookingAt
                , azimuth = Direction2d.toAngle Direction2d.negativeY
                , elevation = elevationToReduce
                , distance = Length.kilometer
                }
    in
    Camera3d.orthographic
        { viewpoint = gradientViewpoint
        , viewportHeight = Length.meters <| 2 ^ (22 - context.zoomLevel)
        }


svgAltitudeScale :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> TrackLoaded msg
    -> Html msg
svgAltitudeScale ( w, h ) context track =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            -- Domain extent across entire model, not current view.
            BoundingBox3d.extrema <| boundingBox track.trackTree

        maxDistance =
            Length.inKilometers <| trueLength track.trackTree

        currentPointAltitude =
            Length.inMeters <|
                Point3d.zCoordinate <|
                    earthPointFromIndex track.currentPosition track.trackTree

        currentPointDistance =
            Length.inMeters <|
                distanceFromIndex track.currentPosition track.trackTree
    in
    C.chart
        [ CA.height <| Pixels.inPixels <| Quantity.toFloatQuantity <| h
        , CA.width <| Pixels.inPixels <| Quantity.toFloatQuantity <| w
        , CA.margin { top = 20, bottom = 30, left = 30, right = 20 }
        , CA.range
            [ CA.lowest 0 CA.exactly
            , CA.highest maxDistance CA.orHigher
            ]
        , CA.domain
            [ CA.lowest 0 CA.orLower
            , CA.highest 200 CA.orHigher
            ]
        ]
        [ C.xLabels [ CA.amount 10, CA.alignLeft, CA.moveDown 20 ]
        , C.yLabels [ CA.amount 10, CA.moveRight 20, CA.withGrid ]
        , C.withPlane <|
            \p ->
                [ C.line
                    [ CA.x1 0
                    , CA.y1 currentPointAltitude
                    , CA.x2 p.x.max
                    , CA.dashed [ 5, 5 ]
                    , CA.color CA.red
                    ]
                , C.line
                    [ CA.x1 currentPointDistance
                    , CA.y1 0
                    , CA.y2 2000
                    , CA.dashed [ 5, 5 ]
                    , CA.color CA.blue
                    ]
                ]
        ]


svgGradientScale :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> TrackLoaded msg
    -> Html msg
svgGradientScale ( w, h ) context track =
    C.chart
        [ CA.height <| Pixels.inPixels <| Quantity.toFloatQuantity <| h
        , CA.width <| Pixels.inPixels <| Quantity.toFloatQuantity <| w
        , CA.margin { top = 20, bottom = 30, left = 30, right = 20 }
        , CA.range
            [ CA.lowest 0 CA.orLower
            , CA.highest 1300 CA.orHigher
            ]
        , CA.domain
            [ CA.lowest -30 CA.orLower
            , CA.highest 30 CA.orHigher
            ]
        ]
        [ C.series .x
            []
            [ { x = 0, y = 0 }
            , { x = 800, y = 0 }
            ]
        , C.yLabels [ CA.amount 10, CA.withGrid ]
        ]


extentOfVisibleModel :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> TrackLoaded msg
    -> ( Maybe (Point3d Meters LocalCoords), Maybe (Point3d Meters LocalCoords) )
extentOfVisibleModel ( w, h ) context track =
    let
        ( wFloat, hFloat ) =
            ( toFloatQuantity w, toFloatQuantity h )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            -- Must use same camera derivation as for the 3D model, else pointless!
            deriveAltitudeCamera track.trackTree context track.currentPosition

        ( blueCorner, redCorner ) =
            ( Point2d.pixels 0 0, Point2d.xy wFloat hFloat )

        ( blueRay, redRay ) =
            ( Camera3d.ray camera screenRectangle blueCorner
            , Camera3d.ray camera screenRectangle redCorner
            )

        ( bluePoint, redPoint ) =
            ( blueRay |> Axis3d.intersectionWithPlane Plane3d.zx
            , redRay |> Axis3d.intersectionWithPlane Plane3d.zx
            )
    in
    ( bluePoint, redPoint )


metresPerPixel :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> TrackLoaded msg
    -> Float
metresPerPixel ( w, h ) context track =
    let
        ( wFloat, hFloat ) =
            ( toFloatQuantity w, toFloatQuantity h )
    in
    case extentOfVisibleModel ( w, h ) context track of
        ( Just blue, Just red ) ->
            Point3d.xCoordinate blue
                |> Quantity.minus (Point3d.xCoordinate red)
                |> Quantity.abs
                |> Length.inMeters
                |> (\len -> len / Pixels.inPixels wFloat)

        _ ->
            -- Oh!
            1.0


modelPointFromClick :
    Mouse.Event
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> TrackLoaded msg
    -> Maybe EarthPoint
modelPointFromClick event ( w, h ) context track =
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
            deriveAltitudeCamera track.trackTree context track.currentPosition

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    ray |> Axis3d.intersectionWithPlane Plane3d.zx


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> Int
detectHit event track ( w, h ) context =
    case modelPointFromClick event ( w, h ) context track of
        Just pointOnZX ->
            DomainModel.indexFromDistance (Point3d.xCoordinate pointOnZX) track.trackTree

        Nothing ->
            -- Leave position unchanged; should not occur.
            track.currentPosition


minZoomLevel treeNode =
    -- More empiricism, which is really embarrassing.
    23.75 - logBase 2.0 (Length.inMeters <| trueLength treeNode)


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) context =
    let
        splitProportion =
            --TODO: Remove duplicate with `view`
            0.5

        altitudePortion =
            ( givenWidth
            , givenHeight
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy splitProportion
                |> Quantity.truncate
            )

        gradientPortion =
            ( givenWidth
            , givenHeight
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy (1.0 - splitProportion)
                |> Quantity.truncate
            )

        areaForZone zone =
            case zone of
                ZoneAltitude ->
                    altitudePortion

                ZoneGradient ->
                    gradientPortion

        centre =
            BoundingBox3d.centerPoint <| boundingBox track.trackTree
    in
    case msg of
        ImageZoomIn ->
            ( { context | zoomLevel = clamp (minZoomLevel track.trackTree) 22.0 <| context.zoomLevel + 0.5 }
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp (minZoomLevel track.trackTree) 22.0 <| context.zoomLevel - 0.5 }
            , []
            )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context), [] )

        ImageNoOp ->
            ( context, [] )

        ImageClick zone event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track (areaForZone zone) context
                  , TrackHasChanged
                  ]
                )

            else
                ( context, [] )

        ImageDoubleClick zone event ->
            let
                nearestPoint =
                    detectHit event track (areaForZone zone) context
            in
            ( { context | focalPoint = earthPointFromIndex nearestPoint track.trackTree }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [] )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                zoomLevel =
                    clamp (minZoomLevel track.trackTree) 22.0 <| context.zoomLevel + increment
            in
            ( { context | zoomLevel = zoomLevel }, [] )

        ImageGrab zone event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                newContext =
                    { context
                        | orbiting = Just event.offsetPos
                        , dragAction = DragPan
                        , waitingForClickDelay = True
                        , metresPerPixel = metresPerPixel (areaForZone zone) context track
                    }
            in
            ( newContext
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            )

        ImageDrag zone event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( context.dragAction, context.orbiting ) of
                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            --TODO: Find out how to do the pixel calculation. See examples?
                            Vector3d.meters
                                --((startX - dx) * 1.15 ^ (22 - context.zoomLevel))
                                ((startX - dx) * context.metresPerPixel)
                                0
                                0

                        newContext =
                            { context
                                | focalPoint =
                                    context.focalPoint |> Point3d.translateBy shiftVector
                                , orbiting = Just ( dx, dy )
                            }
                    in
                    ( newContext, [] )

                _ ->
                    ( context, [] )

        ImageRelease zone event ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , []
            )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint =
                    Point3d.xyz
                        (distanceFromIndex track.currentPosition track.trackTree)
                        Quantity.zero
                        (Point3d.zCoordinate centre)
              }
            , []
            )


initialiseView :
    Int
    -> PeteTree
    -> Maybe Context
    -> Context
initialiseView current treeNode currentContext =
    case currentContext of
        Just context ->
            { context
                | altitudeCameraElevation = Angle.degrees 0
                , gradientCameraElevation = Angle.degrees 0
                , cameraDistance = Length.kilometers 10
                , fieldOfView = Angle.degrees 45
                , orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = minZoomLevel treeNode
                , defaultZoomLevel = 10.0
                , focalPoint = treeNode |> leafFromIndex current |> startPoint
                , waitingForClickDelay = False
                , metresPerPixel = 10.0
            }

        Nothing ->
            { altitudeCameraElevation = Angle.degrees 0
            , gradientCameraElevation = Angle.degrees 0
            , cameraDistance = Length.kilometers 10
            , fieldOfView = Angle.degrees 45
            , orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = minZoomLevel treeNode
            , defaultZoomLevel = minZoomLevel treeNode
            , focalPoint = treeNode |> leafFromIndex current |> startPoint
            , waitingForClickDelay = False
            , followSelectedPoint = False
            , metresPerPixel = 10.0
            }
