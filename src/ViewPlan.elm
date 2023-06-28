module ViewPlan exposing
    ( Msg(..)
    , initialiseView
    , resizeOccured
    , subscriptions
    , trackChanged
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Arc2d
import Axis2d
import Axis3d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Circle2d
import CommonToolStyles
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Cursor as Cursor
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette exposing (white)
import FlatColors.IndianPalette
import Frame2d
import Geometry.Svg as Svg
import Html
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LineSegment2d
import LineSegment3d
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import MapboxKey
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder3D
import SketchPlane3d
import Spherical exposing (metresPerPixel)
import Svg
import Svg.Attributes
import SystemSettings exposing (SystemSettings)
import ToolTip
import Tools.DisplaySettingsOptions
import Tools.Tracks as Tracks
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (colorFromElmUiColour)
import Vector3d
import View3dCommonElements exposing (placesOverlay)
import ViewPlanContext exposing (DragAction(..), PlanContext)
import ViewPureStyles exposing (useIcon)
import Viewpoint3d
import ZoomLevel


type Msg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOp
    | ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | ToggleFollowOrange
    | MapMsg MapViewer.Msg
    | ToggleShowMap
    | ToggleFingerpainting


subscriptions : MapViewer.MapData -> PlanContext -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


initialiseView :
    Int
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe PlanContext
    -> PlanContext
initialiseView current track contentArea currentContext =
    let
        treeNode =
            track.trackTree

        box =
            DomainModel.boundingBox treeNode

        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box

        noPadding =
            { left = 0, right = 0, top = 0, bottom = 0 }

        initialMap =
            MapViewer.init
                { lng = 0, lat = 0 }
                (ZoomLevel.fromLogZoom 12)
                1
                contentArea

        newContext =
            { fieldOfView = Angle.degrees 45
            , orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 12
            , defaultZoomLevel = 12
            , focalPoint = treeNode |> leafFromIndex current |> startPoint
            , waitingForClickDelay = False
            , followSelectedPoint = True
            , map = initialMap
            , showMap = False
            , fingerPainting = False
            }

        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene newContext contentArea track
    in
    { newContext | map = MapViewer.withViewBounds noPadding lngLat1 lngLat2 newContext.map }


stopProp =
    { stopPropagation = True, preventDefault = False }


viewMenu : SystemSettings -> (Msg -> msg) -> PlanContext -> Element msg
viewMenu settings msgWrapper context =
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
        , Input.button []
            { onPress = Just <| msgWrapper ToggleFollowOrange
            , label =
                if context.followSelectedPoint then
                    useIcon FeatherIcons.lock

                else
                    useIcon FeatherIcons.unlock
            }
        , Input.button
            [ ToolTip.tooltip
                onLeft
                (ToolTip.myTooltip <|
                    if context.showMap then
                        "Hide map"

                    else
                        "Show map"
                )
            ]
            { onPress = Just <| msgWrapper ToggleShowMap
            , label =
                if context.showMap then
                    useIcon FeatherIcons.square

                else
                    useIcon FeatherIcons.map
            }
        , Input.button
            [ ToolTip.tooltip
                onLeft
                (ToolTip.myTooltip <|
                    if context.fingerPainting then
                        "Mouse moves view mode"

                    else
                        "Fingerpainting mode"
                )
            ]
            { onPress = Just <| msgWrapper ToggleFingerpainting
            , label =
                if context.fingerPainting then
                    useIcon FeatherIcons.move

                else
                    useIcon FeatherIcons.zap
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
    PlanContext
    -> MapViewer.MapData
    -> SystemSettings
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context mapData settings display contentArea track scene msgWrapper =
    let
        dragging =
            context.dragAction

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        plan3dView =
            el
                [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
                , if dragging /= DragNone then
                    htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

                  else
                    pointer
                , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
                , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
                , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> msgWrapper)
                , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
                , onContextMenu (msgWrapper ImageNoOp)
                , width fill
                , height fill
                , pointer
                , Border.width 0
                , Border.color FlatColors.ChinesePalette.peace
                , inFront <| placesOverlay display contentArea track camera
                , inFront <| fingerPainting context contentArea track camera
                , inFront <| viewMenu settings msgWrapper context
                , if context.fingerPainting then
                    Cursor.pointer

                  else
                    Cursor.default
                ]
            <|
                html <|
                    Scene3d.sunny
                        { camera = camera
                        , dimensions = contentArea
                        , background =
                            if context.showMap then
                                Scene3d.transparentBackground

                            else
                                Scene3d.backgroundColor <| colorFromElmUiColour FlatColors.IndianPalette.keppel
                        , clipDepth = Length.meters 1
                        , entities = scene
                        , upDirection = positiveZ
                        , sunlightDirection = negativeZ
                        , shadows = False
                        }

        mapUnderlay =
            Html.map (msgWrapper << MapMsg) <|
                MapViewer.view
                    Nothing
                    mapData
                    context.map
    in
    el
        [ behindContent <|
            if context.showMap then
                el
                    [ inFront <|
                        el [ alignLeft, alignBottom ] <|
                            html MapViewer.attribution
                    ]
                <|
                    html mapUnderlay

            else
                none
        ]
        plan3dView


fingerPainting :
    PlanContext
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> Camera3d Meters LocalCoords
    -> Element msg
fingerPainting context ( givenWidth, givenHeight ) track camera =
    --TODO: Use this to show the location of any fingerpainting tool.
    let
        ( svgWidth, svgHeight ) =
            ( String.fromInt <| Pixels.inPixels givenWidth
            , String.fromInt <| Pixels.inPixels givenHeight
            )

        screenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy
                    (Quantity.toFloatQuantity givenWidth)
                    (Quantity.toFloatQuantity givenHeight)
                )

        topLeftFrame =
            Frame2d.atPoint
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity givenHeight))

        --|> Frame2d.reverseY
    in
    case context.dragAction of
        DragPaint paintInfo ->
            let
                paintNodes =
                    paintInfo.path
                        |> List.map
                            (\place ->
                                Svg.circle2d
                                    [ Svg.Attributes.stroke "white"
                                    , Svg.Attributes.strokeWidth "1"
                                    , Svg.Attributes.fill "white"
                                    ]
                                    (Circle2d.withRadius (Pixels.float 5) place)
                            )
            in
            html <|
                Svg.svg
                    [ Svg.Attributes.width svgWidth
                    , Svg.Attributes.height svgHeight
                    ]
                    paintNodes

        --[ Svg.relativeTo topLeftFrame paintNodes ]
        DragPush pushInfo ->
            none

        _ ->
            none


resizeOccured : ( Quantity Int Pixels, Quantity Int Pixels ) -> PlanContext -> PlanContext
resizeOccured paneArea context =
    { context | map = MapViewer.resizeCanvas 1.0 paneArea context.map }


deriveCamera : GPXSource -> PeteTree -> PlanContext -> Int -> Camera3d Meters LocalCoords
deriveCamera refPoint treeNode context currentPosition =
    let
        latitude =
            effectiveLatitude <| leafFromIndex currentPosition treeNode

        lookingAt =
            if context.followSelectedPoint then
                startPoint <| leafFromIndex currentPosition treeNode

            else
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


mapBoundsFromScene :
    PlanContext
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> ( LngLat.LngLat, LngLat.LngLat )
mapBoundsFromScene updatedContext ( width, height ) track =
    -- Call this after updating context after any update changing the view/
    let
        ( wFloat, hFloat ) =
            ( toFloatQuantity width, toFloatQuantity height )

        oopsLngLat =
            { lng = 0, lat = 0 }

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            deriveCamera track.referenceLonLat track.trackTree updatedContext track.currentPosition

        ( rayOrigin, rayMax ) =
            ( Camera3d.ray camera screenRectangle Point2d.origin
            , Camera3d.ray camera screenRectangle (Point2d.xy wFloat hFloat)
            )

        ( topLeftModel, bottomRightModel ) =
            ( rayOrigin |> Axis3d.intersectionWithPlane Plane3d.xy
            , rayMax |> Axis3d.intersectionWithPlane Plane3d.xy
            )

        lngLatFromXY : Point3d.Point3d Meters LocalCoords -> LngLat.LngLat
        lngLatFromXY point =
            let
                gps : GPXSource
                gps =
                    DomainModel.gpxFromPointWithReference track.referenceLonLat <| DomainModel.withoutTime point
            in
            { lng = gps.longitude |> Direction2d.toAngle |> Angle.inDegrees
            , lat = gps.latitude |> Angle.inDegrees
            }
    in
    case ( topLeftModel, bottomRightModel ) of
        ( Just topLeft, Just bottomRight ) ->
            ( lngLatFromXY topLeft
            , lngLatFromXY bottomRight
            )

        _ ->
            -- We hope never to see this.
            ( oopsLngLat, oopsLngLat )


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PlanContext
    -> MapViewer.MapData
    -> ( PlanContext, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) context mapData =
    let
        -- Let us have some information about the view, making dragging more precise.
        ( wFloat, hFloat ) =
            ( toFloatQuantity width, toFloatQuantity height )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        ( rayOrigin, rayMax ) =
            ( Camera3d.ray camera screenRectangle Point2d.origin
            , Camera3d.ray camera screenRectangle (Point2d.xy wFloat hFloat)
            )

        ( topLeftModel, bottomRightModel ) =
            ( rayOrigin |> Axis3d.intersectionWithPlane Plane3d.xy
            , rayMax |> Axis3d.intersectionWithPlane Plane3d.xy
            )

        metersPerPixel =
            case ( topLeftModel, bottomRightModel ) of
                ( Just topLeft, Just bottomRight ) ->
                    (Length.inMeters <| Vector3d.xComponent <| Vector3d.from topLeft bottomRight)
                        / Pixels.toFloat wFloat

                _ ->
                    -- We hope never to see this.
                    1

        updatedMap ctxt =
            let
                ( lngLat1, lngLat2 ) =
                    mapBoundsFromScene ctxt ( width, height ) track
            in
            MapViewer.withViewBounds UtilsForViews.noPadding lngLat1 lngLat2 ctxt.map
    in
    -- Second return value indicates whether selection needs to change.
    case msg of
        MapMsg mapMsg ->
            let
                { newModel, newMapData, outMsg, cmd } =
                    MapViewer.update
                        (MapViewer.mapboxAccessToken MapboxKey.mapboxKey)
                        mapData
                        mapMsg
                        context.map
            in
            ( { context | map = newModel }
            , [ ExternalCommand <| Cmd.map (msgWrapper << MapMsg) cmd ]
            , newMapData
            )

        ImageGrab event ->
            {-
               Plan view only has pan, no rotation. Obviously no tilt.
               For fingerpainting, we do a click detect to see whether to paint or push track.
               Painting if within one meter of track.
               Apologies for near duplication of detectHit here.
            -}
            let
                ( x, y ) =
                    event.offsetPos

                screenPoint =
                    Point2d.pixels x y

                ray =
                    Camera3d.ray camera screenRectangle screenPoint

                touchPointInWorld =
                    ray
                        |> Axis3d.intersectionWithPlane Plane3d.xy

                nearestLeafIndex =
                    nearestToRay
                        ray
                        track.trackTree
                        track.leafIndex
                        track.currentPosition

                nearestLeaf =
                    asRecord <|
                        DomainModel.leafFromIndex nearestLeafIndex track.trackTree

                leafLineSegment =
                    LineSegment3d.from nearestLeaf.startPoint.space nearestLeaf.endPoint.space
                        |> LineSegment3d.projectInto SketchPlane3d.xy

                leafLineAxis =
                    Axis2d.throughPoints
                        (LineSegment2d.startPoint leafLineSegment)
                        (LineSegment2d.endPoint leafLineSegment)

                newState =
                    case ( context.fingerPainting, touchPointInWorld, leafLineAxis ) of
                        ( True, Just touchPoint, Just leafAxis ) ->
                            let
                                touchPointXY =
                                    touchPoint |> Point3d.projectInto SketchPlane3d.xy

                                touchPointOnTrack =
                                    touchPointXY |> Point2d.projectOnto leafAxis
                            in
                            -- Still need to determine Paint or Push.
                            if
                                Point2d.distanceFrom touchPointXY touchPointOnTrack
                                    |> Quantity.lessThanOrEqualTo (Length.meters 2)
                            then
                                DragPaint <| ViewPlanContext.PaintInfo [ screenPoint ]

                            else
                                DragPush <| ViewPlanContext.PushInfo

                        --touchPointXY
                        _ ->
                            DragPan
            in
            ( { context
                | orbiting = Just event.offsetPos
                , dragAction = newState
                , waitingForClickDelay = True
              }
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            , mapData
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }
            , []
            , mapData
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
                                ((startX - dx) * metersPerPixel)
                                ((dy - startY) * metersPerPixel)
                                0.0

                        newFocus =
                            context.focalPoint
                                |> .space
                                |> Point3d.translateBy shiftVector
                                |> DomainModel.withoutTime

                        newContext =
                            { context
                                | focalPoint = newFocus
                                , orbiting = Just ( dx, dy )
                            }
                    in
                    ( { newContext | map = updatedMap newContext }
                    , []
                    , mapData
                    )

                ( DragPaint paintInfo, _ ) ->
                    let
                        screenPoint =
                            --Debug.log "point" <|
                            Point2d.pixels dx dy

                        path =
                            screenPoint
                                :: paintInfo.path

                        newContext =
                            { context
                                | dragAction =
                                    DragPaint <|
                                        ViewPlanContext.PaintInfo path
                            }
                    in
                    ( newContext
                    , []
                    , mapData
                    )

                _ ->
                    ( context
                    , []
                    , mapData
                    )

        ImageRelease _ ->
            let
                actions =
                    case context.dragAction of
                        DragPaint paintInfo ->
                            -- One of those occasions where I'm pleased I have Actions, avoiding much plumbing.
                            [ Actions.WithUndo <| Actions.FingerPaint paintInfo
                            , Actions.FingerPaint paintInfo
                            , Actions.TrackHasChanged
                            ]

                        _ ->
                            []
            in
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , actions
            , mapData
            )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                newZoom =
                    clamp 0.0 22.0 <| context.zoomLevel + increment

                newContext =
                    { context
                        | zoomLevel = newZoom
                    }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track ( width, height ) context
                  , TrackHasChanged
                  ]
                , mapData
                )

            else
                ( context
                , []
                , mapData
                )

        ImageDoubleClick event ->
            let
                nearestPoint =
                    detectHit event track ( width, height ) context
            in
            ( { context
                | focalPoint = earthPointFromIndex nearestPoint track.trackTree
              }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            , mapData
            )

        ImageZoomIn ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ImageZoomOut ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ImageReset ->
            let
                newContext =
                    { context | zoomLevel = context.defaultZoomLevel }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ToggleFollowOrange ->
            let
                newContext =
                    { context
                        | followSelectedPoint = not context.followSelectedPoint
                        , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
                    }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ToggleShowMap ->
            ( { context | showMap = not context.showMap }
            , []
            , mapData
            )

        ToggleFingerpainting ->
            ( { context | fingerPainting = not context.fingerPainting }
            , []
            , mapData
            )

        _ ->
            ( context
            , []
            , mapData
            )


trackChanged :
    TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PlanContext
    -> PlanContext
trackChanged newTrack ( width, height ) context =
    -- Only interest is Orange pointer move.
    let
        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene context ( width, height ) newTrack

        noPadding =
            { left = 0, right = 0, top = 0, bottom = 0 }
    in
    { context
        | map = MapViewer.withViewBounds noPadding lngLat1 lngLat2 context.map
    }


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PlanContext
    -> Int
detectHit event track ( w, h ) context =
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
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    nearestToRay
        ray
        track.trackTree
        track.leafIndex
        track.currentPosition
