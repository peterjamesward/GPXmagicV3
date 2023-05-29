module ViewPlan exposing
    ( Msg(..)
    , initialiseView
    , subscriptions
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox2d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import CommonToolStyles
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette exposing (white)
import Html
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Illuminance
import Json.Decode as D
import Length exposing (Meters)
import LngLat
import LocalCoords exposing (LocalCoords)
import MapStyles
import MapViewer
import MapboxKey
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Scene3d.Light as Light
import Spherical exposing (metresPerPixel)
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (flatBox)
import Vector2d
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


subscriptions : MapViewer.MapData -> PlanContext -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


lngLatFromXYZ : TrackLoaded msg -> EarthPoint -> LngLat.LngLat
lngLatFromXYZ track point =
    let
        gps =
            DomainModel.gpxFromPointWithReference track.referenceLonLat point
    in
    { lng = gps.longitude |> Direction2d.toAngle |> Angle.inDegrees
    , lat = gps.latitude |> Angle.inDegrees
    }


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

        ( ne, sw ) =
            ( Point3d.xyz maxX maxY minZ
                |> DomainModel.withoutTime
                |> DomainModel.gpxFromPointWithReference track.referenceLonLat
            , Point3d.xyz minX minY minZ
                |> DomainModel.withoutTime
                |> DomainModel.gpxFromPointWithReference track.referenceLonLat
            )

        map =
            MapViewer.init
                { lng =
                    DomainModel.getFirstLeaf treeNode
                        |> .sourceData
                        |> Tuple.first
                        |> .longitude
                        |> Direction2d.toAngle
                        |> Angle.inDegrees
                , lat =
                    DomainModel.getFirstLeaf treeNode
                        |> .sourceData
                        |> Tuple.first
                        |> .latitude
                        |> Angle.inDegrees
                }
                (ZoomLevel.fromLogZoom 12)
                1
                contentArea
    in
    { fieldOfView = Angle.degrees 45
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = MapViewer.viewZoom map |> ZoomLevel.toLogZoom
    , defaultZoomLevel = MapViewer.viewZoom map |> ZoomLevel.toLogZoom
    , focalPoint = treeNode |> leafFromIndex current |> startPoint
    , waitingForClickDelay = False
    , followSelectedPoint = True
    , map = map
    }


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : SystemSettings -> (Msg -> msg) -> PlanContext -> Element msg
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
        , Input.button []
            { onPress = Just <| msgWrapper ToggleFollowOrange
            , label =
                if context.followSelectedPoint then
                    useIcon FeatherIcons.lock

                else
                    useIcon FeatherIcons.unlock
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

        overlay =
            placesOverlay display contentArea track camera

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
                , inFront <| overlay
                , inFront <| zoomButtons settings msgWrapper context
                ]
            <|
                html <|
                    Scene3d.sunny
                        { camera = camera
                        , dimensions = contentArea
                        , background = Scene3d.transparentBackground
                        , clipDepth = Length.meters 1
                        , entities = scene
                        , upDirection = positiveZ
                        , sunlightDirection = negativeZ
                        , shadows = False
                        }

        sun =
            Light.directional (Light.castsShadows False)
                { direction = Direction3d.negativeZ
                , intensity = Illuminance.lux 80000
                , chromaticity = Light.sunlight
                }

        sky =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.skylight
                , intensity = Illuminance.lux 20000
                }

        environment =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.daylight
                , intensity = Illuminance.lux 15000
                }

        lights =
            Scene3d.threeLights sun sky environment

        plan3dScene =
            Scene3d.toWebGLEntities
                { lights = lights
                , camera = camera
                , clipDepth = Length.meters 1
                , exposure = Scene3d.exposureValue 15
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.daylight
                , aspectRatio = 8 / 6
                , supersampling = 1.0
                , entities = scene
                }

        mapUnderlay =
            Html.map (msgWrapper << MapMsg) <|
                MapViewer.view
                    []
                    --plan3dScene
                    mapData
                    context.map
    in
    el [ behindContent <| html mapUnderlay ] plan3dView



--el [ inFront plan3dView ] mapUnderlay
--html mapUnderlay


deriveCamera : GPXSource -> PeteTree -> PlanContext -> Int -> Camera3d Meters LocalCoords
deriveCamera refPoint treeNode context currentPosition =
    let
        { x, y } =
            -- Center of map view in fractional "Mercator" units.
            MapViewer.viewPosition context.map
                |> Point2d.toUnitless

        longitude =
            (x - 0.5) |> Angle.turns |> Direction2d.fromAngle

        --_ =
        --    Debug.log "longitude" <|
        --        Angle.inDegrees <|
        --            Direction2d.toAngle longitude
        --
        --_ =
        --    Debug.log "latitude" <|
        --        Angle.inDegrees latitude
        latitude =
            effectiveLatitude <| leafFromIndex currentPosition treeNode

        newLatitude =
            -- Reverse Mercator gives latitude in radians, we hope.
            Angle.radians <| 2 * (atan <| e ^ (1 - (y / 2)) - pi / 4)

        {- Mercator encodiing of latitude in MapView is ...
           y =
               0.5 * (1 - (logBase e (tan (lngLat.lat * pi / 180) + 1 / cos (lngLat.lat * pi / 180)) / pi))

           - that's not an obvious inverse, and may be wrong.

           My version is ((1 - (logBase e (tan (halfLat + piBy4)) / pi)) / 2)

           Which inverts easily

           2 * (atan <| e ^ ( 1 - (y/2) ) - pi / 4)
        -}
        lookingAt =
            if context.followSelectedPoint then
                startPoint <| leafFromIndex currentPosition treeNode

            else
                context.focalPoint

        eyePoint =
            --TODO: Perhaps tilting by 'latitude' will compensate for Mercator distortion.
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
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> PlanContext
    -> MapViewer.MapData
    -> ( PlanContext, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) context mapData =
    let
        -- Let us have some information about the view, making dragging more sensible.
        screenPoint x y =
            Point2d.pixels x y

        ( wFloat, hFloat ) =
            ( toFloatQuantity width, toFloatQuantity height )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        rayOrigin =
            Camera3d.ray camera screenRectangle Point2d.origin

        rayMax =
            Camera3d.ray camera screenRectangle (Point2d.xy wFloat hFloat)

        topLeftModel =
            rayOrigin |> Axis3d.intersectionWithPlane Plane3d.xy

        bottomRightModel =
            rayMax |> Axis3d.intersectionWithPlane Plane3d.xy

        metersPerPixel =
            case ( topLeftModel, bottomRightModel ) of
                ( Just topLeft, Just bottomRight ) ->
                    (Length.inMeters <| Vector3d.xComponent <| Vector3d.from topLeft bottomRight)
                        / Pixels.toFloat wFloat

                _ ->
                    -- We hope never to see this.
                    1
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
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            ( { context
                | orbiting = Just event.offsetPos
                , dragAction = DragPan
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
                    in
                    ( { context
                        | focalPoint = newFocus
                        , orbiting = Just ( dx, dy )
                        , map =
                            MapViewer.withPositionAndZoom
                                (MapViewer.lngLatToWorld <| lngLatFromXYZ track newFocus)
                                (MapViewer.viewZoom context.map)
                                context.map
                      }
                    , []
                    , mapData
                    )

                _ ->
                    ( context
                    , []
                    , mapData
                    )

        ImageRelease _ ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , []
            , mapData
            )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                newZoom =
                    clamp 0.0 22.0 <| context.zoomLevel + increment
            in
            ( { context
                | zoomLevel = newZoom
                , map =
                    MapViewer.withPositionAndZoom
                        (MapViewer.viewPosition context.map)
                        (ZoomLevel.fromLogZoom newZoom)
                        context.map
              }
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
                newZoom =
                    clamp 0.0 22.0 <| context.zoomLevel + 0.5
            in
            ( { context
                | zoomLevel = newZoom
                , map =
                    MapViewer.withPositionAndZoom
                        (MapViewer.viewPosition context.map)
                        (ZoomLevel.fromLogZoom newZoom)
                        context.map
              }
            , []
            , mapData
            )

        ImageZoomOut ->
            let
                newZoom =
                    clamp 0.0 22.0 <| context.zoomLevel - 0.5
            in
            ( { context
                | zoomLevel = newZoom
                , map =
                    MapViewer.withPositionAndZoom
                        (MapViewer.viewPosition context.map)
                        (ZoomLevel.fromLogZoom newZoom)
                        context.map
              }
            , []
            , mapData
            )

        ImageReset ->
            ( { context
                | zoomLevel = context.defaultZoomLevel
              }
            , []
            , mapData
            )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
              }
            , []
            , mapData
            )

        _ ->
            ( context
            , []
            , mapData
            )


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
