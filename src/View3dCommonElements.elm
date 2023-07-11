module View3dCommonElements exposing
    ( Context
    , Msg(..)
    , common3dSceneAttributes
    , lngLatFromXY
    , mapBoundsFromScene
    , mapPositionFromTrack
    , onViewControls
    , placesOverlay
    , pointLeafProximity
    , scaleToMapWorld
    , update
    )

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis2d
import Axis3d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Circle2d
import CommonToolStyles
import Dict
import Direction2d exposing (Direction2d)
import DomainModel exposing (EarthPoint, GPXSource, asRecord, earthPointFromIndex, leafFromIndex, nearestPointToRay, withoutTime)
import Drag3dCommonStructures exposing (DragAction(..), PointLeafProximity, ScreenCoords)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Cursor as Cursor
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Frame2d
import Geometry.Svg as Svg
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LineSegment3d
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import MapboxKey
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d
import Svg
import Svg.Attributes
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.DisplaySettingsOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews
import Vector3d
import ViewMode exposing (ViewMode(..))
import ViewPureStyles exposing (stopProp, useIcon)
import Viewpoint3d


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


type alias Context =
    --TODO: Use the same structure (and, largely, functions) for all 3d views.
    { cameraAzimuth : Direction2d LocalCoords --Camera relative to plane normal at focus point
    , cameraElevation : Angle -- Above local horizon plane
    , cameraDistance : Quantity Float Length.Meters
    , fieldOfView : Angle
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , map : MapViewer.Model
    , showMap : Bool
    , fingerPainting : Bool
    , viewMode : ViewMode
    }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> MapViewer.MapData
    -> Context
    -> (Context -> MapViewer.Model)
    -> Camera3d Meters LocalCoords
    -> Maybe String
    -> ( Context, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) mapData context mapUpdater camera paintTool =
    --Anything NOT handled by ViewPlan or ViewThird drops through to here.
    --(Which is now everything except ImageReset!)
    let
        -- Let us have some information about the view, making dragging more precise.
        ( wFloat, hFloat ) =
            ( toFloatQuantity width, toFloatQuantity height )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)
    in
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

        ImageZoomIn ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            in
            ( { newContext | map = mapUpdater newContext }, [], mapData )

        ImageZoomOut ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            in
            ( { newContext | map = mapUpdater newContext }, [], mapData )

        ImageNoOp ->
            ( context, [], mapData )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track ( width, height ) camera
                  , TrackHasChanged
                  ]
                , mapData
                )

            else
                ( context, [], mapData )

        ImageDoubleClick event ->
            let
                nearestPoint =
                    detectHit event track ( width, height ) camera

                newContext =
                    { context | focalPoint = earthPointFromIndex nearestPoint track.trackTree }
            in
            ( { newContext | map = mapUpdater newContext }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            , mapData
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [], mapData )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }
            in
            ( { newContext | map = mapUpdater newContext }, [], mapData )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
              }
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

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                alternate =
                    context.viewMode
                        == ViewThird
                        && (event.keys.ctrl || event.button == SecondButton)

                ( x, y ) =
                    event.offsetPos

                screenPoint =
                    Point2d.fromTuple Pixels.pixels event.offsetPos

                dragging =
                    if alternate then
                        DragRotate x y

                    else
                        DragPan x y

                newState =
                    case ( paintTool, pointLeafProximity camera track screenRectangle screenPoint ) of
                        ( Just tool, Just proximity ) ->
                            -- See if we can apply the named tool.
                            if proximity.distanceFrom |> Quantity.lessThanOrEqualTo (Length.meters 2) then
                                DragTool tool proximity proximity

                            else
                                dragging

                        ( Nothing, Just proximity ) ->
                            -- May be manually drawing, not applying another tool.
                            if
                                context.fingerPainting
                                    && (proximity.distanceFrom |> Quantity.lessThanOrEqualTo (Length.meters 2))
                            then
                                DragPaint <| Drag3dCommonStructures.PaintInfo [ proximity ]

                            else
                                dragging

                        _ ->
                            dragging
            in
            ( { context
                | dragAction = newState
                , waitingForClickDelay = True
              }
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            , mapData
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case context.dragAction of
                DragRotate startX startY ->
                    -- Change the camera azimuth and elevation
                    let
                        newAzimuth =
                            Angle.degrees <|
                                (Angle.inDegrees <| Direction2d.toAngle context.cameraAzimuth)
                                    - (dx - startX)

                        newElevation =
                            Angle.degrees <|
                                Angle.inDegrees context.cameraElevation
                                    + (dy - startY)

                        newContext =
                            { context
                                | cameraAzimuth = Direction2d.fromAngle newAzimuth
                                , cameraElevation = newElevation
                                , dragAction = DragRotate dx dy
                            }
                    in
                    ( { newContext | map = mapUpdater newContext }
                    , []
                    , mapData
                    )

                DragPan startX startY ->
                    --TODO: It would be slightly cleaner to work out the viewPlan once at grab time
                    --TODO: and use that until released. But it's a small optimisation.
                    let
                        viewPlane =
                            SketchPlane3d.withNormalDirection
                                (Viewpoint3d.viewDirection <| Camera3d.viewpoint camera)
                                context.focalPoint.space

                        grabPointOnScreen =
                            Point2d.pixels startX startY

                        movePointOnScreen =
                            Point2d.pixels dx dy

                        grabPointInModel =
                            Camera3d.ray camera screenRectangle grabPointOnScreen
                                |> Axis3d.intersectionWithPlane (SketchPlane3d.toPlane viewPlane)

                        movePointInModel =
                            Camera3d.ray camera screenRectangle movePointOnScreen
                                |> Axis3d.intersectionWithPlane (SketchPlane3d.toPlane viewPlane)

                        newContext =
                            case ( grabPointInModel, movePointInModel ) of
                                ( Just pick, Just drop ) ->
                                    let
                                        shift =
                                            Vector3d.from drop pick
                                                |> Vector3d.projectInto viewPlane

                                        newFocus =
                                            Point2d.origin
                                                |> Point2d.translateBy shift
                                                |> Point3d.on viewPlane
                                    in
                                    { context
                                        | focalPoint = withoutTime newFocus
                                        , dragAction = DragPan dx dy
                                    }

                                _ ->
                                    context
                    in
                    ( { newContext | map = mapUpdater newContext }
                    , []
                    , mapData
                    )

                DragPaint paintInfo ->
                    let
                        screenPoint =
                            Point2d.pixels dx dy

                        path =
                            case pointLeafProximity camera track screenRectangle screenPoint of
                                Just proximity ->
                                    proximity :: paintInfo.path

                                Nothing ->
                                    paintInfo.path
                    in
                    ( { context | dragAction = DragPaint <| Drag3dCommonStructures.PaintInfo path }
                    , []
                    , mapData
                    )

                DragTool tool startPaintInfo endPaintInfo ->
                    --Some playing around with how this should actually work to allow "cancel" effect of
                    --being off-track.
                    let
                        screenPoint =
                            Point2d.pixels dx dy

                        newEnd =
                            case pointLeafProximity camera track screenRectangle screenPoint of
                                Just proximity ->
                                    proximity

                                Nothing ->
                                    endPaintInfo
                    in
                    ( { context | dragAction = DragTool tool startPaintInfo newEnd }
                    , []
                    , mapData
                    )

                _ ->
                    ( context, [], mapData )

        ImageRelease _ ->
            let
                actions =
                    case context.dragAction of
                        DragPaint paintInfo ->
                            -- One of those occasions where I'm pleased I have Actions, avoiding much plumbing.
                            if List.length paintInfo.path > 2 then
                                [ Actions.WithUndo <| Actions.FingerPaint paintInfo
                                , Actions.FingerPaint paintInfo
                                , Actions.TrackHasChanged
                                ]

                            else
                                []

                        _ ->
                            []
            in
            ( { context | dragAction = DragNone }
            , actions
            , mapData
            )

        _ ->
            ( context, [], mapData )


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Camera3d Meters LocalCoords
    -> Int
detectHit event track ( w, h ) camera =
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

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    nearestPointToRay
        ray
        track.trackTree
        track.leafIndex
        track.currentPosition


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


common3dSceneAttributes msgWrapper context =
    (if context.viewMode == ViewThird then
        [ htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY)) ]

     else
        []
    )
        ++ [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
           , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
           , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
           , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> msgWrapper)
           , onContextMenu (msgWrapper ImageNoOp)
           , width fill
           , height fill
           , pointer
           , Border.width 0
           , Border.color FlatColors.ChinesePalette.peace
           , if context.followSelectedPoint then
                Cursor.default

             else
                Cursor.pointer
           ]


onViewControls : SystemSettings -> (Msg -> msg) -> Context -> Element msg
onViewControls settings msgWrapper context =
    let
        zoomIn =
            Input.button []
                { onPress = Just <| msgWrapper ImageZoomIn
                , label = useIcon FeatherIcons.plus
                }

        zoomOut =
            Input.button []
                { onPress = Just <| msgWrapper ImageZoomOut
                , label = useIcon FeatherIcons.minus
                }

        resetView =
            Input.button []
                { onPress = Just <| msgWrapper ImageReset
                , label = useIcon FeatherIcons.maximize
                }

        toggleFollowOrange =
            Input.button
                (if context.followSelectedPoint then
                    [ tooltip onLeft (localisedTooltip settings.location "panes" "locked") ]

                 else
                    [ tooltip onLeft (localisedTooltip settings.location "panes" "unlocked") ]
                )
                { onPress = Just <| msgWrapper ToggleFollowOrange
                , label =
                    if context.followSelectedPoint then
                        useIcon FeatherIcons.lock

                    else
                        useIcon FeatherIcons.unlock
                }

        toggleShowMap =
            Input.button
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

        toggleFreehandMode =
            Input.button
                [ ToolTip.tooltip
                    onLeft
                    (ToolTip.myTooltip <|
                        if context.fingerPainting then
                            "Leave Freehand mode"

                        else
                            "Start Freehand mode"
                    )
                ]
                { onPress = Just <| msgWrapper ToggleFingerpainting
                , label =
                    if context.fingerPainting then
                        useIcon FeatherIcons.move

                    else
                        useIcon FeatherIcons.penTool
                }
    in
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 10
        , Font.size 40
        , padding 6
        , spacing 8
        , Border.width 1
        , Border.rounded 4
        , Border.color FlatColors.AussiePalette.blurple
        , Background.color (CommonToolStyles.themeBackground settings.colourTheme)
        , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp >> msgWrapper)
        ]
    <|
        if context.viewMode == ViewFirst then
            [ toggleShowMap ]
            --else if context.viewMode == ViewThird then
            --    [ zoomIn
            --    , zoomOut
            --    , resetView
            --    , toggleFollowOrange
            --    , toggleShowMap
            --    ]

        else
            [ zoomIn
            , zoomOut
            , resetView
            , toggleFollowOrange
            , toggleShowMap
            , toggleFreehandMode
            ]


scaleToMapWorld : TrackLoaded msg -> Quantity Float Meters -> Quantity Float Quantity.Unitless
scaleToMapWorld track length =
    let
        aLeaf =
            DomainModel.asRecord <|
                DomainModel.leafFromIndex track.currentPosition track.trackTree

        ( worldStart, worldEnd ) =
            Tuple.mapBoth UtilsForViews.mapWorldFromGps UtilsForViews.mapWorldFromGps aLeaf.sourceData

        leafLengthInWorld : Quantity Float Quantity.Unitless
        leafLengthInWorld =
            Point2d.distanceFrom worldStart worldEnd
    in
    length |> Quantity.at_ (Quantity.per leafLengthInWorld aLeaf.trueLength)


mapPositionFromTrack : EarthPoint -> TrackLoaded msg -> Point3d.Point3d Quantity.Unitless MapViewer.WorldCoordinates
mapPositionFromTrack point track =
    let
        groundHeight =
            DomainModel.boundingBox track.trackTree
                |> BoundingBox3d.minZ

        {-
           I want a consistent conversion of meters to World Coordinates across the globe.
           I'll do that by numerically differentiating the Web Mercator formula.
           If it works, I'll do the proper maths.
           For now, I'll just take the nearby leaf and use those two points.
        -}
        lookingAtHeight =
            Point3d.zCoordinate point.space
                |> Quantity.minus groundHeight
                |> scaleToMapWorld track

        withHeight h pt =
            let
                { x, y } =
                    Point2d.toUnitless pt
            in
            Point3d.fromUnitless { x = x, y = y, z = Quantity.toFloat h }

        mapPosition =
            Point3d.mirrorAcross Plane3d.xy <|
                withHeight lookingAtHeight <|
                    UtilsForViews.mapWorldFromGps <|
                        DomainModel.gpxFromPointWithReference track.referenceLonLat <|
                            point
    in
    mapPosition


placesOverlay :
    Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> Camera3d Meters LocalCoords
    -> Element msg
placesOverlay display ( givenWidth, givenHeight ) track camera =
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

        nodes2d =
            track.landUseData.places
                |> Dict.filter
                    (\_ place ->
                        place.space
                            |> Point3d.depth camera
                            |> Quantity.greaterThanZero
                    )
                |> Dict.map
                    (\_ place ->
                        place.space |> Point3d.toScreenSpace camera screenRectangle
                    )
                |> Dict.filter
                    (\_ screenPoint ->
                        Rectangle2d.contains screenPoint screenRectangle
                    )

        textAttributes atPoint =
            [ Svg.Attributes.fill "white"
            , Svg.Attributes.fontFamily "sans-serif"
            , Svg.Attributes.fontSize "12px"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate atPoint) + 10))
            , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate atPoint)))
            ]

        -- Create an SVG label at each place
        placeNames =
            nodes2d
                |> Dict.map
                    (\name place ->
                        Svg.g []
                            [ Svg.circle2d
                                [ Svg.Attributes.stroke "white"
                                , Svg.Attributes.strokeWidth "1"
                                , Svg.Attributes.fill "none"
                                ]
                                (Circle2d.withRadius (Pixels.float 3) place)
                            , Svg.text_
                                (textAttributes place)
                                [ Svg.text name ]
                                -- Hack: flip the text upside down since our later
                                -- 'Svg.relativeTo topLeftFrame' call will flip it
                                -- back right side up
                                |> Svg.mirrorAcross (Axis2d.through place Direction2d.x)
                            ]
                    )
                |> Dict.values
                |> Svg.g []
    in
    if
        Dict.isEmpty track.landUseData.places
            || not display.placeNames
    then
        none

    else
        let
            topLeftFrame =
                Frame2d.atPoint
                    (Point2d.xy Quantity.zero (Quantity.toFloatQuantity givenHeight))
                    |> Frame2d.reverseY
        in
        html <|
            Svg.svg
                [ Svg.Attributes.width svgWidth
                , Svg.Attributes.height svgHeight
                ]
                [ Svg.relativeTo topLeftFrame placeNames ]


pointLeafProximity :
    Camera3d Meters LocalCoords
    -> TrackLoaded msg
    -> Rectangle2d Pixels ScreenCoords
    -> Point2d Pixels ScreenCoords
    -> Maybe PointLeafProximity
pointLeafProximity camera track screenRectangle screenPoint =
    let
        ray =
            Camera3d.ray camera screenRectangle screenPoint

        nearestPointIndex =
            nearestPointToRay ray track.trackTree track.leafIndex track.currentPosition

        sharedPoint =
            earthPointFromIndex nearestPointIndex track.trackTree

        projectionPlane =
            -- Will use this to measure separation between leaf axes and the ray.
            Plane3d.through
                sharedPoint.space
                (Axis3d.direction ray)

        touchPointInWorld =
            Axis3d.intersectionWithPlane projectionPlane ray
                |> Maybe.map (Point3d.projectOnto projectionPlane)

        proximityFrom index =
            let
                {-
                   I find the closest pass between two axes (ray and leaf) by:
                   1. create a plane normal to the ray (origin on the ray, containing nearest point?)
                   2. project each leaf onto that plane
                   3. compare projected leafs:
                       a: is projection of origin to projected leaf within the [start,end]
                       b: which is closest (distanceFrom).
                   The Plan View falls out as a special case.
                -}
                leaf =
                    asRecord <| leafFromIndex index track.trackTree

                leafSegment =
                    LineSegment3d.from leaf.startPoint.space leaf.endPoint.space
                        |> LineSegment3d.projectOnto projectionPlane
            in
            case ( touchPointInWorld, LineSegment3d.axis leafSegment ) of
                ( Just touchPoint, Just leafAxis ) ->
                    let
                        proportion =
                            Quantity.ratio
                                (Point3d.signedDistanceAlong leafAxis touchPoint)
                                (LineSegment3d.length leafSegment)
                    in
                    Just
                        { leafIndex = index
                        , distanceAlong = Point3d.signedDistanceAlong leafAxis touchPoint
                        , distanceFrom = Point3d.distanceFromAxis leafAxis touchPoint
                        , proportionAlong = proportion
                        , screenPoint = screenPoint
                        , worldPoint = touchPoint
                        }

                _ ->
                    Nothing
    in
    -- So, is the click before or after the point?
    -- Used to check proportion along but now just take the first one.
    case
        ( proximityFrom <| nearestPointIndex - 1
        , proximityFrom nearestPointIndex
        )
    of
        ( Just before, _ ) ->
            Just before

        ( Nothing, Just after ) ->
            Just after

        _ ->
            -- Really bad luck, who cares?
            Nothing


mapBoundsFromScene :
    Camera3d Meters LocalCoords
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> ( LngLat.LngLat, LngLat.LngLat )
mapBoundsFromScene camera ( width, height ) track =
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

        ( rayOrigin, rayMax ) =
            ( Camera3d.ray camera screenRectangle Point2d.origin
            , Camera3d.ray camera screenRectangle (Point2d.xy wFloat hFloat)
            )

        ( topLeftModel, bottomRightModel ) =
            ( rayOrigin |> Axis3d.intersectionWithPlane Plane3d.xy
            , rayMax |> Axis3d.intersectionWithPlane Plane3d.xy
            )
    in
    case ( topLeftModel, bottomRightModel ) of
        ( Just topLeft, Just bottomRight ) ->
            ( lngLatFromXY track topLeft
            , lngLatFromXY track bottomRight
            )

        _ ->
            -- We hope never to see this.
            ( oopsLngLat, oopsLngLat )


lngLatFromXY : TrackLoaded msg -> Point3d.Point3d Meters LocalCoords -> LngLat.LngLat
lngLatFromXY track point =
    let
        gps : GPXSource
        gps =
            DomainModel.gpxFromPointWithReference track.referenceLonLat <| DomainModel.withoutTime point
    in
    { lng = gps.longitude |> Direction2d.toAngle |> Angle.inDegrees
    , lat = gps.latitude |> Angle.inDegrees
    }
