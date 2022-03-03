module ViewProfileCharts exposing (..)

import Actions exposing (ToolAction(..))
import Axis2d
import Axis3d
import BoundingBox3d
import Camera3d
import Circle2d
import Color exposing (lightOrange)
import ColourPalette exposing (gradientColourPastel)
import Dict exposing (Dict)
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Frame2d
import Geometry.Svg as Svg
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (Meters)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Polygon2d
import Polyline2d
import PreviewData exposing (PreviewData, PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Scene3d.Material as Material
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showDecimal6, showLongMeasure, showShortMeasure)
import Vector2d
import Vector3d
import View3dCommonElements exposing (Msg(..), common3dSceneAttributes)
import ViewPureStyles exposing (useIcon)
import Viewpoint3d


type ClickZone
    = ZoneAltitude
    | ZoneGradient


type DragAction
    = DragNone
    | DragPan


type alias Context =
    { dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float -- 0 = whole track, 1 = half, etc.
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
    , waitingForClickDelay : Bool
    , profileScene : List (Entity LocalCoords)
    , emphasis : Float
    , altitudeSvgPoints : List (Point3d Meters LocalCoords)
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
        , Element.spacing 8
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
        , Input.button [ Font.size 14, centerX ]
            { onPress = Just <| msgWrapper (SetEmphasis 8)
            , label = text "x8"
            }
        , Input.button [ Font.size 14, centerX ]
            { onPress = Just <| msgWrapper (SetEmphasis 4)
            , label = text "x4"
            }
        , Input.button [ Font.size 14, centerX ]
            { onPress = Just <| msgWrapper (SetEmphasis 2)
            , label = text "x2"
            }
        , Input.button [ Font.size 14, centerX ]
            { onPress = Just <| msgWrapper (SetEmphasis 1)
            , label = text "x1"
            }
        ]


splitProportion =
    -- Fraction of height for the altitude, remainder for gradient.
    0.5


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> Dict String PreviewData
    -> Element msg
view context ( givenWidth, givenHeight ) track msgWrapper previews =
    let
        ( altitudeWidth, altitudeHeight ) =
            ( givenWidth
            , givenHeight
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy 0.5
                |> Quantity.round
            )

        ( gradientWidth, gradientHeight ) =
            ( givenWidth
            , givenHeight
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy 0.5
                |> Quantity.round
            )

        ( svgWidth, svgHeight ) =
            ( String.fromInt <| Pixels.inPixels altitudeWidth
            , String.fromInt <| Pixels.inPixels altitudeHeight
            )

        altitudeChart =
            Svg.svg
                [ Svg.Attributes.width svgWidth
                , Svg.Attributes.height svgHeight
                ]
                [ Svg.relativeTo topLeftFrame <|
                    pointsAsAltitudePolyline "black" context.altitudeSvgPoints
                , Svg.relativeTo topLeftFrame <|
                    Svg.g []
                        (orangeAltitudeSvg :: orangeText ++ purpleSvg)
                , Svg.relativeTo topLeftFrame <|
                    Svg.g []
                        altitudePreviews
                ]

        gradientChart =
            Svg.svg
                [ Svg.Attributes.width svgWidth
                , Svg.Attributes.height svgHeight
                ]
                [ Svg.relativeTo topLeftFrame <|
                    pointsAsGradientPolyline context.altitudeSvgPoints
                , Svg.relativeTo topLeftFrame <|
                    Svg.g []
                        (orangeGradientSvg :: orangeText)
                ]

        altitudePreviews : List (Svg msg)
        altitudePreviews =
            Dict.foldl makePreview [] previews

        makePreview : String -> PreviewData -> List (Svg msg) -> List (Svg msg)
        makePreview k preview outputs =
            case preview.shape of
                PreviewProfile previewTree ->
                    makeProfilePreview preview.colour previewTree :: outputs

                _ ->
                    outputs

        makeProfilePreview : Color -> PeteTree -> Svg msg
        makeProfilePreview colour previewTree =
            let
                dummyContext : Context
                dummyContext =
                    renderProfileData
                        { track | trackTree = previewTree }
                        altitudeWidth
                        context
            in
            pointsAsAltitudePolyline "green" dummyContext.altitudeSvgPoints

        pointsAsAltitudePolyline : String -> List (Point3d Meters LocalCoords) -> Svg msg
        pointsAsAltitudePolyline colour points =
            let
                pointsInScreenSpace =
                    points |> List.map (Point3d.toScreenSpace altitudeCamera altitudeScreenRectangle)
            in
            Svg.polyline2d
                [ Svg.Attributes.stroke colour
                , Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinecap "round"
                , Svg.Attributes.strokeLinejoin "round"
                ]
                (Polyline2d.fromVertices pointsInScreenSpace)

        pointsAsGradientPolyline : List (Point3d Meters LocalCoords) -> Svg msg
        pointsAsGradientPolyline points =
            let
                pointsInScreenSpace =
                    points
                        |> List.map (Point3d.toScreenSpace gradientCamera gradientScreenRectangle)
            in
            Svg.polyline2d
                [ Svg.Attributes.stroke "black"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinecap "round"
                , Svg.Attributes.strokeLinejoin "round"
                ]
                (Polyline2d.fromVertices pointsInScreenSpace)

        altitudeScreenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy
                    (Quantity.toFloatQuantity altitudeWidth)
                    (Quantity.toFloatQuantity altitudeHeight)
                )

        gradientScreenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy
                    (Quantity.toFloatQuantity gradientWidth)
                    (Quantity.toFloatQuantity gradientHeight)
                )

        altitudeCamera =
            deriveAltitudeCamera
                track.trackTree
                context
                track.currentPosition
                ( altitudeWidth, altitudeHeight )

        gradientCamera =
            deriveGradientCamera
                track.trackTree
                context
                track.currentPosition
                ( altitudeWidth, altitudeHeight )

        orangeLeaf =
            asRecord <| DomainModel.leafFromIndex track.currentPosition track.trackTree

        orangeAltitude2d =
            pointInAltitudeView context track.currentPosition track.trackTree
                |> Point3d.toScreenSpace altitudeCamera altitudeScreenRectangle

        orangeAltitudeSvg =
            Svg.circle2d
                [ Svg.Attributes.stroke "orange"
                , Svg.Attributes.strokeWidth "4"
                , Svg.Attributes.fill "none"
                ]
                (Circle2d.withRadius (Pixels.float 10) orangeAltitude2d)

        orangeGradient2d =
            pointInGradientView context track.currentPosition track.trackTree
                |> Point3d.toScreenSpace gradientCamera gradientScreenRectangle

        orangeGradientSvg =
            Svg.circle2d
                [ Svg.Attributes.stroke "orange"
                , Svg.Attributes.strokeWidth "4"
                , Svg.Attributes.fill "none"
                ]
                (Circle2d.withRadius (Pixels.float 10) orangeGradient2d)

        purpleSvg =
            case track.markerPosition of
                Just purple ->
                    let
                        purple2d =
                            pointInAltitudeView context purple track.trackTree
                                |> Point3d.toScreenSpace altitudeCamera altitudeScreenRectangle
                    in
                    [ Svg.circle2d
                        [ Svg.Attributes.stroke "purple"
                        , Svg.Attributes.strokeWidth "4"
                        , Svg.Attributes.fill "none"
                        ]
                        (Circle2d.withRadius (Pixels.float 8) purple2d)
                    ]

                Nothing ->
                    []

        textLine lineNum content =
            Svg.text_
                [ Svg.Attributes.fill "black"
                , Svg.Attributes.fontFamily "Sans serif"
                , Svg.Attributes.fontSize "20px"
                , Svg.Attributes.stroke "none"
                , Svg.Attributes.x
                    (String.fromFloat
                        (Pixels.toFloat (Point2d.xCoordinate orangeAltitude2d) + 20)
                    )
                , Svg.Attributes.y
                    (String.fromFloat
                        (Pixels.toFloat (Point2d.yCoordinate orangeAltitude2d) - lineNum * 20)
                    )
                ]
                [ Svg.text content ]
                -- Hack: flip the text upside down since our later
                -- 'Svg.relativeTo topLeftFrame' call will flip it
                -- back right side up
                |> Svg.mirrorAcross (Axis2d.through orangeAltitude2d Direction2d.x)

        orangeText =
            [ textLine 1 <| (showDecimal2 orangeLeaf.gradientAtStart ++ "%")
            , textLine 2 <| showShortMeasure False <| Point3d.zCoordinate <| orangeLeaf.startPoint
            , textLine 3 <|
                showLongMeasure False <|
                    DomainModel.distanceFromIndex track.currentPosition track.trackTree
            ]

        topLeftFrame =
            Frame2d.atPoint
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity altitudeHeight))
                |> Frame2d.reverseY
    in
    column
        (pointer
            :: (inFront <| zoomButtons msgWrapper context)
            :: common3dSceneAttributes msgWrapper context
        )
        [ Element.html altitudeChart
        , Element.html gradientChart
        ]


deriveAltitudeCamera treeNode context currentPosition ( width, height ) =
    let
        trackLengthInView =
            trueLength treeNode |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

        metresPerPixel =
            Length.inMeters trackLengthInView / (toFloat <| Pixels.inPixels width)

        viewportHeight =
            Length.meters <| metresPerPixel * (toFloat <| Pixels.inPixels height)

        lookingAt =
            if context.followSelectedPoint then
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    (earthPointFromIndex currentPosition treeNode
                        |> Point3d.zCoordinate
                        |> Quantity.multiplyBy context.emphasis
                    )

            else
                context.focalPoint

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 -50000.0 0.0)
                lookingAt

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = lookingAt
                , eyePoint = eyePoint
                , upDirection = Direction3d.positiveZ
                }
    in
    Camera3d.orthographic
        { viewpoint = viewpoint
        , viewportHeight = viewportHeight
        }


deriveGradientCamera treeNode context currentPosition ( width, height ) =
    let
        trackLengthInView =
            trueLength treeNode |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

        metresPerPixel =
            Length.inMeters trackLengthInView / (toFloat <| Pixels.inPixels width)

        viewportHeight =
            Length.meters <| metresPerPixel * (toFloat <| Pixels.inPixels height)

        lookingAt =
            if context.followSelectedPoint then
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    (earthPointFromIndex currentPosition treeNode
                        |> Point3d.zCoordinate
                        |> Quantity.multiplyBy context.emphasis
                    )

            else
                context.focalPoint

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 1000.0)
                lookingAt

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = lookingAt
                , eyePoint = eyePoint
                , upDirection = Direction3d.positiveY
                }
    in
    Camera3d.orthographic
        { viewpoint = viewpoint
        , viewportHeight = viewportHeight
        }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Dict String PreviewData
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) previews context =
    let
        maxZoom =
            (logBase 2 <| toFloat <| skipCount track.trackTree) - 2
    in
    case msg of
        SetEmphasis emphasis ->
            ( { context | emphasis = toFloat emphasis }
                |> renderProfileData track givenWidth
            , []
            )

        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel + 0.5 }
                |> renderProfileData track givenWidth
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel - 0.5 }
                |> renderProfileData track givenWidth
            , []
            )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context)
                |> renderProfileData track givenWidth
            , []
            )

        ImageNoOp ->
            ( context, [] )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            --if context.waitingForClickDelay then
            ( context
            , [ SetCurrent <| detectHit event track ( givenWidth, givenHeight ) context
              , TrackHasChanged
              ]
            )

        --else
        --    ( context, [] )
        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [] )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                zoomLevel =
                    clamp 0 maxZoom <|
                        context.zoomLevel
                            + increment
            in
            ( { context | zoomLevel = zoomLevel }
                |> renderProfileData track givenWidth
            , []
            )

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                newContext =
                    { context
                        | orbiting = Just event.offsetPos
                        , dragAction = DragPan
                        , waitingForClickDelay = True
                    }
            in
            ( newContext
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
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
                    ( newContext |> renderProfileData track givenWidth
                    , []
                    )

                _ ->
                    ( context, [] )

        ImageRelease event ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
                , waitingForClickDelay = False
              }
            , []
            )

        ToggleFollowOrange ->
            let
                currentDistance =
                    distanceFromIndex track.currentPosition track.trackTree

                currentAltitude =
                    gpxPointFromIndex track.currentPosition track.trackTree
                        |> .altitude
            in
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint =
                    Point3d.xyz
                        currentDistance
                        Quantity.zero
                        currentAltitude
              }
            , []
            )

        ImageDoubleClick event ->
            ( context, [] )


pointInAltitudeView : Context -> Int -> PeteTree -> EarthPoint
pointInAltitudeView context i tree =
    let
        distance =
            DomainModel.distanceFromIndex i tree

        altitude =
            DomainModel.gpxPointFromIndex i tree |> .altitude
    in
    Point3d.xyz
        distance
        Quantity.zero
        (altitude |> Quantity.multiplyBy context.emphasis)


pointInGradientView : Context -> Int -> PeteTree -> EarthPoint
pointInGradientView context i tree =
    let
        distance =
            DomainModel.distanceFromIndex i tree

        gradient =
            DomainModel.leafFromIndex i tree
                |> DomainModel.gradientFromNode

        compensateForZoom g =
            -- Empirical!
            2.0 * g * (0.5 ^ context.zoomLevel) * (Length.inKilometers <| trueLength tree)
    in
    Point3d.xyz
        distance
        (Length.meters <| compensateForZoom gradient)
        Quantity.zero


renderProfileData :
    TrackLoaded msg
    -> Quantity Int Pixels
    -> Context
    -> Context
renderProfileData track displayWidth context =
    let
        --TODO: Consider moving this, oddly enough, into view.
        -- ALthough, looks like it's reusable as it stands, if we use a dummy context.
        currentPoint =
            earthPointFromIndex track.currentPosition track.trackTree

        fullRenderBox =
            BoundingBox3d.withDimensions
                ( Length.kilometer, Length.kilometer, Length.kilometer )
                currentPoint

        trackLengthInView =
            trueLength track.trackTree |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

        metresPerPixel =
            Length.inMeters trackLengthInView / (toFloat <| Pixels.inPixels displayWidth)

        compensateForZoom g =
            -- Empirical!
            2.0 * g * (0.5 ^ context.zoomLevel) * (Length.inKilometers <| trueLength track.trackTree)

        pointOfInterest =
            distanceFromIndex track.currentPosition track.trackTree

        floorPlane =
            Plane3d.xy |> Plane3d.offsetBy (BoundingBox3d.minZ <| boundingBox track.trackTree)

        leftEdge =
            Quantity.clamp
                Quantity.zero
                (trueLength track.trackTree |> Quantity.minus trackLengthInView)
                (pointOfInterest |> Quantity.minus (Quantity.half trackLengthInView))

        rightEdge =
            leftEdge |> Quantity.plus trackLengthInView

        ( leftIndex, rightIndex ) =
            -- Make sure we always have a spare point outside the image if possible.
            ( indexFromDistance leftEdge track.trackTree - 1
            , indexFromDistance rightEdge track.trackTree + 1
            )

        ( trueLeftEdge, trueRightEdge ) =
            ( distanceFromIndex leftIndex track.trackTree
            , distanceFromIndex rightIndex track.trackTree
            )

        depthFn : RoadSection -> Maybe Int
        depthFn road =
            --Depth to ensure about 1000 values returned,
            if road.boundingBox |> BoundingBox3d.intersects fullRenderBox then
                Nothing

            else
                Just <| round <| 10 + context.zoomLevel

        makeSvgPoint : Length.Length -> RoadSection -> List (Point3d Meters LocalCoords)
        makeSvgPoint distance road =
            --TODO: Use a fold over the tree to create a polyline and render it with
            -- slightly rounded joints.
            [ Point3d.xyz
                distance
                (Length.meters <| compensateForZoom road.gradientAtStart)
                (road.startPoint |> Point3d.zCoordinate |> Quantity.multiplyBy context.emphasis)
            ]

        foldFn :
            (Length.Length -> RoadSection -> List renderable)
            -> RoadSection
            -> ( Length.Length, List renderable, Maybe RoadSection )
            -> ( Length.Length, List renderable, Maybe RoadSection )
        foldFn renderFn road ( distanceSoFar, outputs, _ ) =
            let
                newEntry : List renderable
                newEntry =
                    renderFn distanceSoFar road
            in
            ( distanceSoFar |> Quantity.plus road.trueLength
            , newEntry ++ outputs
            , Just road
            )

        ( _, altitudeSvgPoints, _ ) =
            --TODO: Make function that takes tree and colour, returns SVG. Use it in view.
            DomainModel.traverseTreeBetweenLimitsToDepth
                leftIndex
                rightIndex
                depthFn
                0
                track.trackTree
                (foldFn makeSvgPoint)
                ( trueLeftEdge, [], Nothing )

        finalSvgPoint =
            let
                leaf =
                    asRecord <| leafFromIndex rightIndex track.trackTree
            in
            Point3d.xyz
                rightEdge
                (Length.meters <| compensateForZoom leaf.gradientAtStart)
                (leaf.endPoint |> Point3d.zCoordinate |> Quantity.multiplyBy context.emphasis)
    in
    { context
        | profileScene = []
        , metresPerPixel = metresPerPixel
        , altitudeSvgPoints = finalSvgPoint :: altitudeSvgPoints
    }


initialiseView :
    Int
    -> PeteTree
    -> Maybe Context
    -> Context
initialiseView current treeNode currentContext =
    let
        currentPoint =
            earthPointFromIndex current treeNode

        currentDistance =
            distanceFromIndex current treeNode
    in
    case currentContext of
        Just context ->
            { context
                | orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = 0.0
                , defaultZoomLevel = 0.0
                , focalPoint =
                    Point3d.xyz
                        currentDistance
                        Quantity.zero
                        (Point3d.zCoordinate currentPoint |> Quantity.multiplyBy context.emphasis)
                , metresPerPixel = 10.0
                , waitingForClickDelay = False
            }

        Nothing ->
            { orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 0.0
            , defaultZoomLevel = 0.0
            , focalPoint =
                Point3d.xyz
                    currentDistance
                    Quantity.zero
                    (currentPoint |> Point3d.zCoordinate)
            , followSelectedPoint = True
            , metresPerPixel = 10.0
            , waitingForClickDelay = False
            , profileScene = []
            , emphasis = 1.0
            , altitudeSvgPoints = []
            }


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
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
            deriveAltitudeCamera track.trackTree context track.currentPosition ( w, h )

        ray =
            Camera3d.ray camera screenRectangle screenPoint

        modelPoint =
            ray |> Axis3d.intersectionWithPlane Plane3d.zx
    in
    case modelPoint of
        Just found ->
            indexFromDistance (Point3d.xCoordinate found) track.trackTree

        Nothing ->
            track.currentPosition
