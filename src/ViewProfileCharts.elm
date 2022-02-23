module ViewProfileCharts exposing (..)

import Actions exposing (ToolAction(..))
import BoundingBox3d
import Camera3d
import Chart.Events as CE
import Color
import ColourPalette exposing (gradientColourPastel)
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
import FlatColors.FlatUIPalette
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Scene3d.Material as Material
import Spherical exposing (metresPerPixel)
import Tools.DisplaySettingsOptions exposing (CurtainStyle(..))
import TrackLoaded exposing (TrackLoaded)
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


splitProportion =
    -- Fraction of height for the altitude, remainder for gradient.
    0.5


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> Element msg
view context ( givenWidth, givenHeight ) track msgWrapper =
    let
        ( altitudeWidth, altitudeHeight ) =
            -- Subtract pixels we use for padding around the scene view.
            ( givenWidth
            , givenHeight
            )

        dragging =
            context.dragAction
    in
    el
        (pointer
            :: (inFront <| zoomButtons msgWrapper context)
            :: common3dSceneAttributes msgWrapper context
        )
    <|
        html <|
            Scene3d.unlit
                { camera = deriveCamera track.trackTree context track.currentPosition
                , dimensions = ( altitudeWidth, altitudeHeight )
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meter
                , entities = context.profileScene
                }


deriveCamera treeNode context currentPosition =
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
                (Vector3d.meters 0.0 -100.0 0.0)
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
        , viewportHeight = Length.meters 1000.0
        }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) context =
    let
        centre =
            BoundingBox3d.centerPoint <| boundingBox track.trackTree

        maxZoom =
            (logBase 2 <| toFloat <| skipCount track.trackTree) - 2
    in
    case msg of
        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel + 0.5 }
                |> renderProfileData track
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel - 0.5 }
                |> renderProfileData track
            , []
            )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context)
                |> renderProfileData track
            , []
            )

        ImageNoOp ->
            ( context, [] )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track ( givenWidth, givenHeight ) context
                  , TrackHasChanged
                  ]
                )

            else
                ( context, [] )

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
                |> renderProfileData track
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
                    ( newContext |> renderProfileData track
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

        ImageDoubleClick event ->
            ( context, [] )


profilePoint : Length.Length -> Float -> EarthPoint -> EarthPoint
profilePoint distance gradient point =
    --Cunningly, use Z for altitude and Y for gradient.
    Point3d.xyz
        distance
        (Length.meters gradient)
        (Point3d.zCoordinate point)


groundPoint : Length.Length -> Float -> EarthPoint -> EarthPoint
groundPoint distance gradient point =
    --Cunningly, use Z for altitude and Y for gradient.
    Point3d.xyz
        distance
        Quantity.zero
        Quantity.zero


renderProfileData : TrackLoaded msg -> Context -> Context
renderProfileData track context =
    let
        trackLengthInView =
            trueLength track.trackTree |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

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
            ( indexFromDistance leftEdge track.trackTree
            , indexFromDistance rightEdge track.trackTree
            )

        depthFn road =
            --TODO: this.
            --Depth to ensure about 1000 values returned,
            Nothing

        --Just <| round <| 10 + context.zoomLevel
        makeVisibleSegment : Length.Length -> RoadSection -> List (Entity LocalCoords)
        makeVisibleSegment distance road =
            let
                gradient =
                    DomainModel.gradientFromNode <| Leaf road

                roadAsSegment =
                    LineSegment3d.from
                        (profilePoint
                            distance
                            gradient
                            road.startPoint
                        )
                        (profilePoint
                            (distance |> Quantity.plus road.trueLength)
                            gradient
                            road.endPoint
                        )

                curtainHem =
                    -- Drop onto x-axis so visible from both angles
                    LineSegment3d.from
                        (groundPoint
                            distance
                            gradient
                            road.startPoint
                        )
                        (groundPoint
                            (distance |> Quantity.plus road.trueLength)
                            gradient
                            road.endPoint
                        )
            in
            [ Scene3d.point { radius = Pixels.pixels 1 }
                (Material.color Color.black)
                (LineSegment3d.startPoint roadAsSegment)
            , Scene3d.lineSegment (Material.color Color.black) <|
                roadAsSegment
            , Scene3d.quad (Material.color <| gradientColourPastel gradient)
                (LineSegment3d.startPoint roadAsSegment)
                (LineSegment3d.endPoint roadAsSegment)
                (LineSegment3d.endPoint curtainHem)
                (LineSegment3d.startPoint curtainHem)
            ]

        foldFn :
            RoadSection
            -> ( Length.Length, List (Entity LocalCoords), Maybe RoadSection )
            -> ( Length.Length, List (Entity LocalCoords), Maybe RoadSection )
        foldFn road ( distanceSoFar, outputs, _ ) =
            let
                newEntry : List (Entity LocalCoords)
                newEntry =
                    makeVisibleSegment distanceSoFar road
            in
            ( distanceSoFar |> Quantity.plus road.trueLength
            , newEntry ++ outputs
            , Just road
            )

        ( _, result, final ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                leftIndex
                rightIndex
                depthFn
                0
                track.trackTree
                foldFn
                ( leftEdge, [], Nothing )

        finalDatum =
            case final of
                Just finalLeaf ->
                    -- Make sure we include final point
                    []

                Nothing ->
                    -- Can't happen (FLW)
                    []
    in
    { context | profileScene = finalDatum ++ result }


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
                , focalPoint = profilePoint currentDistance 0.0 currentPoint
                , metresPerPixel = 10.0
                , waitingForClickDelay = False
            }

        Nothing ->
            { orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 0.0
            , defaultZoomLevel = 0.0
            , focalPoint = profilePoint currentDistance 0.0 currentPoint
            , followSelectedPoint = True
            , metresPerPixel = 10.0
            , waitingForClickDelay = False
            , profileScene = []
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
            deriveCamera track.trackTree context track.currentPosition

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    --TODO: Just intersect ray with XZ plane
    nearestToRay ray track.trackTree
