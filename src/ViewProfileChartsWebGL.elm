module ViewProfileChartsWebGL exposing
    ( ClickZone(..)
    , initialiseView
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Axis2d
import Axis3d
import BoundingBox3d
import Camera3d
import Circle2d
import Color
import ColourPalette exposing (gradientColourPastel)
import CommonToolStyles
import Dict exposing (Dict)
import Direction2d
import Direction3d
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
import Html.Events.Extra.Mouse as Mouse exposing (onContextMenu)
import Html.Events.Extra.Wheel as Wheel
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d, xCoordinate, yCoordinate)
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Polygon2d
import Polyline2d
import PreviewData exposing (PreviewData, PreviewShape(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import SystemSettings exposing (SystemSettings)
import Tools.NamedSegmentOptions exposing (NamedSegment)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (colourHexString, showDecimal2, showLongMeasure, showShortMeasure, uiColourHexString)
import Vector3d
import ViewProfileChartContext exposing (DragAction(..), Msg(..), ProfileContext)
import ViewPureStyles exposing (useIcon)
import Viewpoint3d


type ClickZone
    = ZoneGradient


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : SystemSettings -> (Msg -> msg) -> ProfileContext -> Element msg
zoomButtons settings msgWrapper context =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Font.size 40
        , padding 6
        , Element.spacing 8
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

        --, Input.button []
        --    { onPress = Just <| msgWrapper ToggleFollowOrange
        --    , label =
        --        if context.followSelectedPoint then
        --            useIcon FeatherIcons.lock
        --
        --        else
        --            useIcon FeatherIcons.unlock
        --    }
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


deriveAltitudeCamera treeNode context currentPosition ( width, height ) =
    let
        trackLengthInView =
            trueLength treeNode |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

        metresPerPixel =
            Length.inMeters trackLengthInView / (toFloat <| Pixels.inPixels width)

        viewportHeight =
            Length.meters <| metresPerPixel * (toFloat <| Pixels.inPixels height)

        lookingAt =
            DomainModel.withoutTime <|
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    (earthPointFromIndex currentPosition treeNode
                        |> .space
                        |> Point3d.zCoordinate
                        |> Quantity.multiplyBy context.emphasis
                    )

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 -50000.0 0.0)
                lookingAt.space

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = lookingAt.space
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
            DomainModel.withoutTime <|
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    (earthPointFromIndex currentPosition treeNode
                        |> .space
                        |> Point3d.zCoordinate
                        |> Quantity.multiplyBy context.emphasis
                    )

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 1000.0)
                lookingAt.space

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = lookingAt.space
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
    -> ProfileContext
    -> ( ProfileContext, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) previews context =
    case msg of
        SetEmphasis emphasis ->
            ( { context | emphasis = toFloat emphasis }
            , []
            )

        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel + 0.5 }
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel - 0.5 }
            , []
            )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context)
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
                maxZoom =
                    (logBase 2 <| toFloat <| skipCount track.trackTree) - 2

                increment =
                    -0.001 * deltaY

                zoomLevel =
                    clamp 0 maxZoom <|
                        context.zoomLevel
                            + increment
            in
            ( { context | zoomLevel = zoomLevel }, [] )

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                newProfileContext =
                    { context
                        | orbiting = Just event.offsetPos
                        , waitingForClickDelay = True
                    }
            in
            ( newProfileContext
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            )

        ImageDrag event ->
            ( context, [] )

        ImageRelease _ ->
            ( { context
                | orbiting = Nothing
                , waitingForClickDelay = False
              }
            , []
            )

        ToggleFollowOrange ->
            ( context, [] )

        ImageDoubleClick _ ->
            ( context, [] )

        MouseMove event ->
            ( { context | mouseEvent = Just event }, [] )

        ToggleColours ->
            ( context, [] )


pointInAltitudeView : ProfileContext -> Int -> PeteTree -> EarthPoint
pointInAltitudeView context i tree =
    let
        distance =
            DomainModel.distanceFromIndex i tree

        altitude =
            DomainModel.gpxPointFromIndex i tree |> .altitude
    in
    DomainModel.withoutTime <|
        Point3d.xyz
            distance
            Quantity.zero
            (altitude |> Quantity.multiplyBy context.emphasis)


pointInGradientView : ProfileContext -> Int -> PeteTree -> EarthPoint
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
    DomainModel.withoutTime <|
        Point3d.xyz
            distance
            (Length.meters <| compensateForZoom gradient)
            Quantity.zero


view :
    ProfileContext
    -> SystemSettings
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List NamedSegment
    -> (Msg -> msg)
    -> Dict String PreviewData
    -> Element msg
view context settings ( givenWidth, givenHeight ) track segments msgWrapper previews =
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

        currentPoint =
            earthPointFromIndex track.currentPosition track.trackTree

        fullRenderBox =
            BoundingBox3d.withDimensions
                ( Length.kilometer, Length.kilometer, Length.kilometer )
                currentPoint.space

        trackLengthInView =
            trueLength track.trackTree |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

        compensateForZoom g =
            -- Empirical!
            2.0 * g * (0.5 ^ context.zoomLevel) * (Length.inKilometers <| trueLength track.trackTree)

        pointOfInterest =
            distanceFromIndex track.currentPosition track.trackTree

        leftEdge =
            Quantity.clamp
                Quantity.zero
                (trueLength track.trackTree |> Quantity.minus trackLengthInView)
                (pointOfInterest |> Quantity.minus (Quantity.half trackLengthInView))

        rightEdge =
            leftEdge |> Quantity.plus trackLengthInView

        depthFn : RoadSection -> Maybe Int
        depthFn road =
            --Depth to ensure about 1000 values returned,
            if road.boundingBox |> BoundingBox3d.intersects fullRenderBox then
                Nothing

            else
                Just <| round <| 10 + context.zoomLevel

        makeSvgPointAsList : Length.Length -> RoadSection -> List (Point3d Meters LocalCoords)
        makeSvgPointAsList distance road =
            -- Variant to use existing foldFn. Silly.
            [ makeSvgPoint distance road ]

        makeSvgPoint : Length.Length -> RoadSection -> Point3d Meters LocalCoords
        makeSvgPoint distance road =
            -- This is crucial, note that we put distance in X, altitude in Z and gradient in Y
            Point3d.xyz
                distance
                (Length.meters road.gradientAtStart)
                (road.startPoint.space |> Point3d.zCoordinate |> Quantity.multiplyBy context.emphasis)

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

        renderProfileData : TrackLoaded msg -> List (Point3d Meters LocalCoords)
        renderProfileData trackToRender =
            let
                ( leftIndex, rightIndex ) =
                    -- Make sure we always have a spare point outside the image if possible.
                    ( indexFromDistance leftEdge trackToRender.trackTree - 1
                    , indexFromDistanceRoundedUp rightEdge trackToRender.trackTree + 1
                    )

                ( trueLeftEdge, _ ) =
                    ( distanceFromIndex leftIndex trackToRender.trackTree
                    , distanceFromIndex rightIndex trackToRender.trackTree
                    )

                ( _, altitudeSvgPoints, _ ) =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        leftIndex
                        rightIndex
                        depthFn
                        0
                        trackToRender.trackTree
                        (foldFn makeSvgPointAsList)
                        ( trueLeftEdge, [], Nothing )

                finalSvgPoint =
                    let
                        leaf =
                            asRecord <| leafFromIndex rightIndex trackToRender.trackTree
                    in
                    Point3d.xyz
                        rightEdge
                        (Length.meters leaf.gradientAtStart)
                        (leaf.endPoint.space |> Point3d.zCoordinate |> Quantity.multiplyBy context.emphasis)
            in
            (finalSvgPoint :: altitudeSvgPoints) |> List.reverse

        altitudePreviews : List (Svg msg)
        altitudePreviews =
            Dict.foldl makeAltitudePreview [] previews

        gradientPreviews : List (Svg msg)
        gradientPreviews =
            Dict.foldl makeGradientPreview [] previews

        makeAltitudePreview : String -> PreviewData -> List (Svg msg) -> List (Svg msg)
        makeAltitudePreview k preview outputs =
            case preview.shape of
                PreviewProfile previewTree ->
                    makeAltitudePreviewHelper preview.colour previewTree :: outputs

                _ ->
                    outputs

        makeGradientPreview : String -> PreviewData -> List (Svg msg) -> List (Svg msg)
        makeGradientPreview k preview outputs =
            case preview.shape of
                PreviewProfile previewTree ->
                    makeGradientPreviewHelper preview.colour previewTree :: outputs

                _ ->
                    outputs

        makeAltitudePreviewHelper : Color -> PeteTree -> Svg msg
        makeAltitudePreviewHelper colour previewTree =
            pointsAsAltitudePolyline
                (uiColourHexString colour)
            <|
                renderProfileData { track | trackTree = previewTree }

        makeGradientPreviewHelper : Color -> PeteTree -> Svg msg
        makeGradientPreviewHelper colour previewTree =
            pointsAsGradientPolyline
                (uiColourHexString colour)
            <|
                renderProfileData { track | trackTree = previewTree }

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

        pointsToColouredCurtain : List (Point3d Meters LocalCoords) -> Svg msg
        pointsToColouredCurtain points =
            let
                pointsInScreenSpace =
                    List.map
                        (Point3d.toScreenSpace altitudeCamera gradientScreenRectangle)
                        points

                gradients =
                    List.map
                        (Point3d.yCoordinate >> Length.inMeters >> gradientColourPastel)
                        points

                makeSection :
                    Point2d Pixels LocalCoords
                    -> Point2d Pixels LocalCoords
                    -> Color.Color
                    -> Svg msg
                makeSection pt1 pt2 colour =
                    Svg.polygon2d
                        [ Svg.Attributes.stroke "none"
                        , Svg.Attributes.fill <| colourHexString colour
                        ]
                    <|
                        Polygon2d.singleLoop
                            [ pt1
                            , pt2
                            , pt2 |> Point2d.projectOnto Axis2d.x
                            , pt1 |> Point2d.projectOnto Axis2d.x
                            ]

                steppedLines =
                    List.map3
                        makeSection
                        pointsInScreenSpace
                        (List.drop 1 pointsInScreenSpace)
                        gradients
            in
            Svg.g [] steppedLines

        paintSegments =
            Svg.g [] <|
                List.map paintSegment segments

        paintSegment segment =
            let
                ( leftIndex, rightIndex ) =
                    -- Make sure we always have a spare point outside the image if possible.
                    ( indexFromDistance segment.startDistance track.trackTree
                    , indexFromDistance segment.endDistance track.trackTree
                    )

                ( _, svgShapes ) =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        leftIndex
                        rightIndex
                        (always Nothing)
                        0
                        track.trackTree
                        makeShadedBlock
                        ( segment.startDistance, [] )

                makeShadedBlock :
                    RoadSection
                    -> ( Length.Length, List (Svg msg) )
                    -> ( Length.Length, List (Svg msg) )
                makeShadedBlock road ( left, outputs ) =
                    let
                        right =
                            left |> Quantity.plus road.trueLength

                        pt1 =
                            Point3d.xyz
                                left
                                Quantity.zero
                                (road.startPoint.space |> Point3d.zCoordinate |> Quantity.multiplyBy context.emphasis)
                                |> Point3d.toScreenSpace altitudeCamera gradientScreenRectangle

                        pt2 =
                            Point3d.xyz
                                right
                                Quantity.zero
                                (road.endPoint.space |> Point3d.zCoordinate |> Quantity.multiplyBy context.emphasis)
                                |> Point3d.toScreenSpace altitudeCamera gradientScreenRectangle
                    in
                    ( right
                    , segmentHatch pt1 pt2
                        :: outputs
                    )
            in
            Svg.g [] svgShapes

        segmentHatch pt1 pt2 =
            Svg.polygon2d
                [ Svg.Attributes.stroke "none"
                , Svg.Attributes.fill "black"
                , Svg.Attributes.opacity "40%"
                ]
            <|
                Polygon2d.singleLoop
                    [ pt1
                    , pt2
                    , pt2 |> Point2d.projectOnto Axis2d.x
                    , pt1 |> Point2d.projectOnto Axis2d.x
                    ]

        pointsAsGradientPolyline : String -> List (Point3d Meters LocalCoords) -> Svg msg
        pointsAsGradientPolyline colour points =
            let
                zoomAdjust pt =
                    -- NOTE This is one of our weird combined points.
                    Point3d.xyz
                        (Point3d.xCoordinate pt)
                        (Point3d.yCoordinate pt |> Quantity.multiplyBy (compensateForZoom 1.0))
                        (Point3d.zCoordinate pt)

                pointsInScreenSpace =
                    List.map
                        (zoomAdjust
                            >> Point3d.toScreenSpace gradientCamera gradientScreenRectangle
                        )
                        points

                makeStep :
                    Point2d Pixels LocalCoords
                    -> Point2d Pixels LocalCoords
                    -> List (Point2d Pixels LocalCoords)
                makeStep pt1 pt2 =
                    [ pt1
                    , Point2d.xy (xCoordinate pt2) (yCoordinate pt1)
                    , pt2
                    ]

                steppedLines =
                    List.concat <|
                        List.map2
                            makeStep
                            pointsInScreenSpace
                            (List.drop 1 pointsInScreenSpace)
            in
            Svg.polyline2d
                [ Svg.Attributes.stroke colour
                , Svg.Attributes.fill "none"
                , Svg.Attributes.strokeWidth "3"
                , Svg.Attributes.strokeLinecap "round"
                , Svg.Attributes.strokeLinejoin "round"
                ]
                (Polyline2d.fromVertices steppedLines)

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

        renderDataOnce =
            renderProfileData track

        altitudeChart =
            Svg.svg
                [ Svg.Attributes.width svgWidth
                , Svg.Attributes.height svgHeight
                ]
                [ Svg.relativeTo topLeftFrame <|
                    Svg.g []
                        [ pointsAsAltitudePolyline "black" <| renderDataOnce
                        , pointsToColouredCurtain renderDataOnce
                        , paintSegments
                        , Svg.g [] (orangeAltitudeSvg :: orangeText ++ purpleSvg)
                        , Svg.g [] altitudePreviews
                        ]
                ]

        gradientChart =
            Svg.svg
                [ Svg.Attributes.width svgWidth
                , Svg.Attributes.height svgHeight
                ]
                [ Svg.relativeTo topLeftFrame <|
                    Svg.g []
                        [ distanceAxis
                        , pointsAsGradientPolyline "grey" <| renderDataOnce
                        , Svg.g [] (orangeGradientSvg :: orangeText)
                        , Svg.g [] gradientPreviews
                        ]
                ]

        distanceAxis =
            pointsAsGradientPolyline "grey"
                [ Point3d.xyz leftEdge Quantity.zero Quantity.zero
                , Point3d.xyz rightEdge Quantity.zero Quantity.zero
                ]

        orangeLeaf =
            asRecord <| DomainModel.leafFromIndex track.currentPosition track.trackTree

        orangePoint =
            DomainModel.earthPointFromIndex track.currentPosition track.trackTree

        orangeAltitude2d =
            pointInAltitudeView context track.currentPosition track.trackTree
                |> .space
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
                |> .space
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
                                |> .space
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
                [ Svg.Attributes.fill "grey"
                , Svg.Attributes.fontFamily "sans-serif"
                , Svg.Attributes.fontSize "14px"
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
            , textLine 2 <|
                showShortMeasure settings.imperial <|
                    Point3d.zCoordinate <|
                        orangePoint.space
            , textLine 3 <|
                showLongMeasure settings.imperial <|
                    DomainModel.distanceFromIndex track.currentPosition track.trackTree
            ]

        topLeftFrame =
            Frame2d.atPoint
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity altitudeHeight))
                |> Frame2d.reverseY

        common3dSceneAttributes =
            --TODO: Avoid copying these from common elemtns
            [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
            , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
            , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
            , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> msgWrapper)
            , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.pointer
            , Border.width 0
            , Border.color FlatColors.ChinesePalette.peace
            ]
    in
    column
        (pointer
            --:: Background.color FlatColors.ChinesePalette.antiFlashWhite
            :: (inFront <| zoomButtons settings msgWrapper context)
            --:: (htmlAttribute <| Mouse.onMove (MouseMove >> msgWrapper))
            :: common3dSceneAttributes
        )
        [ Element.html altitudeChart
        , Element.html gradientChart
        ]


initialiseView :
    Int
    -> PeteTree
    -> Maybe ProfileContext
    -> ProfileContext
initialiseView current treeNode currentProfileContext =
    let
        currentPoint =
            earthPointFromIndex current treeNode

        currentDistance =
            distanceFromIndex current treeNode
    in
    case currentProfileContext of
        Just context ->
            { context
                | orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = 0.0
                , defaultZoomLevel = 0.0
                , focalPoint3d =
                    Point3d.xyz
                        currentDistance
                        Quantity.zero
                        (Point3d.zCoordinate currentPoint.space
                            |> Quantity.multiplyBy context.emphasis
                        )
                , metresPerPixel = 10.0
                , waitingForClickDelay = False
                , followSelectedPoint = True
            }

        Nothing ->
            { orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 0.0
            , defaultZoomLevel = 0.0
            , focalPoint = Quantity.zero
            , focalPoint3d =
                Point3d.xyz
                    currentDistance
                    Quantity.zero
                    (currentPoint.space |> Point3d.zCoordinate)
            , followSelectedPoint = True
            , metresPerPixel = 10.0
            , waitingForClickDelay = False
            , emphasis = 1.0
            , mouseEvent = Nothing
            , contextSuffix = ""
            , colouredChart = False
            }


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> ProfileContext
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
