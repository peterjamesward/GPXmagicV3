module ViewProfileCharts exposing (..)

import Actions exposing (ToolAction(..))
import BoundingBox3d
import Chart as C exposing (chart, interpolated, series, xAxis, xLabels, yAxis, yLabels)
import Chart.Attributes as CA exposing (margin, withGrid)
import Chart.Events as CE
import Chart.Item as CI
import Color
import ColourPalette exposing (gradientColourPastel, gradientHue)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, toFloatQuantity)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (colourHexString)
import Vector3d
import ViewPureStyles exposing (useIcon)


type ClickZone
    = ZoneAltitude
    | ZoneGradient


type Msg
    = ImageMouseWheel Float
    | ImageGrab ClickZone Mouse.Event
    | ImageDrag ClickZone Mouse.Event
    | ImageRelease ClickZone Mouse.Event
    | ImageNoOp
    | ImageClick (Maybe CE.Point)
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
    { dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float -- 0 = whole track, 1 = half, etc.
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
    , waitingForClickDelay : Bool
    , profileData : List ProfileDatum
    , gradientProblems : List Int
    , imperial : Bool
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
        currentPointAltitude =
            earthPointFromIndex track.currentPosition track.trackTree
                |> Point3d.zCoordinate
                |> (if context.imperial then
                        Length.inFeet

                    else
                        Length.inMeters
                   )

        currentPointGradient =
            leafFromIndex track.currentPosition track.trackTree
                |> gradientFromNode

        currentPointDistance =
            distanceFromIndex track.currentPosition track.trackTree
                |> (if context.imperial then
                        Length.inMiles

                    else
                        Length.inMeters
                   )

        ( altitudeWidth, altitudeHeight ) =
            -- Subtract pixels we use for padding around the scene view.
            ( givenWidth
                --|> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Pixels.inPixels
            , givenHeight
                --|> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy splitProportion
                |> Pixels.inPixels
            )

        ( gradientWidth, gradientHeight ) =
            ( givenWidth
                --|> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Pixels.inPixels
            , givenHeight
                --|> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy (1.0 - splitProportion)
                |> Pixels.inPixels
            )

        backgroundColour =
            colourHexString FlatColors.ChinesePalette.antiFlashWhite
    in
    column []
        [ el
            [ width <| px <| round altitudeWidth
            , height <| px <| round altitudeHeight
            , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
            ]
          <|
            html <|
                C.chart
                    [ CA.height altitudeHeight
                    , CA.width altitudeWidth
                    , CA.htmlAttrs [ HA.style "background" backgroundColour ]
                    , CA.range [ CA.likeData ]
                    , CA.domain [ CA.likeData ]
                    , CA.margin { top = 10, bottom = 30, left = 30, right = 20 }
                    , CA.padding { top = 10, bottom = 30, left = 20, right = 20 }
                    , CE.onClick (msgWrapper << ImageClick << Just) CE.getCoords
                    ]
                    [ C.xAxis []
                    , C.xTicks []
                    , C.xLabels []
                    , C.yAxis []
                    , C.yTicks []
                    , C.yLabels []
                    , C.withPlane <|
                        \p ->
                            [ C.line
                                [ CA.x1 p.x.min
                                , CA.y1 currentPointAltitude
                                , CA.x2 p.x.max
                                , CA.dashed [ 5, 5 ]
                                , CA.color CA.red
                                ]
                            , C.line
                                [ CA.x1 currentPointDistance
                                , CA.y1 p.y.min
                                , CA.y2 p.y.max
                                , CA.dashed [ 5, 5 ]
                                , CA.width 2
                                , CA.color CA.red
                                ]
                            ]
                    , series .distance
                        [ interpolated .altitude
                            [ CA.width 2
                            , CA.opacity 0.2
                            , CA.gradient []
                            ]
                            []
                        ]
                        context.profileData
                    ]
        , el
            [ width <| px <| round gradientWidth
            , height <| px <| round gradientHeight
            , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
            ]
          <|
            html <|
                C.chart
                    [ CA.height gradientHeight
                    , CA.width gradientWidth
                    , CA.htmlAttrs [ HA.style "background" backgroundColour ]
                    , CA.range [ CA.likeData ]
                    , CA.domain [ CA.likeData ]
                    , CA.margin { top = 20, bottom = 30, left = 30, right = 20 }
                    , CA.padding { top = 20, bottom = 20, left = 20, right = 20 }
                    , CE.onClick (msgWrapper << ImageClick << Just) CE.getCoords
                    ]
                    [ C.xAxis []
                    , C.xTicks []
                    , C.xLabels []
                    , C.yAxis []
                    , C.yTicks []
                    , C.yLabels []
                    , C.withPlane <|
                        \p ->
                            [ C.line
                                [ CA.x1 p.x.min
                                , CA.y1 currentPointGradient
                                , CA.x2 p.x.max
                                , CA.dashed [ 5, 5 ]
                                , CA.color CA.red
                                ]
                            , C.line
                                [ CA.x1 currentPointDistance
                                , CA.y1 p.y.min
                                , CA.y2 p.y.max
                                , CA.dashed [ 5, 5 ]
                                , CA.width 2
                                , CA.color CA.red
                                ]
                            ]
                    , series .distance
                        [ interpolated .gradient
                            [ CA.width 2, CA.stepped ]
                            []
                        ]
                        context.profileData
                    ]
        ]


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) context =
    let
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
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel + 0.5 }
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel - 0.5 }
            , []
            )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context), [] )

        ImageNoOp ->
            ( context, [] )

        ImageClick point ->
            case point of
                Just isPoint ->
                    ( context
                    , [ Actions.SetCurrent <|
                            indexFromDistance (Length.meters isPoint.x) track.trackTree
                      ]
                    )

                Nothing ->
                    ( context, [] )

        ClickDelayExpired ->
            --TODO: Replace with logic that looks for mouse movement.
            ( { context | waitingForClickDelay = False }, [] )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                zoomLevel =
                    clamp 0 10 <| context.zoomLevel + increment
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
                            --TODO: Follow the chart example.
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
                    ( newContext, [] )

                _ ->
                    ( context, [] )

        ImageRelease zone event ->
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

        ImageDoubleClick clickZone event ->
            ( context, [] )


type alias ProfileDatum =
    -- Intended for use with the terezka charts, but agnostic.
    -- One required for each point
    { distance : Float -- metres or miles depending on units setting
    , altitude : Float -- metres or feet
    , gradient : Float -- percent
    , colour : Color.Color -- use average gradient if not Leaf
    }


renderProfileDataForCharts : Bool -> List Int -> Context -> TrackLoaded msg -> Context
renderProfileDataForCharts imperial bumps context track =
    -- "bumps" = indices of abrupt gradient changes.
    let
        lengthConversion =
            if imperial then
                Length.inMiles

            else
                Length.inMeters

        heightConversion =
            if imperial then
                Length.inFeet

            else
                Length.inMeters

        trackLengthInView =
            trueLength track.trackTree |> Quantity.multiplyBy (0.5 ^ context.zoomLevel)

        pointOfInterest =
            if context.followSelectedPoint then
                distanceFromIndex track.currentPosition track.trackTree

            else
                Point3d.xCoordinate context.focalPoint

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
            --Depth to ensure about 1000 values returned,
            Just <| round <| 10 + context.zoomLevel

        foldFn :
            RoadSection
            -> ( Length.Length, List ProfileDatum, Maybe RoadSection )
            -> ( Length.Length, List ProfileDatum, Maybe RoadSection )
        foldFn road ( nextDistance, outputs, _ ) =
            let
                newEntry : ProfileDatum
                newEntry =
                    { distance = lengthConversion nextDistance
                    , altitude = heightConversion <| Point3d.zCoordinate road.startPoint
                    , gradient = road.gradientAtStart * 0.5 + road.gradientAtEnd * 0.5
                    , colour = gradientColourPastel (gradientFromNode <| Leaf road)
                    }
            in
            ( nextDistance |> Quantity.plus road.trueLength
            , newEntry :: outputs
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
                    { distance = lengthConversion rightEdge
                    , altitude = heightConversion <| Point3d.zCoordinate finalLeaf.endPoint
                    , gradient = gradientFromNode <| Leaf finalLeaf
                    , colour = gradientColourPastel (gradientFromNode <| Leaf finalLeaf)
                    }

                Nothing ->
                    -- Can't happen
                    { distance = lengthConversion rightEdge
                    , altitude = 0.0
                    , gradient = 0.0
                    , colour = Color.black
                    }
    in
    { context
        | profileData = finalDatum :: result
        , gradientProblems = bumps
        , imperial = imperial
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
                | orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = 0.0
                , defaultZoomLevel = 0.0
                , focalPoint = treeNode |> leafFromIndex current |> startPoint
                , metresPerPixel = 10.0
                , waitingForClickDelay = False
            }

        Nothing ->
            { orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 0.0
            , defaultZoomLevel = 0.0
            , focalPoint = treeNode |> leafFromIndex current |> startPoint
            , followSelectedPoint = True
            , metresPerPixel = 10.0
            , waitingForClickDelay = False
            , profileData = []
            , gradientProblems = []
            , imperial = False
            }
