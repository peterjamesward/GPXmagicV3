module Tools.CurveFormer exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import Arc2d
import Arc3d
import Axis2d
import BoundingBox2d
import Circle3d exposing (Circle3d)
import Color
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, endPoint, skipCount, startPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Geometry101
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Interval
import Length exposing (Meters)
import LineSegment2d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Maybe.Extra
import Pixels
import Point2d exposing (Point2d)
import Point3d
import Polyline2d
import Polyline3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import String.Interpolate
import Svg
import Svg.Attributes as SA
import SweptAngle
import Tools.CurveFormerOptions exposing (GradientSmoothing(..), Options, Point)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import Utils
import UtilsForViews exposing (flatBox, showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, edges, neatToolsBorder, noTrackMessage, prettyButtonStyles, subtleToolStyles, useIcon)


toolId =
    "radius"


defaultOptions : Options
defaultOptions =
    { vector = Vector2d.zero
    , referencePoint = Nothing
    , dragging = Nothing
    , smoothGradient = Holistic
    , pushRadius = Length.meters 10.0
    , pullRadius = Length.meters 15.0
    , spacing = Length.meters 5.0
    , usePullRadius = False
    , pointsWithinCircle = Dict.empty
    , pointsWithinDisc = Dict.empty

    --, circle = Nothing
    , pointsAreContiguous = False
    , newTrackPoints = []
    , fixedAttachmentPoints = Nothing
    , transitionRadius = Length.meters 20.0
    , lastVector = Vector2d.zero
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | SetGradientSmoothingMode GradientSmoothing
    | DraggerReset
    | SetPushRadius Float
    | SetDiscWidth Float
    | SetTransitionRadius Float
    | SetSpacing Float
    | ToggleUsePullRadius Bool
    | ApplyWithOptions
    | DisplayInfo String String


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource, ( Int, Int ) )
applyUsingOptions options track =
    case options.fixedAttachmentPoints of
        Just ( entryPoint, exitPoint ) ->
            let
                ( fromStart, fromEnd ) =
                    ( entryPoint, skipCount track.trackTree - exitPoint )

                newTree =
                    DomainModel.replaceRange
                        (fromStart + 1)
                        (fromEnd + 1)
                        track.referenceLonLat
                        (List.map .gpx <| options.newTrackPoints)
                        track.trackTree

                oldPoints =
                    DomainModel.extractPointsInRange
                        fromStart
                        fromEnd
                        track.trackTree
            in
            ( newTree
            , oldPoints |> List.map Tuple.second
            , ( entryPoint, exitPoint )
            )

        Nothing ->
            ( Just track.trackTree, [], ( 0, 0 ) )


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, previewActions defaultOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "formerOutcome", HidePreview "formerTool" ] )


previewActions newOptions colour track =
    -- Subverting this mechanism to show the discs and captured points on the views.
    [ ShowPreview
        { tag = "formerOutcome"
        , shape = PreviewCircle
        , colour = colour
        , points = newOptions.newTrackPoints
        }
    , ShowPreview
        { tag = "formerTool"
        , shape = PreviewToolSupplied <| showToolTrackInteractions newOptions track
        , colour = colour
        , points = []
        }
    ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetPushRadius radius ) ->
            let
                newOptions =
                    { options | pushRadius = Length.meters radius }
                        |> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, SetDiscWidth width ) ->
            let
                newOptions =
                    { options | pullRadius = options.pushRadius |> Quantity.plus (Length.meters width) }
                        |> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, SetTransitionRadius radius ) ->
            let
                newOptions =
                    { options | transitionRadius = Length.meters radius }
                        |> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, SetSpacing spacing ) ->
            let
                newOptions =
                    { options | spacing = Length.meters spacing }
                        |> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, ToggleUsePullRadius _ ) ->
            let
                newOptions =
                    { options | usePullRadius = not options.usePullRadius }
                        |> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, SetGradientSmoothingMode mode ) ->
            let
                newOptions =
                    { options | smoothGradient = mode }
                        |> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, DraggerGrab offset ) ->
            let
                newOptions =
                    { options | dragging = Just offset }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, DraggerMove offset ) ->
            case options.dragging of
                Nothing ->
                    ( options, [] )

                Just dragStart ->
                    let
                        newVector =
                            options.lastVector |> Vector2d.plus (Vector2d.from dragStart offset)

                        newOptions =
                            { options
                                | vector = newVector
                                , referencePoint =
                                    if options.referencePoint == Nothing then
                                        Just <| DomainModel.earthPointFromIndex track.currentPosition track.trackTree

                                    else
                                        options.referencePoint
                            }
                                |> makeCurveIfPossible track
                    in
                    ( newOptions, previewActions newOptions previewColour track )

        ( Just track, DraggerRelease _ ) ->
            let
                newOptions =
                    { options
                        | dragging = Nothing
                        , lastVector = options.vector
                    }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, DraggerReset ) ->
            let
                newOptions =
                    { options
                        | dragging = Nothing
                        , referencePoint = Nothing
                        , lastVector = Vector2d.zero
                        , vector = Vector2d.zero
                    }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ Actions.CurveFormerApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial wrapper options track =
    let
        i18n =
            I18N.text location toolId

        squared x =
            x * x

        showPushRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPushRadius << squared
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "radius")
                                [ showShortMeasure imperial options.pushRadius ]
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = options.pushRadius |> Length.inMeters |> sqrt
                , thumb = Input.defaultThumb
                }

        showTransitionRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetTransitionRadius << squared
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "join")
                                [ showShortMeasure imperial options.transitionRadius ]
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = options.transitionRadius |> Length.inMeters |> sqrt
                , thumb = Input.defaultThumb
                }

        showPullRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetDiscWidth
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "join")
                                [ showShortMeasure imperial options.pullRadius ]
                , min = 1.0
                , max = 40.0
                , step = Nothing
                , value = options.pullRadius |> Quantity.minus options.pushRadius |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showSpacingSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetSpacing
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location toolId "spacing")
                                [ showShortMeasure imperial options.spacing ]
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = options.spacing |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showActionButtons =
            row [ padding 5, spacing 5, width fill ]
                [ Input.button neatToolsBorder
                    { label = i18n "Reset"
                    , onPress = Just <| wrapper DraggerReset
                    }
                , case ( List.length options.newTrackPoints >= 3, options.pointsAreContiguous ) of
                    ( _, True ) ->
                        Input.button
                            neatToolsBorder
                            { label = i18n "Apply"
                            , onPress = Just <| wrapper ApplyWithOptions
                            }

                    ( _, False ) ->
                        Input.button
                            (width fill :: subtleToolStyles)
                            { label = paragraph [ width fill ] <| [ i18n "none" ]
                            , onPress = Nothing
                            }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange =
                    wrapper
                        << SetGradientSmoothingMode
                        << (\check ->
                                if check then
                                    Holistic

                                else
                                    Piecewise
                           )
                , icon = Input.defaultCheckbox
                , checked = options.smoothGradient == Holistic
                , label = Input.labelRight [ centerY ] (i18n "gradient")
                }

        showPullSelection =
            Input.checkbox []
                { onChange = wrapper << ToggleUsePullRadius
                , icon = Input.defaultCheckbox
                , checked = options.usePullRadius
                , label = Input.labelRight [ centerY ] (i18n "outliers")
                }
    in
    case track of
        Just isTrack ->
            row
                [ paddingEach { edges | right = 10 }
                , spacing 5
                , width fill
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ twoWayDragControl options wrapper
                , column [ width fill, spacing 5 ]
                    [ wrappedRow
                        [ Element.alignLeft
                        , Element.width Element.fill
                        , spacing 5
                        ]
                        [ showPushRadiusSlider
                        , showTransitionRadiusSlider
                        , showSpacingSlider
                        , showPullSelection
                        , if options.usePullRadius then
                            showPullRadiusSlider

                          else
                            none
                        ]
                    , showModeSelection
                    , showActionButtons
                    ]
                ]

        Nothing ->
            noTrackMessage location


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


controlSvgRadius =
    100


twoWayDragControl : Options -> (Msg -> msg) -> Element msg
twoWayDragControl options wrapper =
    let
        clickableContainer =
            el
                [ htmlAttribute <| Pointer.onDown (.pointer >> .offsetPos >> point >> DraggerGrab >> wrapper)
                , htmlAttribute <| Pointer.onMove (.pointer >> .offsetPos >> point >> DraggerMove >> wrapper)
                , htmlAttribute <| Pointer.onUp (.pointer >> .offsetPos >> point >> DraggerRelease >> wrapper)
                , htmlAttribute <| Html.Attributes.style "touch-action" "none"
                , Element.pointer
                , Element.alignLeft
                ]
                << html
                << Svg.svg
                    [ SA.viewBox "-150 -150 300 300"
                    , SA.width "140px"
                    , SA.height "140px"
                    ]

        ( x, y ) =
            Vector2d.components options.vector

        ( xPoint, yPoint ) =
            ( String.fromFloat <| Length.inMeters x
            , String.fromFloat <| Length.inMeters y
            )
    in
    clickableContainer <|
        [ Svg.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| String.fromInt controlSvgRadius
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SA.fill "darkslategrey"
            ]
            []
        , Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 xPoint
            , SA.y2 yPoint
            , SA.stroke "orange"
            , SA.strokeWidth "10"
            , SA.strokeLinecap "round"
            ]
            []
        ]


type alias FoldState =
    { newPoints : List EarthPoint
    }


formCurveWithOptions : Bool -> Options -> Int -> Int -> PeteTree -> List EarthPoint
formCurveWithOptions isLoop options fromStart fromEnd treeNode =
    let
        foldFn : RoadSection -> FoldState -> FoldState
        foldFn road state =
            state

        foldOutput =
            DomainModel.traverseTreeBetweenLimitsToDepth
                fromStart
                (skipCount treeNode - fromEnd)
                (always Nothing)
                0
                treeNode
                foldFn
                (FoldState [])
    in
    foldOutput.newPoints |> List.reverse



-- Some 3d views that arguably should not be in here but where else?


showToolTrackInteractions : Options -> TrackLoaded msg -> List (Entity LocalCoords)
showToolTrackInteractions options track =
    showCircle options track
        ++ showDisc options track
        ++ highlightPoints Color.white (Dict.keys options.pointsWithinCircle) track
        ++ highlightPoints Color.blue (Dict.keys options.pointsWithinDisc) track


getCircle : Options -> TrackLoaded msg -> Circle3d Meters LocalCoords
getCircle options track =
    let
        orange =
            DomainModel.earthPointFromIndex track.currentPosition track.trackTree

        translation =
            -- Flip Y because drag control is SVG coordinate based.
            Vector3d.xyz (Vector2d.xComponent options.vector)
                (Vector2d.yComponent options.vector |> Quantity.negate)
                Quantity.zero

        centre =
            case options.referencePoint of
                Just localReference ->
                    localReference.space
                        |> Point3d.translateBy translation

                Nothing ->
                    orange.space
                        |> Point3d.translateBy translation
    in
    Circle3d.withRadius options.pushRadius Direction3d.positiveZ centre


getOuterCircle : Options -> TrackLoaded msg -> Circle3d Meters LocalCoords
getOuterCircle options track =
    let
        orange =
            DomainModel.earthPointFromIndex track.currentPosition track.trackTree

        translation =
            -- Flip Y because drag control is SVG coordinate based.
            Vector3d.xyz (Vector2d.xComponent options.vector)
                (Vector2d.yComponent options.vector |> Quantity.negate)
                Quantity.zero

        centre =
            case options.referencePoint of
                Just localReference ->
                    localReference.space
                        |> Point3d.translateBy translation

                Nothing ->
                    orange.space
                        |> Point3d.translateBy translation

        outerRadius =
            options.pullRadius
    in
    Circle3d.withRadius outerRadius Direction3d.positiveZ centre


showCircle : Options -> TrackLoaded msg -> List (Entity LocalCoords)
showCircle options track =
    let
        circle =
            getCircle options track
    in
    let
        arc =
            Circle3d.toArc circle

        segments =
            Arc3d.segments 20 arc |> Polyline3d.segments

        material =
            Material.color Color.white

        drawSegment segment =
            Scene3d.lineSegment material segment
    in
    List.map drawSegment segments


showDisc : Options -> TrackLoaded msg -> List (Entity LocalCoords)
showDisc options track =
    let
        circle =
            getCircle options track

        centre =
            Circle3d.centerPoint circle

        direction =
            Circle3d.axialDirection circle

        outerCircle =
            Circle3d.withRadius
                options.pullRadius
                direction
                centre

        arc =
            Circle3d.toArc outerCircle

        segments =
            Arc3d.segments 20 arc |> Polyline3d.segments

        material =
            Material.color Color.lightYellow

        drawSegment segment =
            Scene3d.lineSegment material segment
    in
    if options.usePullRadius then
        List.map drawSegment segments

    else
        []


highlightPoints : Color.Color -> List Int -> TrackLoaded msg -> List (Entity LocalCoords)
highlightPoints color points track =
    let
        material =
            Material.color color

        highlightPoint index =
            let
                xyzt =
                    DomainModel.earthPointFromIndex index track.trackTree
            in
            Scene3d.point { radius = Pixels.pixels 5 } material xyzt.space
    in
    List.map highlightPoint points



-- And here at last is the heavy lifting.


type alias IntersectionInformation =
    { intersection : Point2d Meters LocalCoords
    , distanceAlong : Quantity Float Meters
    , tangentPoint : Point2d Meters LocalCoords
    , joinsBendAt : Point2d Meters LocalCoords
    , originalTrackPoint : EarthPoint
    , index : Int
    }


type TransitionMode
    = EntryMode
    | ExitMode


makeCurveIfPossible : TrackLoaded msg -> Options -> Options
makeCurveIfPossible track options =
    -- The compute we do here is most of the work for the Apply, so
    -- we keep it in our model.
    let
        ( fromStart, fromEnd ) =
            if track.markerPosition == Nothing then
                ( 0, 0 )

            else
                TrackLoaded.getRangeFromMarkers track

        ( startRange, endRange ) =
            ( fromStart, skipCount track.trackTree - fromEnd )

        searchInterval =
            Interval.from startRange endRange

        circle =
            getCircle options track

        ( centre, axis, drawingPlane ) =
            -- Yes, I know this is trite.
            ( Circle3d.centerPoint circle
            , Circle3d.axis circle
            , SketchPlane3d.xy
                |> SketchPlane3d.translateBy
                    (Vector3d.withLength (Point3d.zCoordinate <| Circle3d.centerPoint circle) Direction3d.positiveZ)
            )

        centreOnPlane =
            -- Most of the work is planar; we layer the elevations on at the end.
            centre |> Point3d.projectInto drawingPlane

        innerBox =
            BoundingBox2d.withDimensions
                ( Quantity.twice options.pushRadius, Quantity.twice options.pushRadius )
                centreOnPlane

        outerBox =
            BoundingBox2d.withDimensions
                ( Quantity.twice options.pullRadius, Quantity.twice options.pullRadius )
                centreOnPlane

        overlapsCircleAndRange : Int -> Int -> RoadSection -> Bool
        overlapsCircleAndRange start end road =
            -- Road section is interesting?
            (road.boundingBox |> flatBox |> BoundingBox2d.intersects innerBox)
                && (Interval.from start end |> Interval.intersects searchInterval)

        isWithinPushRadius pt =
            pt
                |> Point3d.distanceFromAxis axis
                |> Quantity.lessThanOrEqualTo options.pushRadius

        isWithinPullRadius pt =
            pt
                |> Point3d.distanceFromAxis axis
                |> Quantity.lessThanOrEqualTo options.pullRadius

        isWithinDisc pt =
            isWithinPullRadius pt && (not <| isWithinPushRadius pt)

        overlapsDiscAndRange start end road =
            (road.boundingBox |> flatBox |> BoundingBox2d.intersects outerBox)
                && (Interval.from start end |> Interval.intersects searchInterval)

        pointsWithinCircle : Dict Int RoadSection
        pointsWithinCircle =
            let
                collector index road dict =
                    if isWithinPushRadius road.startPoint.space then
                        Dict.insert index road dict

                    else
                        dict
            in
            DomainModel.queryRoadsUsingFilter
                overlapsCircleAndRange
                track.trackTree
                collector
                Dict.empty

        pointsWithinDisc : Dict Int RoadSection
        pointsWithinDisc =
            let
                collector index road dict =
                    if isWithinDisc road.startPoint.space then
                        Dict.insert index road dict

                    else
                        dict
            in
            DomainModel.queryRoadsUsingFilter
                overlapsDiscAndRange
                track.trackTree
                collector
                Dict.empty

        capturedRoadSections : Dict Int RoadSection
        capturedRoadSections =
            Dict.union pointsWithinCircle pointsWithinDisc

        -- Care over turn direction. Bend may exceed 180 degrees and Angle would not be good.
        -- Perhaps simpler and more reliable is "which side of the entry road is the centre?"
        isLeftHandBend =
            let
                runningAverageDirectionChange :
                    Int
                    -> RoadSection
                    -> ( Maybe (Direction2d LocalCoords), Float )
                    -> ( Maybe (Direction2d LocalCoords), Float )
                runningAverageDirectionChange idx road change =
                    case change of
                        ( Nothing, _ ) ->
                            ( Just road.directionAtStart, 0.0 )

                        ( Just previousDirection, prevTotal ) ->
                            ( Just road.directionAtStart
                            , road.directionAtStart
                                |> Direction2d.angleFrom previousDirection
                                |> Angle.inDegrees
                                |> (+) prevTotal
                            )

                ( _, changeInDirection ) =
                    Dict.foldl runningAverageDirectionChange ( Nothing, 0.0 ) capturedRoadSections
            in
            changeInDirection > 0.0

        findAcceptableTransition : TransitionMode -> Int -> Int -> Maybe IntersectionInformation
        findAcceptableTransition mode idx1 idx2 =
            let
                -- Construct a parallel to the given road segment, 'r' meters towards the arc start.
                -- Where this intersects the '2r' circle, is centre of the entry bend.
                -- Where this new circle is tangent to the line segment is where the entry begins.
                -- If the entry precedes the segment start, we have failed to find a line;
                -- the user will need to move the marker. Or we recurse back down the track; that might work.
                ( tp1, tp2 ) =
                    ( DomainModel.earthPointFromIndex idx1 track.trackTree
                    , DomainModel.earthPointFromIndex idx2 track.trackTree
                    )

                entryLineSegment =
                    LineSegment2d.from
                        (Point3d.projectInto drawingPlane tp1.space)
                        (Point3d.projectInto drawingPlane tp2.space)

                entryLineAxis =
                    Axis2d.throughPoints
                        (LineSegment2d.startPoint entryLineSegment)
                        (LineSegment2d.endPoint entryLineSegment)

                entryLineShiftVector =
                    let
                        shiftAmount =
                            if isLeftHandBend then
                                Quantity.negate options.transitionRadius

                            else
                                options.transitionRadius
                    in
                    Maybe.map (Vector2d.withLength shiftAmount)
                        (LineSegment2d.perpendicularDirection entryLineSegment)

                shiftedEntryLine =
                    case entryLineShiftVector of
                        Just theVector ->
                            Just <| LineSegment2d.translateBy theVector entryLineSegment

                        _ ->
                            Nothing

                outerCircleIntersections =
                    case shiftedEntryLine of
                        Just line ->
                            let
                                lineEqn =
                                    Geometry101.lineEquationFromTwoPoints
                                        (LineSegment2d.startPoint line |> Point2d.toRecord Length.inMeters)
                                        (LineSegment2d.endPoint line |> Point2d.toRecord Length.inMeters)

                                outerCircle =
                                    { centre = centreOnPlane |> Point2d.toRecord Length.inMeters
                                    , radius =
                                        Quantity.plus options.pushRadius options.transitionRadius
                                            |> Length.inMeters
                                    }
                            in
                            Geometry101.lineCircleIntersections lineEqn outerCircle
                                |> List.map (Point2d.fromRecord Length.meters)

                        Nothing ->
                            []

                validCounterBendCentresAndTangentPoints : List IntersectionInformation
                validCounterBendCentresAndTangentPoints =
                    -- Point is 'valid' if it is not 'before' the segment start (or axis origin).
                    -- Want to return the outer intersection point (new bend centre) and the tangent point.
                    case entryLineAxis of
                        Just sameOldAxis ->
                            let
                                selectionFunction =
                                    case mode of
                                        EntryMode ->
                                            List.Extra.minimumBy (.distanceAlong >> Length.inMeters)

                                        ExitMode ->
                                            List.Extra.maximumBy (.distanceAlong >> Length.inMeters)

                                elaborateIntersectionPoint i =
                                    let
                                        distanceAlong =
                                            Point2d.signedDistanceAlong sameOldAxis i

                                        tangentPoint2d =
                                            Point2d.along sameOldAxis distanceAlong

                                        bendJoinPoint =
                                            Point2d.interpolateFrom
                                                centreOnPlane
                                                i
                                                (Quantity.ratio options.pushRadius
                                                    (Quantity.plus options.pushRadius options.transitionRadius)
                                                )
                                    in
                                    { intersection = i
                                    , distanceAlong = distanceAlong
                                    , tangentPoint = tangentPoint2d
                                    , joinsBendAt = bendJoinPoint
                                    , originalTrackPoint =
                                        -- Point on track, used to compute altitudes and
                                        -- final line segment to adjoining tangent point.
                                        case mode of
                                            EntryMode ->
                                                tp1

                                            ExitMode ->
                                                tp2
                                    , index =
                                        -- Point on track, used to compute altitudes and
                                        -- final line segment to adjoining tangent point.
                                        case mode of
                                            EntryMode ->
                                                idx1

                                            ExitMode ->
                                                idx2
                                    }
                            in
                            List.map elaborateIntersectionPoint outerCircleIntersections
                                |> selectionFunction
                                |> Maybe.Extra.toList
                                |> List.filter
                                    (.distanceAlong
                                        >> Quantity.greaterThanOrEqualTo Quantity.zero
                                    )
                                |> List.filter
                                    (.distanceAlong
                                        >> Quantity.lessThanOrEqualTo (LineSegment2d.length entryLineSegment)
                                    )

                        _ ->
                            []
            in
            List.head validCounterBendCentresAndTangentPoints

        arcToSegments arc =
            let
                arcLength =
                    Length.inMeters (Arc2d.radius arc)
                        * Angle.inRadians (Arc2d.sweptAngle arc)
                        |> abs

                entryArcNumSegments =
                    arcLength
                        / Length.inMeters options.spacing
                        |> ceiling
                        |> max 1
            in
            Arc2d.segments entryArcNumSegments arc |> Polyline2d.segments

        entryCurveSeeker : Int -> Int -> Maybe IntersectionInformation
        entryCurveSeeker limit index =
            let
                ( tp1, tp2 ) =
                    ( DomainModel.earthPointFromIndex (index - 1) track.trackTree
                    , DomainModel.earthPointFromIndex index track.trackTree
                    )
            in
            case findAcceptableTransition EntryMode (index - 1) index of
                Just transition ->
                    Just transition

                Nothing ->
                    if index > 1 && limit > 0 then
                        entryCurveSeeker (limit - 1) (index - 1)

                    else
                        Nothing

        exitCurveSeeker : Int -> Int -> Maybe IntersectionInformation
        exitCurveSeeker limit index =
            let
                ( tp1, tp2 ) =
                    ( DomainModel.earthPointFromIndex index track.trackTree
                    , DomainModel.earthPointFromIndex (index + 1) track.trackTree
                    )
            in
            case findAcceptableTransition ExitMode index (index + 1) of
                Just transition ->
                    Just transition

                Nothing ->
                    if index < routeLength - 2 && limit > 0 then
                        exitCurveSeeker (limit - 1) (index + 1)

                    else
                        Nothing

        routeLength =
            skipCount track.trackTree

        ( entryInformation, exitInformation ) =
            -- Scan route for joins that suffice.
            ( capturedRoadSections
                |> Dict.keys
                |> List.head
                |> Maybe.andThen (entryCurveSeeker 100)
            , capturedRoadSections
                |> Dict.keys
                |> List.Extra.last
                |> Maybe.andThen (exitCurveSeeker 100)
            )

        entryCurve =
            case entryInformation of
                Just { intersection, distanceAlong, tangentPoint, joinsBendAt } ->
                    Arc2d.withRadius
                        options.transitionRadius
                        (if isLeftHandBend then
                            SweptAngle.smallNegative

                         else
                            SweptAngle.smallPositive
                        )
                        tangentPoint
                        joinsBendAt
                        |> Maybe.map arcToSegments
                        |> Maybe.withDefault []

                Nothing ->
                    []

        exitCurve =
            case exitInformation of
                Just { intersection, distanceAlong, tangentPoint, joinsBendAt } ->
                    Arc2d.withRadius
                        options.transitionRadius
                        (if isLeftHandBend then
                            SweptAngle.smallNegative

                         else
                            SweptAngle.smallPositive
                        )
                        joinsBendAt
                        tangentPoint
                        |> Maybe.map arcToSegments
                        |> Maybe.withDefault []

                Nothing ->
                    []

        theArcItself =
            case ( entryInformation, exitInformation ) of
                ( Just entry, Just exit ) ->
                    let
                        ( entryDirection, exitDirection ) =
                            ( Direction2d.from centreOnPlane entry.joinsBendAt
                            , Direction2d.from centreOnPlane exit.joinsBendAt
                            )

                        turn =
                            Maybe.map2 Direction2d.angleFrom entryDirection exitDirection
                    in
                    case turn of
                        Just turnAngle ->
                            Arc2d.withRadius
                                options.pushRadius
                                (if isLeftHandBend then
                                    if turnAngle |> Quantity.greaterThanOrEqualTo Quantity.zero then
                                        SweptAngle.smallPositive

                                    else
                                        SweptAngle.largePositive

                                 else if turnAngle |> Quantity.lessThanOrEqualTo Quantity.zero then
                                    SweptAngle.smallNegative

                                 else
                                    SweptAngle.largeNegative
                                )
                                entry.joinsBendAt
                                exit.joinsBendAt
                                |> Maybe.map arcToSegments
                                |> Maybe.withDefault []

                        Nothing ->
                            []

                _ ->
                    []

        prepareOriginalAltitudesForInterpolation =
            case attachmentPoints of
                Just ( start, end ) ->
                    let
                        originalSection =
                            DomainModel.extractPointsInRange
                                start
                                (skipCount track.trackTree - end)
                                track.trackTree
                                |> List.map Tuple.first

                        startDistance =
                            DomainModel.distanceFromIndex start track.trackTree

                        endDistance =
                            DomainModel.distanceFromIndex end track.trackTree

                        length =
                            endDistance |> Quantity.minus startDistance

                        altitudesByFraction =
                            -- Not the most efficient but staying close to v2.
                            List.range start end
                                |> List.map
                                    (\idx ->
                                        let
                                            thisPointDistanceFromStart =
                                                DomainModel.distanceFromIndex idx track.trackTree
                                        in
                                        ( Quantity.ratio (thisPointDistanceFromStart |> Quantity.minus startDistance)
                                            length
                                        , Point3d.zCoordinate <|
                                            .space <|
                                                DomainModel.earthPointFromIndex idx track.trackTree
                                        )
                                    )
                    in
                    altitudesByFraction

                Nothing ->
                    []

        interpolateOriginalAltitudesByDistance fraction =
            let
                twoSides =
                    prepareOriginalAltitudesForInterpolation
                        |> List.Extra.splitWhen (\( k, _ ) -> k >= fraction)
            in
            case twoSides of
                Just ( beforePairs, afterPairs ) ->
                    let
                        ( lastBefore, firstAfter ) =
                            ( List.Extra.last beforePairs, List.head afterPairs )
                    in
                    case ( lastBefore, firstAfter ) of
                        ( Just ( priorFraction, priorAltitude ), Just ( nextFraction, nextAltitude ) ) ->
                            let
                                ( beforeContribution, afterContribution ) =
                                    ( (nextFraction - fraction) / (nextFraction - priorFraction)
                                    , (fraction - priorFraction) / (nextFraction - priorFraction)
                                    )
                            in
                            Quantity.plus
                                (Quantity.multiplyBy beforeContribution priorAltitude)
                                (Quantity.multiplyBy afterContribution nextAltitude)

                        ( Just ( priorFraction, priorAltitude ), Nothing ) ->
                            -- Might happen at the end.
                            priorAltitude

                        ( Nothing, Just ( nextFraction, nextAltitude ) ) ->
                            -- Probably should not happen, but
                            nextAltitude

                        ( Nothing, Nothing ) ->
                            -- Huh?
                            Quantity.zero

                Nothing ->
                    Quantity.zero

        -- Given we interpolate using interval [0..1], we can figure out the
        -- original altitude by interpolation using this as a proportion of
        -- distance along the original section of track.
        newBendEntirely : List EarthPoint
        newBendEntirely =
            -- We (probably) have a bend, but it's flat. We need to interpolate altitudes.
            -- We can do this uniformly, or based on the original profile (by distance portion).
            --TODO: Interpolate times.
            case ( entryInformation, exitInformation ) of
                ( Just entry, Just exit ) ->
                    let
                        completeSegments =
                            []
                                ++ entryCurve
                                ++ theArcItself
                                ++ exitCurve
                                ++ [ LineSegment2d.from
                                        exit.tangentPoint
                                        (Point3d.projectInto drawingPlane exit.originalTrackPoint.space)
                                   ]

                        actualNewLength =
                            completeSegments |> List.map LineSegment2d.length |> Quantity.sum

                        altitudeChange =
                            Quantity.minus
                                (Point3d.zCoordinate entry.originalTrackPoint.space)
                                (Point3d.zCoordinate exit.originalTrackPoint.space)

                        cumulativeDistances =
                            completeSegments
                                |> List.Extra.scanl
                                    (\seg run -> Quantity.plus run (LineSegment2d.length seg))
                                    Quantity.zero

                        times =
                            entry.originalTrackPoint.time
                                :: Utils.equalIntervals
                                    (List.length completeSegments)
                                    entry.originalTrackPoint.time
                                    exit.originalTrackPoint.time
                                ++ [ exit.originalTrackPoint.time ]

                        adjustedAltitudes =
                            -- Can make this return the altitude adjusted end track point.
                            List.map3
                                (\seg dist time ->
                                    let
                                        originalSegmentStart =
                                            LineSegment2d.startPoint seg

                                        proportionalDistance =
                                            Quantity.ratio dist actualNewLength

                                        adjustment =
                                            altitudeChange |> Quantity.multiplyBy proportionalDistance

                                        newAltitude =
                                            case options.smoothGradient of
                                                Holistic ->
                                                    Point3d.zCoordinate entry.originalTrackPoint.space
                                                        |> Quantity.plus adjustment

                                                Piecewise ->
                                                    interpolateOriginalAltitudesByDistance proportionalDistance
                                    in
                                    { space =
                                        Point3d.xyz
                                            (Point2d.xCoordinate originalSegmentStart)
                                            (Point2d.yCoordinate originalSegmentStart)
                                            newAltitude
                                    , time = time
                                    }
                                )
                                (List.drop 0 completeSegments)
                                cumulativeDistances
                                times
                    in
                    adjustedAltitudes

                _ ->
                    []

        attachmentPoints =
            -- We need this to inform "Main" of where the track has changed.
            case ( entryInformation, exitInformation ) of
                ( Just entry, Just exit ) ->
                    Just
                        ( entry.index
                        , exit.index
                        )

                _ ->
                    Nothing

        previewPoints =
            case entryInformation of
                Just entry ->
                    TrackLoaded.asPreviewPoints
                        track
                        (DomainModel.distanceFromIndex entry.index track.trackTree)
                        newBendEntirely

                Nothing ->
                    []
    in
    { options
        | pointsWithinCircle = pointsWithinCircle
        , pointsWithinDisc = pointsWithinDisc
        , pointsAreContiguous = areContiguous capturedRoadSections
        , newTrackPoints = previewPoints
        , fixedAttachmentPoints = attachmentPoints
    }


areContiguous : Dict Int RoadSection -> Bool
areContiguous roads =
    let
        indices =
            Dict.keys roads
    in
    (List.length indices == 0)
        || (case ( List.maximum indices, List.minimum indices ) of
                ( Just isMax, Just isMin ) ->
                    (isMax - isMin == List.length indices - 1) && List.Extra.allDifferent indices

                _ ->
                    False
           )
