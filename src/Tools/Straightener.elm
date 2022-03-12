module Tools.Straightener exposing (..)

import Actions
import Axis3d
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length
import LineSegment3d
import Point3d exposing (zCoordinate)
import Quantity
import TrackLoaded exposing (TrackLoaded, adjustAltitude)
import ViewPureStyles exposing (neatToolsBorder, prettyButtonStyles)


type Msg
    = StraightenStraight
    | SetPreserveAltitude Bool
    | DisplayInfo String String


toolID : String
toolID =
    "straight"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Straightener" )
        , ( "info", infoText )
        ]
    )


infoText =
    """Sometimes you just want a straight to be straight and it's tedious to get rid of
all the wriggles. Sure, you could delete some of the points, but you might want to
keep some altitude changes.
Straightener is simple and single minded. It takes all the points in the range and lines
them up. It will either retain their altitudes or impose a constant gradient. Note that
keeping the altitudes and squishing the points up increases gradients.
"""


type alias Options =
    { preserveAltitude : Bool }


defaultOptions : Options
defaultOptions =
    { preserveAltitude = True }


update :
    Msg
    -> Options
    -> ( Options, List (Actions.ToolAction msg) )
update msg options =
    case msg of
        StraightenStraight ->
            ( options, [ Actions.Straighten ] )

        SetPreserveAltitude bool ->
            ( { options | preserveAltitude = bool }, [] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


view : (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view wrapper options track =
    let
        straightenButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper StraightenStraight
                , label =
                    text <|
                        "Straighten between markers"
                }
    in
    column
        [ spacing 10
        , padding 10
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        if track.markerPosition /= Nothing then
            [ el [ centerX ] <|
                Input.checkbox []
                    { onChange = wrapper << SetPreserveAltitude
                    , icon = Input.defaultCheckbox
                    , checked = options.preserveAltitude
                    , label = Input.labelRight [] <| text "Preserve altitudes"
                    }
            , el [ centerX ] straightenButton
            ]

        else
            [ paragraph [ padding 10, centerX ]
                [ text "The straighten tool requires a range. "
                , text "Drop the marker and move it away from the current pointer."
                ]
            ]


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        endIndex =
            DomainModel.skipCount track.trackTree - fromEnd

        ( startPoint, endPoint ) =
            ( DomainModel.earthPointFromIndex fromStart track.trackTree
            , DomainModel.earthPointFromIndex endIndex track.trackTree
            )

        ( startDistance, endDistance ) =
            ( DomainModel.distanceFromIndex fromStart track.trackTree
            , DomainModel.distanceFromIndex endIndex track.trackTree
            )

        trackDistance =
            endDistance |> Quantity.minus startDistance

        idealLine =
            LineSegment3d.from startPoint endPoint

        straightLineDistance =
            LineSegment3d.length idealLine

        applyAdjustment :
            RoadSection
            -> ( Length.Length, List EarthPoint )
            -> ( Length.Length, List EarthPoint )
        applyAdjustment road ( distance, outputs ) =
            let
                proportionOfTrackDistance =
                    Quantity.ratio distance trackDistance

                interpolatedPoint =
                    LineSegment3d.interpolate idealLine proportionOfTrackDistance

                newPoint =
                    if options.preserveAltitude then
                        adjustAltitude (zCoordinate road.startPoint) interpolatedPoint

                    else
                        interpolatedPoint
            in
            ( distance |> Quantity.plus road.trueLength
            , newPoint :: outputs
            )

        ( _, adjustedPoints ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                fromStart
                endIndex
                (always Nothing)
                0
                track.trackTree
                applyAdjustment
                ( Quantity.zero, [] )
    in
    adjustedPoints
        |> List.reverse
        |> List.map
            (\earth ->
                ( earth
                , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                )
            )


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newPoints =
            computeNewPoints options track

        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                (List.map Tuple.second newPoints)
                track.trackTree

        oldPoints =
            DomainModel.extractPointsInRange
                fromStart
                fromEnd
                track.trackTree
    in
    ( newTree
    , oldPoints |> List.map Tuple.second
    )
