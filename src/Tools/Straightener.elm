module Tools.Straightener exposing (..)

import Actions
import Axis3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Point3d
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (neatToolsBorder, prettyButtonStyles)


type Msg
    = StraightenStraight
    | SetPreservwAltitude Bool


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

        SetPreservwAltitude bool ->
            ( { options | preserveAltitude = bool }, [] )


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
                    { onChange = wrapper << SetPreservwAltitude
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

        idealLine =
            Axis3d.throughPoints startPoint endPoint

        applyAdjustment : RoadSection -> List EarthPoint -> List EarthPoint
        applyAdjustment road outputs =
            let
                newPoint =
                    case idealLine of
                        Just axis ->
                            if options.preserveAltitude then
                                TrackLoaded.adjustAltitude
                                    (Point3d.zCoordinate road.startPoint)
                                    (Point3d.projectOntoAxis axis road.startPoint)

                            else
                                Point3d.projectOntoAxis axis road.startPoint

                        Nothing ->
                            road.startPoint
            in
            newPoint :: outputs

        adjustedPoints =
            List.reverse <|
                List.drop 1 <|
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        fromStart
                        endIndex
                        (always Nothing)
                        0
                        track.trackTree
                        applyAdjustment
                        []
    in
    adjustedPoints
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
