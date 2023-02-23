module Tools.Straightener exposing (Msg(..), Options, apply, defaultOptions, toolId, update, view)

import Actions
import CommonToolStyles
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length
import LineSegment3d
import Point3d exposing (zCoordinate)
import Quantity
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded, adjustAltitude)
import ViewPureStyles exposing (neatToolsBorder)


toolId =
    "straight"


type Msg
    = StraightenStraight
    | SetPreserveAltitude Bool


type alias Options =
    { preserveAltitude : Bool }


defaultOptions : Options
defaultOptions =
    { preserveAltitude = True }


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track =
    case msg of
        StraightenStraight ->
            ( options
            , [ Actions.WithUndo Actions.Straighten
              , Actions.Straighten
              , Actions.TrackHasChanged
              ]
            )

        SetPreserveAltitude bool ->
            ( { options | preserveAltitude = bool }, [] )


view : SystemSettings -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view settings wrapper options track =
    let
        i18n =
            I18N.text settings.location toolId
    in
    column
        (CommonToolStyles.toolContentBoxStyle settings)
    <|
        if track.markerPosition /= Nothing then
            let
                straightenButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper StraightenStraight
                        , label = i18n "straight"
                        }
            in
            [ el [ centerX ] <|
                Input.checkbox []
                    { onChange = wrapper << SetPreserveAltitude
                    , icon = Input.defaultCheckbox
                    , checked = options.preserveAltitude
                    , label = Input.labelRight [] <| i18n "altitudes"
                    }
            , el [ centerX ] straightenButton
            ]

        else
            [ i18n "range" ]


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
            LineSegment3d.from startPoint.space endPoint.space

        applyAdjustment :
            RoadSection
            -> ( Length.Length, List EarthPoint )
            -> ( Length.Length, List EarthPoint )
        applyAdjustment road ( distance, outputs ) =
            let
                proportionOfTrackDistance =
                    Quantity.ratio distance trackDistance

                interpolatedPoint =
                    { space = LineSegment3d.interpolate idealLine proportionOfTrackDistance
                    , time = road.startPoint.time
                    }

                newPoint =
                    if options.preserveAltitude then
                        adjustAltitude (zCoordinate road.startPoint.space) interpolatedPoint

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


apply : Options -> TrackLoaded msg -> TrackLoaded msg
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
    in
    case newTree of
        Just isTree ->
            { track
                | trackTree = Maybe.withDefault track.trackTree newTree
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track
