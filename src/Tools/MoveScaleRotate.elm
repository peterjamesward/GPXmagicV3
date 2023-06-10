module Tools.MoveScaleRotate exposing (Msg(..), applyMapElevations, applyRecentre, applyRotateAndScale, defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import CommonToolStyles exposing (noTrackMessage)
import Direction2d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length
import Plane3d
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.MoveScaleRotateOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2)
import Vector3d
import ViewPureStyles exposing (..)


toolId =
    "affine"


type Msg
    = RotateAndScale
    | SetRotateAngle Angle
    | Recentre
    | SetTrackLength Float
    | Zero
    | UseMapElevations


defaultOptions : Options
defaultOptions =
    { rotateAngle = Angle.degrees 0
    , desiredTrackLength = Length.kilometers 10.0
    }


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            let
                newSettings =
                    { defaultOptions | desiredTrackLength = DomainModel.trueLength theTrack.trackTree }
            in
            ( newSettings
            , actions newSettings colour theTrack
            )

        _ ->
            ( options, [ HidePreview "affine" ] )


actions : Options -> Element.Color -> TrackLoaded msg -> List (ToolAction msg)
actions options previewColour track =
    [ ShowPreview
        { tag = "affine"
        , shape = PreviewCircle
        , colour = previewColour
        , points = rotateAndScale options track
        }
    ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg settings previewColour hasTrack =
    let
        makeActions actionCode track =
            [ WithUndo actionCode
            , actionCode
            , TrackHasChanged
            ]
    in
    case ( msg, hasTrack ) of
        ( SetRotateAngle theta, Just track ) ->
            let
                newSettings =
                    { settings | rotateAngle = theta }
            in
            ( newSettings
            , actions newSettings previewColour track
            )

        ( SetTrackLength scale, Just track ) ->
            let
                newSettings =
                    { settings | desiredTrackLength = Length.kilometers scale }
            in
            ( newSettings
            , actions newSettings previewColour track
            )

        ( RotateAndScale, Just track ) ->
            ( settings
            , makeActions (ApplyRotateAndScale settings) track
            )

        ( Recentre, Just track ) ->
            ( settings
            , makeActions (ApplyRecentre track.lastMapClick) track ++ [ HidePreview "affine" ]
            )

        ( Zero, Just track ) ->
            let
                newSettings =
                    { defaultOptions | desiredTrackLength = DomainModel.trueLength track.trackTree }
            in
            ( newSettings
            , actions newSettings previewColour track
            )

        ( UseMapElevations, Just track ) ->
            -- This is problematic if the map points are elided due to quantity.
            -- "Best" option is here to force a new set of points, then
            -- do the fetch.
            ( settings
            , makeActions AddFullTrackToMapForElevations track
            )

        _ ->
            ( settings, [] )


applyMapElevations : List (Maybe Float) -> TrackLoaded msg -> TrackLoaded msg
applyMapElevations elevations track =
    -- We have previously forced a full load into the map (caveat user).
    -- So these should be in order to match up with the domain model.
    let
        useNewElevation gpx newAltitude =
            case newAltitude of
                Just altitude ->
                    { gpx | altitude = Length.meters altitude }

                Nothing ->
                    gpx

        currentPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        adjustedPoints =
            List.map2 useNewElevation currentPoints elevations
    in
    case
        DomainModel.treeFromSourcesWithExistingReference
            track.referenceLonLat
            adjustedPoints
    of
        Just isTree ->
            { track | trackTree = isTree }

        Nothing ->
            track


computeRecentredPoints : ( Float, Float ) -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeRecentredPoints ( lon, lat ) track =
    -- To allow us to use the Purple marker as a designated reference,
    -- we need to move the earth reference coords AND shift the track
    -- by the opposite of the Purple position (?).
    let
        referenceGpx =
            { longitude = Direction2d.fromAngle <| Angle.degrees lon
            , latitude = Angle.degrees lat
            , altitude = Quantity.zero
            , timestamp = Nothing
            }

        shiftFn : RoadSection -> List EarthPoint -> List EarthPoint
        shiftFn road outputs =
            shiftPoint road.endPoint :: outputs

        shiftedTrackPoints =
            shiftPoint (DomainModel.earthPointFromIndex 0 track.trackTree)
                :: DomainModel.foldOverRouteRL shiftFn track.trackTree []

        shiftBasis =
            case track.markerPosition of
                Just purple ->
                    DomainModel.earthPointFromIndex purple track.trackTree
                        |> .space
                        |> Point3d.projectOnto Plane3d.xy

                Nothing ->
                    Point3d.origin

        shiftVector =
            Vector3d.from shiftBasis Point3d.origin

        shiftPoint xyzt =
            { space = xyzt.space |> Point3d.translateBy shiftVector
            , time = xyzt.time
            }
    in
    shiftedTrackPoints
        |> List.map
            (\earth ->
                ( earth
                , DomainModel.gpxFromPointWithReference referenceGpx earth
                )
            )


rotateAndScale : Options -> TrackLoaded msg -> List PreviewPoint
rotateAndScale settings track =
    let
        centre =
            -- Scale about centre of bounding box. Note the `minZ`.
            Point3d.xyz
                (BoundingBox3d.midX <| DomainModel.boundingBox track.trackTree)
                (BoundingBox3d.midY <| DomainModel.boundingBox track.trackTree)
                (BoundingBox3d.minZ <| DomainModel.boundingBox track.trackTree)

        axisOfRotation =
            -- Rotate acts around Orange marker
            Axis3d.through
                (.space <| DomainModel.earthPointFromIndex track.currentPosition track.trackTree)
                Direction3d.z

        scaleFactor =
            Quantity.ratio
                settings.desiredTrackLength
                (DomainModel.trueLength track.trackTree)

        rotateAndScaleEndPoint : RoadSection -> List EarthPoint -> List EarthPoint
        rotateAndScaleEndPoint road outputs =
            { space =
                road.endPoint.space
                    |> Point3d.rotateAround axisOfRotation settings.rotateAngle
                    |> Point3d.scaleAbout centre scaleFactor
            , time = road.endPoint.time
            }
                :: outputs

        transformedEndPoints =
            DomainModel.foldOverRouteRL rotateAndScaleEndPoint track.trackTree []

        transformedStartPoint =
            let
                theStart =
                    DomainModel.earthPointFromIndex 0 track.trackTree
            in
            { theStart
                | space =
                    DomainModel.earthPointFromIndex 0 track.trackTree
                        |> .space
                        |> Point3d.rotateAround axisOfRotation settings.rotateAngle
                        |> Point3d.scaleAbout centre scaleFactor
            }
    in
    TrackLoaded.asPreviewPoints track (transformedStartPoint :: transformedEndPoints)


applyRotateAndScale : Options -> TrackLoaded msg -> TrackLoaded msg
applyRotateAndScale options track =
    let
        newPoints =
            List.map .gpx <|
                rotateAndScale options track
    in
    case DomainModel.treeFromSourcePoints newPoints of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
                , referenceLonLat = DomainModel.gpxPointFromIndex 0 isTree
            }

        Nothing ->
            track


applyRecentre : ( Float, Float ) -> TrackLoaded msg -> TrackLoaded msg
applyRecentre newReferenceCoords track =
    let
        newPoints =
            List.map Tuple.second <|
                computeRecentredPoints newReferenceCoords track

        ( lon, lat ) =
            track.lastMapClick

        newReferenceGPX =
            { longitude = Direction2d.fromAngle <| Angle.degrees lon
            , latitude = Angle.degrees lat
            , altitude = Quantity.zero
            , timestamp = Nothing
            }
    in
    case DomainModel.treeFromSourcePoints newPoints of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
                , currentPosition = 0
                , markerPosition = Nothing
                , referenceLonLat = newReferenceGPX
            }

        Nothing ->
            track


view :
    SystemSettings
    -> Options
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Element msg
view settings options wrapper maybeTrack =
    let
        recentreButton =
            case maybeTrack of
                Just track ->
                    let
                        ( lon, lat ) =
                            track.lastMapClick
                    in
                    button
                        (buttonStylesWithTooltip below "Click on Map to set the destination")
                        { onPress = Just <| wrapper Recentre
                        , label =
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString settings.location toolId "move")
                                    [ UtilsForViews.longitudeString <| Angle.degrees lon
                                    , UtilsForViews.latitudeString <| Angle.degrees lat
                                    ]
                        }

                Nothing ->
                    none
    in
    case maybeTrack of
        Just _ ->
            let
                i18n =
                    I18N.text settings.location toolId

                rotationSlider =
                    Input.slider
                        commonShortHorizontalSliderStyles
                        { onChange = wrapper << SetRotateAngle << Angle.degrees
                        , label =
                            Input.labelBelow [] <|
                                text <|
                                    String.Interpolate.interpolate
                                        (I18N.localisedString settings.location toolId "rotation")
                                        [ showDecimal0 <| Angle.inDegrees options.rotateAngle ]
                        , min = -30.0
                        , max = 30.0
                        , step = Just 1.0
                        , value = Angle.inDegrees <| options.rotateAngle
                        , thumb = Input.defaultThumb
                        }

                scaleSlider =
                    Input.slider
                        commonShortHorizontalSliderStyles
                        { onChange = wrapper << SetTrackLength
                        , label =
                            Input.labelBelow [] <|
                                text <|
                                    if settings.imperial then
                                        String.Interpolate.interpolate
                                            (I18N.localisedString settings.location toolId "imperial")
                                            [ showDecimal2 <| Length.inMiles options.desiredTrackLength ]

                                    else
                                        String.Interpolate.interpolate
                                            (I18N.localisedString settings.location toolId "metric")
                                            [ showDecimal2 <| Length.inKilometers options.desiredTrackLength ]
                        , min = 1.0
                        , max = 100.0
                        , step =
                            Just <|
                                if settings.imperial then
                                    Length.inKilometers <| Length.yards 17.6

                                else
                                    Length.inKilometers <| Length.meters 10
                        , value = Length.inKilometers options.desiredTrackLength
                        , thumb = Input.defaultThumb
                        }

                rotateButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper RotateAndScale
                        , label = i18n "apply"
                        }

                zeroButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper Zero
                        , label = i18n "Zero"
                        }

                elevationFetchButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper UseMapElevations
                        , label = i18n "elevations"
                        }
            in
            column
                (CommonToolStyles.toolContentBoxStyle settings)
                [ el [ centerX ] rotationSlider
                , el [ centerX ] scaleSlider
                , wrappedRow
                    [ spacing 6
                    , padding 6
                    ]
                    [ rotateButton
                    , zeroButton
                    , recentreButton
                    , elevationFetchButton
                    ]
                ]

        Nothing ->
            noTrackMessage settings
