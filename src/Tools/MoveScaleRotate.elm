module Tools.MoveScaleRotate exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Dict exposing (Dict)
import Direction2d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters)
import Plane3d
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.MoveScaleRotateOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure)
import Vector3d
import ViewPureStyles exposing (..)


type Msg
    = RotateAndScale
    | SetRotateAngle Angle
    | Recentre
    | SetTrackLength Float
    | Zero
    | UseMapElevations
    | DisplayInfo String String


toolID : String
toolID =
    "affine"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Move,Scale,Rotate" )
        , ( "info", infoText )
        ]
    )


infoText =
    """Want to ride Ventoux in the Sahara? Want your local loop to be a bit longer, or a lot?

Move, Scale & Rotate lets you perform some simple transformations on the whole route. It's maths.

Scale and Rotate are fairly obvious but Move requires you to use the Map view to identify where
you want your route. The coordinates of the last map click are displayed in the tool. Without the
Purple marker, the centre point of the route is moved to the last map click position. With the
Purple marker, the Purple marker is moved there; this can give you more control over placement.

This tool is also useful if you import an SVG drawing, as these will appear off the coast
of Ghana by default (zero longitude, zero latitude), and certainly will not be the right length.
"""


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
            ( options
            , actions options colour theTrack
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
            , [ ApplyRotateAndScale settings, TrackHasChanged ]
            )

        ( Recentre, Just track ) ->
            ( settings
            , [ ApplyRecentre track.lastMapClick, TrackHasChanged, HidePreview "affine" ]
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
            , [ AddFullTrackToMap ]
            )

        _ ->
            ( settings, [] )


applyMapElevations : List (Maybe Float) -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
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
    ( DomainModel.treeFromSourcesWithExistingReference
        track.referenceLonLat
        adjustedPoints
    , currentPoints
    )


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
                        |> Point3d.projectOnto Plane3d.xy

                Nothing ->
                    Point3d.origin

        shiftVector =
            Vector3d.from shiftBasis Point3d.origin

        shiftPoint =
            Point3d.translateBy shiftVector
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
                (DomainModel.earthPointFromIndex track.currentPosition track.trackTree)
                Direction3d.z

        scaleFactor =
            Quantity.ratio
                settings.desiredTrackLength
                (DomainModel.trueLength track.trackTree)

        rotateAndScaleEndPoint : RoadSection -> List EarthPoint -> List EarthPoint
        rotateAndScaleEndPoint road outputs =
            (road.endPoint
                |> Point3d.rotateAround axisOfRotation settings.rotateAngle
                |> Point3d.scaleAbout centre scaleFactor
            )
                :: outputs

        transformedEndPoints =
            DomainModel.foldOverRouteRL rotateAndScaleEndPoint track.trackTree []

        transformedStartPoint =
            DomainModel.earthPointFromIndex 0 track.trackTree
                |> Point3d.rotateAround axisOfRotation settings.rotateAngle
                |> Point3d.scaleAbout centre scaleFactor
    in
    TrackLoaded.asPreviewPoints track
        Quantity.zero
        (transformedStartPoint :: transformedEndPoints)


applyRotateAndScale : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyRotateAndScale options track =
    let
        newPoints =
            rotateAndScale options track
    in
    ( DomainModel.treeFromSourcePoints <| List.map .gpx newPoints
    , DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    )


applyRecentre : ( Float, Float ) -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyRecentre newReference track =
    let
        newPoints =
            computeRecentredPoints newReference track
    in
    ( DomainModel.treeFromSourcePoints <| List.map Tuple.second newPoints
    , DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    )


view :
    Bool
    -> Options
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Element msg
view imperial options wrapper maybeTrack =
    let
        rotationSlider =
            Input.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetRotateAngle << Angle.degrees
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Rotation: "
                                ++ (showDecimal0 <| Angle.inDegrees options.rotateAngle)
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
                            "Length: "
                                ++ (if imperial then
                                        showDecimal0
                                            (Length.inMiles
                                                options.desiredTrackLength
                                            )
                                            ++ "mi"

                                    else
                                        showDecimal0
                                            (Length.inKilometers
                                                options.desiredTrackLength
                                            )
                                            ++ "km"
                                   )
                , min = 1.0
                , max = 100.0
                , step = Just 1.0
                , value = Length.inKilometers options.desiredTrackLength
                , thumb = Input.defaultThumb
                }

        rotateButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper RotateAndScale
                , label =
                    text <|
                        "Rotate & Scale"
                }

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
                            paragraph [ width fill ]
                                [ text <|
                                    "Move to "
                                        ++ String.fromFloat lon
                                        ++ ", "
                                        ++ String.fromFloat lat
                                ]
                        }

                Nothing ->
                    none

        zeroButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Zero
                , label = text "Zero"
                }

        elevationFetchButton =
            button
                (buttonStylesWithTooltip below "First, tilt the Map view to get elevation data")
                { onPress = Just <| wrapper UseMapElevations
                , label = text "Use elevations fetched from Mapbox"
                }
    in
    case maybeTrack of
        Just track ->
            column
                [ spacing 6
                , padding 6
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                , width fill
                ]
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
            noTrackMessage
