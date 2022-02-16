module Tools.MoveScaleRotate exposing (..)

import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Direction3d
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length exposing (Meters, inMeters)
import List.Extra
import Plane3d
import Point3d
import Vector3d
import ViewPureStyles exposing (..)


type Msg
    = RotateAndScale
    | SetRotateAngle Angle
    | Recentre
    | SetScale Float
    | Zero
    | UseMapElevations


type alias Options =
    { rotateAngle : Angle
    , scaleFactor : Float
    }


defaultOptions : Options
defaultOptions =
    { rotateAngle = Angle.degrees 0
    , scaleFactor = 1.0
    }


update :
    Msg
    -> Options
    -> ( Float, Float )
    -> Track
    -> ( Options, PostUpdateActions.PostUpdateAction trck cmd )
update msg settings lastMapClick track =
    case msg of
        SetRotateAngle theta ->
            ( { settings | rotateAngle = theta }
            , PostUpdateActions.ActionPreview
            )

        SetScale scale ->
            ( { settings | scaleFactor = scale }
            , PostUpdateActions.ActionPreview
            )

        RotateAndScale ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesIndex
                (buildRotateAndScale settings track)
            )

        Recentre ->
            ( settings
            , PostUpdateActions.ActionTrackChanged
                TrackEditType.EditPreservesIndex
                (buildRecentre settings lastMapClick track)
            )

        Zero ->
            ( defaultOptions
            , PostUpdateActions.ActionPreview
            )

        UseMapElevations ->
            ( settings
            , PostUpdateActions.ActionFetchMapElevations
            )


applyMapElevations : List Float -> Track -> EditResult
applyMapElevations elevations track =
    let
        useNewElevation tp ele =
            Point3d.xyz
                (Point3d.xCoordinate tp.xyz)
                (Point3d.yCoordinate tp.xyz)
                (Length.meters ele)
                |> TrackPoint.trackPointFromPoint

        newPoints =
            List.map2
                useNewElevation
                track.trackPoints
                elevations
                |> TrackPoint.prepareTrackPoints
    in
    { before = []
    , edited = newPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


recentre : ( Float, Float ) -> Track -> EditResult
recentre ( lon, lat ) track =
    -- To allow us to use the Purple marker as a designated reference,
    -- we need to move the earth reference coords AND shift the track
    -- by the opposite of the Purple position (?).
    let
        shiftedTrackPoints =
            List.map shiftPoint track.trackPoints

        shiftBasis =
            case track.markedNode of
                Just purple ->
                    purple.xyz |> Point3d.projectOnto Plane3d.xy

                Nothing ->
                    Point3d.origin

        shiftVector =
            Vector3d.from shiftBasis Point3d.origin

        shiftPoint =
            .xyz
                >> Point3d.translateBy shiftVector
                >> trackPointFromPoint
    in
    { before = []
    , edited = shiftedTrackPoints
    , after = []
    , earthReferenceCoordinates = ( lon, lat, 0.0 )
    , graph = track.graph
    }


rotateAndScale : Options -> Track -> EditResult
rotateAndScale settings track =
    let
        centre =
            -- Scale about centre of bounding box
            Point3d.xyz
                (BoundingBox3d.midX track.box)
                (BoundingBox3d.midY track.box)
                (BoundingBox3d.minZ track.box)

        axisOfRotation =
            -- Rotate acts around Orange marker
            Axis3d.through track.currentNode.xyz Direction3d.z

        rotatedRoute =
            List.map rotatePoint track.trackPoints

        rotatePoint =
            .xyz
                >> Point3d.rotateAround axisOfRotation settings.rotateAngle
                >> trackPointFromPoint

        scaleAboutCentre point =
            centre
                |> Point3d.translateBy
                    (point |> Vector3d.from centre |> Vector3d.scaleBy settings.scaleFactor)

        transformedPoints =
            List.map
                (.xyz >> scaleAboutCentre >> trackPointFromPoint)
                rotatedRoute
    in
    { before = []
    , edited = transformedPoints
    , after = []
    , earthReferenceCoordinates = track.earthReferenceCoordinates
    , graph = track.graph
    }


view : Bool -> Options -> ( Float, Float ) -> (Msg -> msg) -> Track -> Element msg
view imperial options ( lastX, lastY ) wrapper track =
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
                { onChange = wrapper << SetScale << (\x -> 10.0 ^ x)
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Scale: "
                                ++ showDecimal2 options.scaleFactor
                , min = -1.0
                , max = 1.0
                , step = Nothing
                , value = logBase 10 options.scaleFactor
                , thumb = Input.defaultThumb
                }

        ( lon, lat, _ ) =
            track.earthReferenceCoordinates

        trackLength =
            case List.Extra.last track.trackPoints of
                Just last ->
                    inMeters last.distanceFromStart

                _ ->
                    0.0

        rotateButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper RotateAndScale
                , label =
                    text <|
                        "Rotate & Scale"
                }

        recentreButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper Recentre
                , label =
                    text <|
                        "Recentre at\n("
                            ++ String.fromFloat lastX
                            ++ ", "
                            ++ String.fromFloat lastY
                            ++ ")"
                }

        zeroButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper Zero
                , label = text "Zero"
                }

        elevationFetchButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrapper UseMapElevations
                , label = text "Use elevations fetched from Mapbox"
                }
    in
    wrappedRow [ spacing 5, padding 5 ]
        [ rotationSlider
        , scaleSlider
        , rotateButton
        , zeroButton
        , recentreButton
        , elevationFetchButton
        ]
