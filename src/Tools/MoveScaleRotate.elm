module Tools.MoveScaleRotate exposing (..)

import Actions exposing (PreviewShape(..), ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters)
import Point3d
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.MoveScaleRotateOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2)
import ViewPureStyles exposing (..)


type Msg
    = RotateAndScale
    | SetRotateAngle Angle
    | Recentre
    | SetScale Float
    | Zero
    | UseMapElevations


defaultOptions : Options
defaultOptions =
    { rotateAngle = Angle.degrees 0
    , scaleFactor = 1.0
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

        ( SetScale scale, Just track ) ->
            let
                newSettings =
                    { settings | scaleFactor = scale }
            in
            ( newSettings
            , actions newSettings previewColour track
            )

        ( RotateAndScale, Just track ) ->
            let
                newSettings =
                    defaultOptions
            in
            ( newSettings
            , [ ApplyRotateAndScale settings, TrackHasChanged ]
            )

        ( Recentre, Just track ) ->
            let
                newSettings =
                    settings
            in
            ( newSettings
            , actions newSettings previewColour track
            )

        ( Zero, Just track ) ->
            ( defaultOptions
            , actions defaultOptions previewColour track
            )

        ( UseMapElevations, Just track ) ->
            let
                newSettings =
                    settings
            in
            ( newSettings
            , actions newSettings previewColour track
            )

        _ ->
            ( settings, [] )


applyMapElevations : List Float -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
applyMapElevations elevations track =
    let
        useNewElevation tp ele =
            Point3d.xyz
                (Point3d.xCoordinate tp)
                (Point3d.yCoordinate tp)
                (Length.meters ele)
    in
    []



{-
   recentre : ( Float, Float ) -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
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
               Point3d.translateBy shiftVector
       in
       []
-}


rotateAndScale : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
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

        rotateAndScaleEndPoint : RoadSection -> List EarthPoint -> List EarthPoint
        rotateAndScaleEndPoint road outputs =
            (road.endPoint
                |> Point3d.rotateAround axisOfRotation settings.rotateAngle
                |> Point3d.scaleAbout centre settings.scaleFactor
            )
                :: outputs

        transformedEndPoints =
            DomainModel.foldOverRouteRL rotateAndScaleEndPoint track.trackTree []

        transformedStartPoint =
            DomainModel.earthPointFromIndex 0 track.trackTree
                |> Point3d.rotateAround axisOfRotation settings.rotateAngle
                |> Point3d.scaleAbout centre settings.scaleFactor
    in
    List.map
        (\earth ->
            ( earth
            , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
            )
        )
        (transformedStartPoint :: transformedEndPoints)


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        newPoints =
            rotateAndScale options track
    in
    ( DomainModel.treeFromSourcePoints <| List.map Tuple.second newPoints
    , DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    )


view :
    Bool
    -> Options
    -> ( Float, Float )
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Element msg
view imperial options ( lastX, lastY ) wrapper maybeTrack =
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

        rotateButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper RotateAndScale
                , label =
                    text <|
                        "Rotate & Scale"
                }

        recentreButton =
            button
                (buttonStylesWithTooltip below "Click on Map to set the destination")
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
                neatToolsBorder
                { onPress = Just <| wrapper Zero
                , label = text "Zero"
                }

        elevationFetchButton =
            button
                neatToolsBorder
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
