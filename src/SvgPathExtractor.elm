module SvgPathExtractor exposing (Msg(..), Options, PathAndTransform, PathInfo, PathState, defaultOptions, trackFromSvg, update, view)

import Actions exposing (ToolAction(..))
import AltMath.Matrix4 as AltMath
import AltMath.Vector3
import Angle
import CubicSpline3d
import Direction2d
import DomainModel
import Element exposing (Element, padding, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import GeoCodeDecoders
import GpxParser exposing (asRegex)
import Length exposing (Meters, inMeters, meters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Path.LowLevel exposing (DrawTo(..), Mode(..), MoveTo(..), SubPath)
import Path.LowLevel.Parser as PathParser
import Plane3d
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Quantity(..))
import Regex
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import XmlParser exposing (Node(..))


type alias Options =
    { svgFilename : String
    , ipInfo : Maybe GeoCodeDecoders.IpInfo
    }


defaultOptions : Options
defaultOptions =
    { svgFilename = "SVG"
    , ipInfo = Nothing
    }


type Msg
    = ReadFile (Maybe GeoCodeDecoders.IpInfo)
    | FileSelected File
    | FileLoaded String


update : Msg -> Options -> (Msg -> msg) -> ( Options, List (ToolAction msg) )
update msg options wrap =
    case msg of
        ReadFile ipInfo ->
            let
                newOptions =
                    { options | ipInfo = ipInfo }
            in
            ( newOptions
            , [ SelectSvgFile (wrap << FileSelected) ]
              --Select.file [ "text/svg" ] (wrap << FileSelected)
            )

        FileSelected file ->
            ( options
            , [ LoadSvgFile (wrap << FileLoaded) file ]
              --, Task.perform (wrap << FileLoaded) (File.toString file)
            )

        FileLoaded content ->
            ( options
            , [ TrackFromSvg content, TrackHasChanged ]
              --, Task.perform (wrap << FileLoaded) (File.toString file)
            )


trackFromSvg : Options -> String -> Maybe (TrackLoaded msg)
trackFromSvg options content =
    processXML options content


type alias PathInfo =
    { d : Maybe String
    , transform : Maybe String
    }


type alias PathAndTransform =
    { subpaths : List SubPath
    , transform : AltMath.Mat4
    }


identityTransform =
    AltMath.identity


parseTransform : String -> AltMath.Mat4
parseTransform text =
    let
        value x =
            case x of
                Just val ->
                    String.toFloat val

                _ ->
                    Nothing

        hasScale =
            --TODO: Optional y scale value!
            text
                |> Regex.find (asRegex "scale\\((-?\\d*\\.?\\d*)\\)")
                |> List.concatMap .submatches
                |> List.filterMap value
                |> List.head

        hasMatrix =
            text
                |> Regex.find (asRegex "matrix\\((-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*),(-?\\d*\\.?\\d*)\\)")
                |> List.concatMap .submatches
                |> List.filterMap value

        applyScale baseMatrix =
            case hasScale of
                Just scale ->
                    baseMatrix |> AltMath.scale3 scale scale 1.0

                Nothing ->
                    baseMatrix

        applyMatrix baseMatrix =
            case hasMatrix of
                [ a, b, c, d, e, f ] ->
                    let
                        matrix =
                            -- Careful, it's a 4x4.
                            { identityTransform
                                | m11 = a
                                , m21 = b
                                , m12 = c
                                , m22 = d
                                , m14 = e
                                , m24 = f
                            }
                    in
                    baseMatrix |> AltMath.mul matrix

                _ ->
                    baseMatrix
    in
    identityTransform
        |> applyMatrix
        |> applyScale


parsePathInfo : PathInfo -> PathAndTransform
parsePathInfo { d, transform } =
    let
        parsedPath =
            case Maybe.map PathParser.parse d |> Maybe.withDefault (Err []) of
                Ok subpaths ->
                    subpaths

                Err _ ->
                    []

        parsedTransform =
            transform
                |> Maybe.map parseTransform
                |> Maybe.withDefault identityTransform
    in
    { subpaths = parsedPath
    , transform = parsedTransform
    }


processXML : Options -> String -> Maybe (TrackLoaded msg)
processXML options content =
    let
        xmlParse =
            XmlParser.parse content

        ( lon, lat ) =
            case options.ipInfo of
                Just ipInfo ->
                    ( ipInfo.longitude, ipInfo.latitude )

                Nothing ->
                    ( 0, 52 )
    in
    case xmlParse of
        Ok { root } ->
            case root of
                XmlParser.Element _ _ _ ->
                    let
                        pathNodes =
                            root
                                |> getAllXmlTags
                                |> List.reverse
                                |> List.filter (\( t, _ ) -> t == "path")

                        pathInfos : List PathInfo
                        pathInfos =
                            pathNodes
                                |> List.map
                                    (\( _, node ) ->
                                        { d = node |> getAttribute "d"
                                        , transform = node |> getAttribute "transform"
                                        }
                                    )

                        untransformedPaths : List PathAndTransform
                        untransformedPaths =
                            pathInfos |> List.map parsePathInfo

                        pathState : PathState
                        pathState =
                            { startPoint = Point3d.origin
                            , currentPoint = Point3d.origin
                            , outputs = []
                            }

                        finalPathState =
                            -- Now back where we were, add in the transforms.
                            untransformedPaths
                                |> List.foldl convertToPoints pathState

                        pointZero =
                            { longitude = Direction2d.fromAngle <| Angle.degrees lon
                            , latitude = Angle.degrees lat
                            , altitude = Length.meters 0
                            , timestamp = Nothing
                            }

                        gpxPoints =
                            finalPathState.outputs
                                |> List.map
                                    (DomainModel.withoutTime
                                        >> DomainModel.gpxFromPointWithReference pointZero
                                    )
                    in
                    TrackLoaded.trackFromPoints
                        options.svgFilename
                        gpxPoints

                _ ->
                    Nothing

        Err _ ->
            Nothing


getAllXmlTags : XmlParser.Node -> List ( String, XmlParser.Node )
getAllXmlTags node =
    case node of
        XmlParser.Element tag _ children ->
            ( tag, node )
                :: List.concatMap getAllXmlTags children

        Text _ ->
            []


getAttribute : String -> XmlParser.Node -> Maybe String
getAttribute attribute node =
    case node of
        XmlParser.Element _ attributes _ ->
            attributes
                |> List.Extra.find
                    (\{ name } -> name == attribute)
                |> Maybe.map .value

        Text _ ->
            Nothing


view : (Msg -> msg) -> Maybe GeoCodeDecoders.IpInfo -> Element msg
view wrap ipInfo =
    Input.button
        [ padding 5
        , Border.color FlatColors.FlatUIPalette.peterRiver
        , Background.color FlatColors.FlatUIPalette.clouds
        , Font.color FlatColors.FlatUIPalette.peterRiver
        , Border.width 2
        , Border.rounded 4
        ]
        { onPress = Just (wrap <| ReadFile ipInfo)
        , label = text "Extract paths from SVG file"
        }


type alias PathState =
    { startPoint : Point3d Meters LocalCoords
    , currentPoint : Point3d Meters LocalCoords
    , outputs : List (Point3d Meters LocalCoords)
    }


convertToPoints : PathAndTransform -> PathState -> PathState
convertToPoints pathAndTransform pathState =
    -- We now fold this over the paths so need to track current point externally.
    let
        applyTransform : Point3d Meters LocalCoords -> Point3d Meters LocalCoords
        applyTransform before =
            before
                |> Point3d.toRecord inMeters
                |> AltMath.Vector3.fromRecord
                |> AltMath.transform pathAndTransform.transform
                |> AltMath.Vector3.toRecord
                |> Point3d.fromRecord meters

        localPathState =
            { pathState | outputs = [] }

        newLocalPathState =
            List.foldl followSubPath localPathState pathAndTransform.subpaths

        pointsFromThisPath =
            newLocalPathState
                |> .outputs
                |> List.map applyTransform
                |> List.map flipY
    in
    { pathState
        | outputs = pathState.outputs ++ pointsFromThisPath
    }


flipY : Point3d Meters LocalCoords -> Point3d Meters LocalCoords
flipY =
    Point3d.mirrorAcross Plane3d.zx


followSubPath : SubPath -> PathState -> PathState
followSubPath sub state =
    let
        newPoint =
            case sub.moveto of
                MoveTo Absolute ( x, y ) ->
                    Point3d.meters x y 0.0

                MoveTo Relative ( dx, dy ) ->
                    state.currentPoint
                        |> Point3d.translateBy (Vector3d.meters dx dy 0.0)

        subPathState =
            { state
                | currentPoint = newPoint
                , startPoint = newPoint
                , outputs = [ newPoint ]
            }

        endSubPathState =
            List.foldl drawCommand subPathState sub.drawtos
    in
    { state
        | currentPoint = endSubPathState.currentPoint
        , outputs = state.outputs ++ List.reverse endSubPathState.outputs
    }


drawCommand : DrawTo -> PathState -> PathState
drawCommand command state =
    --let
    --    _ =
    --        Debug.log "Command" command
    --in
    case command of
        LineTo Absolute points ->
            let
                initialState =
                    -- Current point and list of outputs
                    ( state.currentPoint, [] )

                finalState =
                    List.foldl absoluteLine initialState points

                absoluteLine ( x, y ) ( lastPoint, outputs ) =
                    let
                        nextPoint =
                            Point3d.meters x y 0.0
                    in
                    ( nextPoint, nextPoint :: outputs )
            in
            { state
                | currentPoint = Tuple.first finalState
                , outputs = Tuple.second finalState ++ state.outputs
            }

        LineTo Relative points ->
            -- This is a fold over the list of points.
            let
                initialState =
                    -- Current point and list of outputs
                    ( state.currentPoint, [] )

                finalState =
                    List.foldl relativeLine initialState points

                relativeLine ( dx, dy ) ( lastPoint, outputs ) =
                    let
                        nextPoint =
                            lastPoint |> Point3d.translateBy (Vector3d.meters dx dy 0.0)
                    in
                    ( nextPoint, nextPoint :: outputs )
            in
            { state
                | currentPoint = Tuple.first finalState
                , outputs = Tuple.second finalState ++ state.outputs
            }

        Horizontal Absolute xs ->
            let
                pairs =
                    xs |> List.map (\x -> ( x, 0 ))
            in
            drawCommand (LineTo Absolute pairs) state

        Horizontal Relative dxs ->
            let
                pairs =
                    dxs |> List.map (\dx -> ( dx, 0 ))
            in
            drawCommand (LineTo Relative pairs) state

        Vertical Absolute ys ->
            let
                pairs =
                    ys |> List.map (\y -> ( 0, y ))
            in
            drawCommand (LineTo Absolute pairs) state

        Vertical Relative dys ->
            let
                pairs =
                    dys |> List.map (\dy -> ( 0, dy ))
            in
            drawCommand (LineTo Relative pairs) state

        CurveTo Relative triples ->
            let
                curveRelativeInitialState =
                    ( state.currentPoint, [] )

                curveRelativeFinalState =
                    List.foldl curveRelative curveRelativeInitialState triples

                curveRelative ( ( dx1, dy1 ), ( dx2, dy2 ), ( dxN, dyN ) ) ( lastPoint, outputs ) =
                    let
                        ( c1, c2, cN ) =
                            ( lastPoint |> Point3d.translateBy (Vector3d.meters dx1 dy1 0.0)
                            , lastPoint |> Point3d.translateBy (Vector3d.meters dx2 dy2 0.0)
                            , lastPoint |> Point3d.translateBy (Vector3d.meters dxN dyN 0.0)
                            )

                        spline =
                            CubicSpline3d.fromControlPoints lastPoint c1 c2 cN

                        splinePoints =
                            spline
                                |> CubicSpline3d.approximate (Quantity 1.0)
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                                |> List.reverse
                    in
                    ( cN, splinePoints ++ outputs )
            in
            { state
                | currentPoint = Tuple.first curveRelativeFinalState
                , outputs =
                    Tuple.second curveRelativeFinalState
                        ++ state.outputs
            }

        CurveTo Absolute triples ->
            let
                curveAbsoluteInitialState =
                    ( state.currentPoint, [] )

                curveAbsoluteFinalState =
                    List.foldl curveAbsolute curveAbsoluteInitialState triples

                curveAbsolute ( ( dx1, dy1 ), ( dx2, dy2 ), ( dxN, dyN ) ) ( lastPoint, outputs ) =
                    let
                        ( c1, c2, cN ) =
                            ( Point3d.meters dx1 dy1 0.0
                            , Point3d.meters dx2 dy2 0.0
                            , Point3d.meters dxN dyN 0.0
                            )

                        spline =
                            -- Should we be using lastPoint or currentPoint??
                            CubicSpline3d.fromControlPoints lastPoint c1 c2 cN

                        splinePoints =
                            spline
                                |> CubicSpline3d.approximate (Quantity 1.0)
                                |> Polyline3d.segments
                                |> List.map LineSegment3d.endPoint
                                |> List.reverse
                    in
                    ( cN, splinePoints ++ outputs )
            in
            { state
                | currentPoint = Tuple.first curveAbsoluteFinalState
                , outputs =
                    Tuple.second curveAbsoluteFinalState
                        ++ state.outputs
            }

        ClosePath ->
            { state
                | currentPoint = state.startPoint
                , outputs = state.startPoint :: state.outputs
            }

        _ ->
            state
