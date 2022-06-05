module GpxParser exposing (..)

import Angle
import Direction2d
import DomainModel exposing (GPXSource)
import ElmEscapeHtml
import Length
import List.Extra
import Quantity
import Regex
import Spherical
import XmlParser


asRegex t =
    -- Helper to make a regex pattern.
    Maybe.withDefault Regex.never <| Regex.fromString t


parseTrackName xml =
    case Regex.find (asRegex "<name>(.*)<\\/name>") xml of
        [] ->
            Nothing

        x :: _ ->
            case x.submatches of
                [] ->
                    Nothing

                n :: _ ->
                    Maybe.map ElmEscapeHtml.unescape n


parseGPX : String -> List ( Maybe String, List GPXSource )
parseGPX xml =
    -- This will become our new entry point and works at the <trkseg> level so they can be named.
    let
        rgtNamespace =
            --TODO: Look through attributes of "gpx" element.
            Maybe.withDefault "rgt" <|
                case
                    Regex.find
                        (asRegex
                            "xmlns:(.*)=\\\"http:\\/\\/www\\.rgtcycling\\.com\\/XML\\/GpxExtensions"
                        )
                        xml
                of
                    match :: _ ->
                        case match.submatches of
                            n :: _ ->
                                n

                            [] ->
                                Nothing

                    [] ->
                        Nothing

    in
    case XmlParser.parse xml of
        Ok parseTree ->
            parseSegments parseTree.root

        Err deadEnds ->
            []


parseSegments : XmlParser.Node -> List ( Maybe String, List GPXSource )
parseSegments node =
    let
        collectSegments :
            XmlParser.Node
            -> List ( Maybe String, List GPXSource )
            -> List ( Maybe String, List GPXSource )
        collectSegments child collection =
            case child of
                XmlParser.Element "trkseg" attributes children ->
                    parseOneSegment children collection

                _ ->
                    collection
    in
    case node of
        XmlParser.Element "gpx" attributes children ->
            children
                |> childrenFromFirstTag "trk"
                |> List.foldl collectSegments []
                |> List.reverse

        _ ->
            []


parseOneSegment :
    List XmlParser.Node
    -> List ( Maybe String, List GPXSource )
    -> List ( Maybe String, List GPXSource )
parseOneSegment nodes collection =
    let
        collectTrackPoints :
            XmlParser.Node
            -> ( Maybe String, List GPXSource )
            -> ( Maybe String, List GPXSource )
        collectTrackPoints child ( segName, pointCollection ) =
            case child of
                XmlParser.Element "trkpt" attributes children ->
                    ( segName, parseTrackPoint attributes children pointCollection )

                XmlParser.Element "extensions" attributes children ->
                    ( getSegmentName children, pointCollection )

                _ ->
                    ( segName, pointCollection )

        getSegmentName : List XmlParser.Node -> Maybe String
        getSegmentName optionals =
            case childrenFromFirstTag "rgt:namedSegment" optionals of
                (XmlParser.Text txt) :: _ ->
                    Just txt

                _ ->
                    Nothing

        ( segmentName, segmentPoints ) =
            nodes |> List.foldl collectTrackPoints ( Nothing, [] )
    in
    ( segmentName, List.reverse segmentPoints ) :: collection


childrenFromFirstTag : String -> List XmlParser.Node -> List XmlParser.Node
childrenFromFirstTag tag nodes =
    case nodes of
        (XmlParser.Element childTag attributes children) :: moreNodes ->
            if childTag == tag then
                children

            else
                childrenFromFirstTag tag moreNodes

        _ :: moreNodes ->
            childrenFromFirstTag tag moreNodes

        [] ->
            []


parseTrackPoint : List XmlParser.Attribute -> List XmlParser.Node -> List GPXSource -> List GPXSource
parseTrackPoint attributes children outputs =
    let
        latitude =
            List.Extra.find (\attr -> attr.name == "lat") attributes
                |> Maybe.map .value
                |> Maybe.andThen String.toFloat

        longitude =
            List.Extra.find (\attr -> attr.name == "lon") attributes
                |> Maybe.map .value
                |> Maybe.andThen String.toFloat

        elevation =
            childrenFromFirstTag "ele" children
                |> List.head
                |> Maybe.andThen numberFromXml
    in
    case ( latitude, longitude, elevation ) of
        ( Just lat, Just lon, Just alt ) ->
            GPXSource
                (Direction2d.fromAngle <| Angle.degrees lon)
                (Angle.degrees lat)
                (Length.meters alt)
                :: outputs

        ( Just lat, Just lon, _ ) ->
            GPXSource
                (Direction2d.fromAngle <| Angle.degrees lon)
                (Angle.degrees lat)
                (Length.meters 0)
                :: outputs

        _ ->
            outputs


numberFromXml : XmlParser.Node -> Maybe Float
numberFromXml node =
    case node of
        XmlParser.Text txt ->
            String.toFloat txt

        XmlParser.Element tag attrs children ->
            Nothing
