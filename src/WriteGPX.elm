module WriteGPX exposing (writeGPX)

import Angle
import Direction2d
import DomainModel exposing (GPXSource)
import ElmEscapeHtml
import Length
import Tools.NamedSegmentOptions exposing (NamedSegment)
import TrackLoaded exposing (TrackLoaded)


preamble =
    """<?xml version='1.0' encoding='UTF-8'?>
<gpx version="1.1"
  creator="GPXmagic"
  xmlns="http://www.topografix.com/GPX/1/1"
  xmlns:rgt="http://www.rgtcycling.com/XML/GpxExtensions/v1">
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.topografix.com/GPX/1/1
  http://www.topografix.com/GPX/1/1/gpx.xsd">
  <metadata>
    <name>Smoothed with GPXmagic</name>
    <author>
      <link href="https://www.stepwiserefinement.co.uk">
        <text>GPXmagic v3</text>
        <type>text/html</type>
      </link>
    </author>
  </metadata>
"""


writePreamble trackName =
    preamble
        ++ """<trk>  <name>"""
        ++ ElmEscapeHtml.escape trackName
        ++ """</name>"""


writeTrackPoint : GPXSource -> String
writeTrackPoint gpx =
    "<trkpt lat=\""
        ++ (String.fromFloat <| Angle.inDegrees gpx.latitude)
        ++ "\" lon=\""
        ++ (String.fromFloat <| Angle.inDegrees <| Direction2d.toAngle gpx.longitude)
        ++ "\">"
        ++ "<ele>"
        ++ (String.fromFloat <| Length.inMeters gpx.altitude)
        ++ "</ele>"
        ++ "</trkpt>\n"


writeSegment : Maybe String -> List GPXSource -> String
writeSegment segmentName trackPoints =
    let
        namePart =
            case segmentName of
                Just name ->
                    "<extensions><rgt:namedSegment>"
                        ++ ElmEscapeHtml.escape name
                        ++ "</rgt:namedSegment></extensions>"

                Nothing ->
                    ""
    in
    "\n<trkseg>\n"
        ++ String.concat (List.map writeTrackPoint trackPoints)
        ++ namePart
        ++ "</trkseg>\n"


writeFooter =
    "</trk></gpx>\n"


writeGPX : Maybe String -> TrackLoaded msg -> List NamedSegment -> String
writeGPX name track segments =
    --May need to consider storage and excessive concatenation, but try obvious first.
    let
        useName =
            Maybe.withDefault "A track from GPXmagic" name

        writeSegments segs startFrom =
            case segs of
                seg :: moreSegs ->
                    let
                        ( segStartIndex, segEndIndex ) =
                            ( DomainModel.indexFromDistance seg.startDistance track.trackTree
                            , DomainModel.indexFromDistance seg.endDistance track.trackTree
                            )

                        precedingPoints =
                            List.map Tuple.second <|
                                DomainModel.extractPointsInRange
                                    startFrom
                                    (DomainModel.skipCount track.trackTree - segStartIndex + 1)
                                    track.trackTree

                        segmentPoints =
                            List.map Tuple.second <|
                                DomainModel.extractPointsInRange
                                    segStartIndex
                                    (DomainModel.skipCount track.trackTree - segEndIndex)
                                    track.trackTree
                    in
                    writeSegment Nothing precedingPoints
                        ++ writeSegment (Just seg.name) segmentPoints
                        ++ writeSegments moreSegs segEndIndex

                _ ->
                    writeSegment Nothing <|
                        List.map Tuple.second <|
                            DomainModel.extractPointsInRange
                                (startFrom + 1)
                                0
                                track.trackTree
    in
    writePreamble useName
        ++ writeSegments segments 0
        ++ writeFooter
