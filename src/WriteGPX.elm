module WriteGPX exposing (writeGPX)

import Angle
import Direction2d
import DomainModel exposing (GPXSource)
import ElmEscapeHtml
import Length
import TrackLoaded exposing (TrackLoaded)


preamble =
    """<?xml version='1.0' encoding='UTF-8'?>
<gpx version="1.1"
  creator="https://www.stepwiserefinement.co.uk"
  xmlns="http://www.topografix.com/GPX/1/1"
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


writePreamble =
    preamble


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


writeTrack : String -> List ( GPXSource ) -> String
writeTrack name trackPoints =
    """
  <trk>
    <name>"""
        ++ ElmEscapeHtml.escape name
        ++ """</name>
    <trkseg>
"""
        ++ String.concat (List.map writeTrackPoint trackPoints)
        ++ """    </trkseg>
  </trk>
 """


writeFooter =
    "</gpx>"


writeGPX : Maybe String -> TrackLoaded msg -> String
writeGPX name track =
    let
        points =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        useName =
            case name of
                Just n ->
                    n

                _ ->
                    "A track from GPXmagic"
    in
    writePreamble
        ++ writeTrack useName points
        ++ writeFooter
