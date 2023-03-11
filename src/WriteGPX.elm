module WriteGPX exposing (writeGPX)

import Angle
import Direction2d
import DomainModel exposing (GPXSource, RoadSection)
import ElmEscapeHtml
import Iso8601
import Length exposing (Meters)
import Quantity exposing (Quantity)
import String.Interpolate
import Tools.NamedSegmentOptions exposing (NamedSegment)
import Tools.RGTOptions
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



{-
   <extensions>
   <rgt:parserOptions>
   <rgt:disableElevationFixes/>
   <rgt:disableAdvancedSmoothing/>
   <rgt:maxSlope>20</rgt:maxSlope>
   </rgt:parserOptions>
   </extensions>
-}


optionsIfNotDefault : Tools.RGTOptions.Options -> String
optionsIfNotDefault options =
    if options == Tools.RGTOptions.defaults then
        ""

    else
        String.concat
            [ """<extensions>
<rgt:parserOptions>"""
            , if options.disableElevationFixes then
                """
<rgt:disableElevationFixes/>"""

              else
                ""
            , if options.disableAdvancedSmoothing then
                """
<rgt:disableAdvancedSmoothing/>"""

              else
                ""
            , if options.maxSlope /= Tools.RGTOptions.defaults.maxSlope then
                String.Interpolate.interpolate
                    """
<rgt:maxSlope>{0}</rgt:maxSlope>"""
                    [ String.fromFloat options.maxSlope ]

              else
                ""
            , """
</rgt:parserOptions>
</extensions>
"""
            ]


writePreamble : String -> Tools.RGTOptions.Options -> String
writePreamble trackName rgtOptions =
    preamble
        ++ optionsIfNotDefault rgtOptions
        ++ """<trk>  <name>"""
        ++ ElmEscapeHtml.escape trackName
        ++ """</name>"""


writeTrackPoint : GPXSource -> String
writeTrackPoint gpx =
    case gpx.timestamp of
        Just isTimed ->
            String.Interpolate.interpolate
                "<trkpt lat=\"{0}\" lon=\"{1}\"><ele>{2}</ele><time>{3}</time></trkpt>\n"
                [ String.fromFloat <| Angle.inDegrees gpx.latitude
                , String.fromFloat <| Angle.inDegrees <| Direction2d.toAngle gpx.longitude
                , String.fromFloat <| Length.inMeters gpx.altitude
                , Iso8601.fromTime isTimed
                ]

        Nothing ->
            String.Interpolate.interpolate
                "<trkpt lat=\"{0}\" lon=\"{1}\"><ele>{2}</ele></trkpt>\n"
                [ String.fromFloat <| Angle.inDegrees gpx.latitude
                , String.fromFloat <| Angle.inDegrees <| Direction2d.toAngle gpx.longitude
                , String.fromFloat <| Length.inMeters gpx.altitude
                ]


namePart segmentName =
    case segmentName of
        Just name ->
            "<extensions><rgt:namedSegment>"
                ++ ElmEscapeHtml.escape name
                ++ "</rgt:namedSegment></extensions>"

        Nothing ->
            ""


writeSegment : Maybe String -> List GPXSource -> String
writeSegment segmentName trackPoints =
    "\n<trkseg>\n"
        ++ String.concat (List.map writeTrackPoint trackPoints)
        ++ namePart segmentName
        ++ "</trkseg>\n"


writeFooter =
    "</trk></gpx>\n"


writeGPX : String -> Tools.RGTOptions.Options -> TrackLoaded msg -> String
writeGPX name options track =
    --May need to consider storage and excessive concatenation, but try obvious first.
    let
        segments =
            track.namedSegments

        writeSegments segs startFrom =
            case segs of
                seg :: moreSegs ->
                    let
                        ( segStartIndex, segEndIndex ) =
                            ( DomainModel.indexFromDistanceRoundedUp seg.startDistance track.trackTree
                            , DomainModel.indexFromDistanceRoundedDown seg.endDistance track.trackTree
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
                        ++ writeSegments moreSegs (segEndIndex + 1)

                _ ->
                    writeSegment Nothing <|
                        List.map Tuple.second <|
                            DomainModel.extractPointsInRange
                                startFrom
                                0
                                track.trackTree
    in
    writePreamble name options
        ++ writeSegments segments 0
        ++ writeFooter
