module About exposing (..)


aboutText =
    """
# GPXmagic v3.2.4

(c0d8bbd4)

## In this release ...

* Reduced repainting on Map view; should be a cleaner experience moving along the track.

### In 3.2.3

* All tools now have a video. Some are shared.

* Fixed bug where Move & Stretch was duplicating end points.

* Fixed bug caused by Strava changing the segment API.

* Added "Flush Undo" button in Simplify, which may help if memory usage is high.

* Simplify tool better preserves Orange and Purple marker positions.

## Acknowledgements

* Special thanks to David Ogle for thorough testing, great bug reports, usability advice, and more.

* Thanks to John Bytheway for v3 ideas and support.

* Thanks to all those who've provided support, comments, bug reports, and help along the way.

* Thank to RGT for having the Magic Roads concept and an excellent indoor cycling platform.

## Legal guff

> peterjamesward/GPXmagicV3 is licensed under the
> Creative Commons Zero v1.0 Universal license

Source code available: https://github.com/peterjamesward/GPXmagicV3

Compatible with Strava, for the purpose of loading route and segment data.

Map component provided by MapBox.com

Land use data courtesy of Open Street Map via the Overpass API.

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

No cookies are used, though many chocolate digestives were consumed whilst writing.


    """
