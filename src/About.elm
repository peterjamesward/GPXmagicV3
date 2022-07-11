module About exposing (..)


aboutText =
    """
# GPXmagic v3.3.18 (9ced8ef7)

**GPXmagic V3 works (by far) best with Chrome (and some derivatives)**

## In 3.3.18

In David Ogle's words:

> [Say] the orange marker is near the end of the course and I select a trackpoint nearby,
  it tends to select the one at the start of the course, instead of the one I was trying
  to select near the end. So I'm thinking if you've got 2 trackpoints in the same position,
  on selection (mouse click), it'd be good to select the one closest to the current position.

It's subtle, but nicer, and might provide slightly better performance on some routes, not all.

Quite a few internal changes to support this, maybe Save more often, just in case.

## Acknowledgements

* Thanks to all those who've provided support, comments, bug reports, and help along the way.

* Thanks to RGT for the Magic Roads concept and an excellent indoor cycling platform.

## Legal guff

Compatible with Strava, for the purpose of loading route and segment data.

GPXmagicV3 is open source at https://github.com/peterjamesward/GPXmagicV3

Contains numerous libraries under various licence terms, all of which are available in source
form via https://package.elm-lang.org.

Map component provided by MapBox.com.

Land use data courtesy of Open Street Map via the Overpass API.

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

Cookie policy is "use no cookies". This may not apply to third-party components.


    """
