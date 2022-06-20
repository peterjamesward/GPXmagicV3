module About exposing (..)


aboutText =
    """
# GPXmagic v3.3.8 (6cb67092)

**GPXmagic V3 works best with Chrome** (by far)

## In this update

* New options menu next to Save button that allows selecting the new RGT GPX parser options.

(If that means nothing, don't use it.)

## In 3.3.7

* Fixed "out by one error" in Named Segments. The points at the orange and purple marker should now
be correctly written within the segment. My understanding is that the segment end gate is placed at
the final point *within* the segment. I have tested this with a two-point segment and all seems well.

* Route maker hides white "snap" dots once route is analyzed.

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
