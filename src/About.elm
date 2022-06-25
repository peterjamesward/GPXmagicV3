module About exposing (..)


aboutText =
    """
# GPXmagic v3.3.9 (e10db30d)

**GPXmagic V3 works best with Chrome** (by far)

## In 3.3.9

In Route maker:

- In "Route" view, as you change the tolerance, you will see how that affects how the route
is divided into Places and Roads. This should help determine the best tolerance.

- The buttons are re-labelled and work _slightly_ differently. "Merge nearby points" will
simply combine the nearby points (as a normal track edit) but will not then enter the Route
setting mode. Whereas "Enter route maker mode" will also merge the points but will then take
you into the route planning mode of Route maker.

There'll be a new video up soon to demonstrate.

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
