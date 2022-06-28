module About exposing (..)


aboutText =
    """
# GPXmagic v3.3.10 (e10db30d)

**GPXmagic V3 works best with Chrome** (by far)

## Changes

In Route maker:

- When you click "Enter route maker mode", the track display will switch to the Route view, unless
you already have this visible. You will need this to make changes to the route.

- There's a new menu option in the Route view when you click on a Road. If the Road is not being
used in the route, you can delete the road. If either end Place now has no Roads, they will also
be deleted.

- If you deleted the wrong Road, you can use the new "Undo delete road" button which appears below
the Route listing.

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
