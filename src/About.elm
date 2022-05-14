module About exposing (..)


aboutText =
    """
# GPXmagic v3.2.14 (9d787e35)

**GPXmagic V3 works best with Chrome** (by far)

* V3 is now the official release.
    The previous version 2.9.5 is [here](https://s3.eu-west-1.amazonaws.com/stepwiserefinement.co.uk/GPXmagic_2_9_5/index.html)

## In this update ...

* Minor tweaks to Smart Smoother:
  - Increment for radius and turn-in is now 0.1m rather than 0.5
  - New "blend" control allows you to vary the relative influence of the forward and reverse routes, which
  can result in a more suitable result.

## Acknowledgements

* Thanks to all those who've provided support, comments, bug reports, and help along the way.

* Thanks to RGT for having the Magic Roads concept and an excellent indoor cycling platform.

## Legal guff

Compatible with Strava, for the purpose of loading route and segment data.

GPXmagicV3 is currently not open-source but source code is available on request.

Contains numerous libraries under various licence terms, all of which are available in source
form via https://package.elm-lang.org.

Map component provided by MapBox.com.

Land use data courtesy of Open Street Map via the Overpass API.

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

Cookie policy is "use no cookies". This may not apply to third-party components.


    """
