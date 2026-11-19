module About exposing (aboutText)


aboutText =
    """# GPXmagic v3.18.0 (b079f914, 2026-11-19 09:45)

**GPXmagic V3 works fine with Safari, Firefox & Chrome.**

## 3.18.0

Maintenance release.

 - updated mapbox libraries to 3.33.0 (was 3.13.0),
 
 - GPXmagic is now built with Elm 0.19.3 compiler (was 0.19.1),

 - Strava tool removed as strava now requires a subscription for an API key,  
 (if needed, please send money for Strava subs)

 - Changed source of land use data from Kumi to private.coffee but please note this free service is often overloaded.

## 3.17.0

- Fixed an old "out by one" error in Straightener.

- Previews for Straightener better behaved.

- Updated Mapbox to 3.13.0 (was 3.7.0)

## Acknowledgements

* Thanks to all those who've provided support, comments, bug reports, and help along the way.

* Thanks to Biketerra for extending the concept of Magic Roads.

## Legal guff

Compatible with Strava, for the purpose of loading route and segment data.

GPXmagicV3 is open source at https://github.com/peterjamesward/GPXmagicV3

Contains numerous libraries under various licence terms, all of which are available in source
form via https://package.elm-lang.org.

Map component provided by MapBox.com.

Land use data courtesy of Open Street Map via the Overpass API.

Icons from www.flaticon.com/free-icons/.

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

Cookie policy is "use no cookies". This may not apply to third-party components.
    """
