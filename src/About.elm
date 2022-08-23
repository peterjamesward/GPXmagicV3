module About exposing (..)


aboutText =
    """
# GPXmagic v3.4.0 (fa1942cc)

**GPXmagic V3 works (by far) best with Chrome (and some derivatives)**

## In 3.4

### New Timestamps tool

As of 3.4, GPXmagic will read timing information in recorded GPX files, and will write these
out again. This can be useful if you're trying to match video frames with GPX locations, for
Kinomap for example.

It permits editing with caveats:

* Edits that simply move existing points in space will not change the time on those points.
(Example: Nudge, Straighten, Profile smoothing, Centroid average.)

* Edits that create new points will attempt to use times that are "sensible" -- generally being
interpolated from the existing track.
(Example: Splines, Bend smoothing, One-Click Quick-Fix.)

* Edits that delete points will not change the timing of remaining points.
(Example: Delete.)

* Route planner doesn't make any sense, not does out and back.

* Errors are quite likely, at this stage in development.

The new tool allows a few operations specifically affecting time:

* Change the time on a track point to correspond with a specific time on a video (say). This will
shift all subsequent points by the same amount, to the end of the track.

* The "time control" is uses the time offset from the start of the ride (is this best?). The
chevrons above and below allow you to increment and decrement by hours, minutes, seconds, or
milliseconds. You can also remove the milliseconds. You may not move to a time offset earlier
than the preceding point.

* You may _replace_ all existing points by points that interpolate time-wise at intervals of
half-second, second, or 5 seconds.

* You may double all the time offsets for the whole route. Suppose you've
recorded a 60fps video from a motor vehicle, and if played at half-speed, it would sync. well
for a bike ride. (This does not require that you first use time interpolation.)

### Strava Activity import

In the Strava tool you can now load both Routes and Activities directly into GPXmagic. Activities
will (of course) have time information available.

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
