module About exposing (..)


aboutText =
    """
# GPXmagic v3.5.7 (fb46e089)

**GPXmagic V3 works (by far) best with Chrome (and some derivatives)**

## 3.5.7

Adds support for "Open in GPXmagic button". If you have a site that hosts GPX files, you
can add this button (or any button of your choice), so that GPXmagic will launch and load
the GPX file from the URL provided. **Please note** for this to work, you must ensure that
the site allows CORS access from http://stepwiserefinement.co.uk.s3-website-eu-west-1.amazonaws.com
-- if you don't know what that means, you probably won't be able to make it work :)

## 3.5.6

Moved Pane Layout menu to top right of the first view pane, instead of being buried in the
top bar.

Profile view now respects option to use Imperial measures.

## 3.5.5

Fixes bug that misplaced segments on routes with named segments and duplicate adjacent trackpoints.

## 3.5.4

Shades segments on Profile view, all the time.

Fixes some refresh errors on Named Segments previews.

Add a "Google Street View" link to the Information tool (Point mode).

## 3.5.3

When Named Segment tool is open, the buffer zones at the start and end of the route
are marked with a pale colour. (Best not use this colour for the tool.)

## 3.5.2

In the Strava tool, when a Strava route is loaded, the "view on Strava" link now works.

In the Information tool, under "Point", there is a link that will attempt to open Google
Street View, looking along the route from the location of the Orange marker.

## 3.5.1

A satisfying liaison between Land Use data and Named Segments provides automatic creation
(or "seeding" if you prefer) of segments near named features on the route, given available
Open Street Map data. 3.5.1 makes it easier to detect incorrect segments.

## 3.4.11

Small changes to Move, Scale, Rotate:

* Track length matches current length when tool is opened.
* Track length can be adjusted in 10m or 17.6 yard increments (click on the slider then use the
left and right arrows to adjust finely.)

## 3.4.10

Displays route on Map when route loaded from Strava.

Corrected segment proximity to route check that was failing in Southern hemisphere.

## 3.4.9

Bumps the Mapbox component from v2.6 to v2.10.

Switches to "globe" projection instead of Mercator.

Added a new "Basic" map option with no terrain.

## 3.4.8

In Bend Problems and Gradient Problems, clicking on the next / current / previous buttons
will centre the Map view on the problem, regardless of the status of the padlock in the view.

## 3.4.7

In Named Segments, the "eye" icon is always visible for all segments. Thanks, Jan Langkjær Hansen.

## 3.4.6

The Ukrainian stripes are replaced by a black stripe of mourning for Queen Elizabeth II.

## In 3.4.5

Fix bug where updating a Named Segment such that it changes the segment order results
in much confusion.

## In 3.4.4

Culls place names in 3D view so that only those visible are painted. More responsive and
way less confusing.

## In 3.4.3

In Split and Join, you can control where the splits are. If you only need one split, use
the Orange marker only and it will write two files, one up to the Orange marker and one
beyond. If you place the Purple marker, it will write the section between the markers, so
you can dairly easily hop along the route and write out your chosen sections. Thanks to
Jan Deca for this suggestion.

## In 3.4.2

You can Hide a tool directly from the tool setting control. If you do this, you will need
to use the Tools summary control to restore it. Thanks to David Ogle for this suggestion.

## In 3.4.1

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

* Route planner doesn't make any sense, nor does out and back.

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
