
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf remains.

BUG: "Error: Invalid value for <circle> attribute cy="NaN"" -- what triggers this?

--- 


# WIP

## Route Builder

* More testing.
* Video(s).

---

# BACKLOG

## Save tools settings

e.g. Tools Summary should remember its state.

## Tools Controller

Code clean up, optimisation (albeit very minor and localised).
Not nice doing all those list maps. 
A Dict perhaps, though large sum type involved which stopped me before.
Hide previews for hidden tools.

## Remove Actions

One at a time. I think the way into this is to reduce action codes to:
- UpdatedTrack (Trackloaded msg)
- UpdatedGraph (Graph msg)
- MovedPointers (?)
- etc

>(export cycles permitting)

> Not sure quite about subscriptions: mapPort, Flythrough, ...

Then Main.update reduces to a few cases where we can act directly.
This may work better with changes above to ToolsController.
It's almost a V4, not quite as core structures are the same,

## Derive climbs from WKO file

_Eric Spencer_
> i haven't fully thought this through yet, but just airing to determine if it's viable as there are some potentially obvious issues. I ride a lot of workouts in ERG mode but many of the roads I use visually don't match the workout segment I'm on (eg. I'm hitting 120% FTP whilst riding downhill). Is there something that could roughly use inputs of my FTP, and my weight, I then provide a wko file that has say 3 blocks of 10 mins at 250W, with 3 mins between at 150W and GPXmagic would create a route with constant climbs and flats that broadly mimic the workout structure for my parameters without needing to use ERG. It would never be the exact durations of the wko as I may put in more or less power but effort wise it will broadly be correct.

Would do this with gradients and segments applied on to a base course.

## Camera

Full control of camera in relation to marker.

## Picture button

Add button to images to allow screen capture. Because "easy".

## Test cases for edits.

You could do this you know, just not for the visuals.

## Languages

Awaiting French support.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

## Technical debt

Tagged types for Point v Line indices to avoid confusion.

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Land use 3D rendering

Experiment proved the idea (of roads partitioning polygons) but implementation was weak.
Roads should divide polygons, but care needed over directionality and crossing points.
It needs doing properly, including the start and finish cases.

## Usability

Drag Curve Former circle directly in Plan View. (Add an SVG "handle" to hit detect.)
Ditto for Move & Stretch, possibly Nudge.
Provide info text capability on top bar and on view panes.

## Tools: old, updated, & new

- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Use localised number formatting everywhere (for French use of , and .)

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

---

# The cellar

## Street view

Nope, can't be done in an iFrame. Google stops it. Alternative APIs too costly.

## LIDAR

Direct reading of LIDAR data.
-- Nope. Not enough data, tricky to access, mostly not free.

## Replace MapBox?

https://github.com/klaftertief/slippery-slope

Motivation is to be free of some dodgy JavaScript.
Free. Forever.
Or Haskell, I suppose.
Bloody good though, pure Elm.
Not sure how to build it, but I cloned it anyway.

https://github.com/lucamug/elm-map
Might make more sense - more recently updated.

## Route joining

Open multiple GPX routes (file, Strava) and combine them in Route Maker.
May not even be that hard, given Route Maker exists.
But, routing tools exist.

## Video playback window

Sync'd to Orange?
https://www.w3schools.com/jsref/dom_obj_video.asp

## Segment output

Semantically, endpoints should be duplicated between segments, but apparently RGT barfs.
Thus, provide option to output duplicate points but leave default as is until RGT complies.
(Dan Connelly)



