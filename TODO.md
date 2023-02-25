
# BUGS

Title bar display not showing track name when loaded from URL.

Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

Delete should display "Sorry" message unless one leaf remains.

No preview for single point nudge without fade.

Land use 3D is broken. Suspect elevations are not being read from map.

--- 

# WIP

## Route joining

Open multiple GPX routes (file, Strava) and combine them in Route Maker.

May not even be that hard, given Route Maker exists.

---

# BACKLOG

## Refactor Main -> Tools

I hope this will open the door to a much cleaner control flow, perhaps deprecating
actions (yes, after all that) and using the track/newTrack diff to drive command generation.

## Video playback window

Sync'd to Orange?
https://www.w3schools.com/jsref/dom_obj_video.asp

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

## Redo Profile (again)?

https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/
No: renders to SVG not Canvas, so disappointing really.

## Street view

Nope, can't be done in an iFrame. Google stops it.

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


