
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf remains.

--- 

# WIP

## Promote Graph

Implementation: (RM = existing Route Maker)

1. ~~"Arc" option for route view should just traverse with very low depth.~~
2. Click detect is broken.
3. Fetch elevation data iffy.
4. Identify clusters not very good. 
> Index contains point numbers greater than number of points on route. Oops. 
5. "Snap" (consolidate) nearby points and roads from RM. ( Inverse is XY -> (Track, Offset) )
6. Canonicalize from RM. Might write again, could be clearer.
7. Use random words list for canonical node & edge labelling. 
8. Limited Undo as in RM; use simple state machine across canonicalization & routing.
9. Should be able to make a route, if ends join up, which they won't, in general.
10. New track with offset from RM..
11. Rename tool: "Many ways", "Route builder" ?
12. Option to show/hide unused nodes.
13. Check Land Use data being handled correctly.
14. (Optimise loading from GPX/Strava to avoid having to rebuild tree. -- Nah.)

---

# BACKLOG

## Refactor Main -> Tools

I hope this will open the door to a much cleaner control flow, perhaps deprecating
actions (yes, after all that) and using the track/newTrack diff to drive command generation.

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

## Route joining

Open multiple GPX routes (file, Strava) and combine them in Route Maker.
May not even be that hard, given Route Maker exists.
But, routing tools exist.

## Video playback window

Sync'd to Orange?
https://www.w3schools.com/jsref/dom_obj_video.asp


