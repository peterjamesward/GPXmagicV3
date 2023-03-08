
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf remains.

--- 

# WIP

## Promote Graph

Fetch elevation data iffy -- would be good to sort this out.

Click detect is broken.

### Implementation: (RM = existing Route Maker)

1. Identify clusters not very good. 
> Will (largely) rewrite this. It will be clearer. E.g. do a "map over all leaves in all tracks" function.
> Point in more than one cluster -- nearest, not biggest, should win?
> Wonder if the first phase should just insert points at perp feet as it finds them. Whatever is clearest!
2. "Snap" (consolidate) nearby points and roads from RM. ( Inverse is XY -> (Track, Offset) )
3. Canonicalize from RM. Might write again, could be clearer.
4. Use random words list for canonical node & edge labelling. 
5. Limited Undo as in RM; use simple state machine across canonicalization & routing.
6. Should be able to make a route, if ends join up, which they won't, in general.
7. New track with offset from RM..
8. Rename tool: "Many ways", "Route builder" ?
9. Option to show/hide unused nodes.
10. Check Land Use data being handled correctly.
11. (Optimise loading from GPX/Strava to avoid having to rebuild tree. -- Nah.)

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


