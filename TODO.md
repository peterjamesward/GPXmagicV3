
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf remains.

**Route maker** Is this broke with simple loops?

--- 

# WIP

## Promote Graph

It's all graphs. 
* Some are trivial, with one track.
* Load more than one GPX, each is a track.
* Draw more than one route, each is a track.
* Analyse them to identify canonical sections.
* Recombine sections into single route.
* All views show all sections.
* Perhaps show "non-active" tracks subdued in all views.
* Edits constrained to one section/track (as now).
* Push the Graph (not Track) on the undo stack (relying on shared immutable data).
* New tool, or extended Graph tool to unload, select, hide tracks. (Pick & Mix?)

We have all the tools for this, it just requires some re-arrangement.
(Does Track == Section?)

Implementation plan:

1. Branch!
2. Take Graph out of TrackLoaded.
3. Allow load of more than one GPX. (New tool rather than change the Load button?)
4. Change Undo to be Graph-based not Track-based.
5. Modify views to show multiple tracks. 
6. Allow drawing of multiple tracks, added to graph
7. New tool to select Active Track.
8. Views to subdue inactive tracks.
9. Modify graph to analyse multiple tracks.
10. Manual addition of node in any edge.
11. Clarify the relationship between tracks (sections) and a route (using sections at least once)
12. Adopting a route returns us to a "trivial" (one track) graph.

---

# BACKLOG

## Route maker -- combine routes

Use more than one GPX as input to route maker.

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


