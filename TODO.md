
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf remains.

BUG: Map projection not restored correctly on reload.

BUG: Route Builder previews not hiding when tool closed.

TODO: Make size of preview dot user choice, 3D and Map views.

--- 

# WIP

## Strava

Use the new Action from annealing branch to tidy up some of the flow.

---

# BACKLOG

## Land Use

Refetch data when more tracks loaded. Or provide button in the eponymous tool.

## Remove Actions

One at a time. I think the way into this is to reduce action codes to:
- UpdatedTrack (Trackloaded msg)
- UpdatedGraph (Graph msg)
- MovedPointers (?)
- etc

## Merge terrain and land use painting

+ Fetch Land Use for subsequent track loads.

+ Make the Land Use tool actually do something.

Using similar but less regular recursion scheme, use Land Use to colour terrain.
Maybe put some buildings, trees, water in for suitable land types, provided not on road.
Previous attempt at doing precise road occlusion was complex and not well done.
Maybe would be OK to just paint any land use parts that don't intersect any roads,
then devise some recursion scheme for those that do. Simple would be to split into
four (NE,NW,SE,SW) and repeat, but perhaps could be more influenced by leaf index.

1. The geometry
   More specifically, in SceneBuilder3D.makeLandUsePlanar(|Sloped), when we have the
   triangulated polygon, do this:
- If the polygon (bounding box) intersects no roads, paint it.
- If any of the polygon intersects any roads:
- For all triangles in polygon:
  - If triangle intersects any road then (recursively)
    - if triangle area > some threshold, subdivide (by splitting longest edge)
    - else make sure the triangle is _below_ the road section
    - Repeat intersection test on split triangles
- This will cease recursion based on size or absence of overlaps
- Paint revised triangulation.

2. The rendering
   For Industrial, Retail, Residential, consider placing some grey or brown blocks in each triangle.
   For Wood, Forest, see if some cones would create a pleasing effect.
   For Water, Rock, Farmland, etc, consider some textures.

On triangles, I think we could make Terrain look less blocky by starting out with a
simple triangulation based on bounding box centroid to each edge (with random perturbation?),
then using the above triangle splitting recursion. Mmm.

## Test cases for edits.

You could do this you know, just not for the visuals.

## Languages

Awaiting French support.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

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

## Snap to Roads with TomTom

https://developer.tomtom.com/snap-to-roads-api/documentation/product-information/introduction

Free for small volumes.
5000 point limit, much better than MapBox.

## Derive climbs from WKO file

_Eric Spencer_
> i haven't fully thought this through yet, but just airing to determine if it's viable as there are some potentially obvious issues. I ride a lot of workouts in ERG mode but many of the roads I use visually don't match the workout segment I'm on (eg. I'm hitting 120% FTP whilst riding downhill). Is there something that could roughly use inputs of my FTP, and my weight, I then provide a wko file that has say 3 blocks of 10 mins at 250W, with 3 mins between at 150W and GPXmagic would create a route with constant climbs and flats that broadly mimic the workout structure for my parameters without needing to use ERG. It would never be the exact durations of the wko as I may put in more or less power but effort wise it will broadly be correct.

Would do this with gradients and segments applied on to a base course.

## Camera

Full control of camera in relation to marker.

## Picture button

Add button to images to allow screen capture. Because "easy".

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



