
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf will remain.

**Map projection** not restored correctly on reload.

**Route Builder** previews not hiding when tool closed.

**BUG** Map and course part company! 3D and Plan. Disappointing.
> Something smells. Maybe related to resize (which seems to fix it).

--- 

# WIP

---

# BACKLOG

## Terrain

Oh, I have just had this idea (2024-10-29)! -- 
> Push terrain generation into a shader.
Each pixel to extract nearby track (fixed radius or nearest).
If it's on a road, use the road height. Otherwise some blend.
Colour roads grey, colour non-roads by altitude and slope.
Could also have macro-level effects across whole track.

## Alternative 3D thing

There's also a different 3d package
https://package.elm-lang.org/packages/ryan-haskell/elm-3d/latest/
which can use (some) OBJ files, like:
https://kaylousberg.itch.io/kaykit-medieval-hexagon
Wouldn't that be fun!
Don't know what I'd do about roads, that's almost irrelevant :~D

## Update Mapbox

Mapbox GL JS V3.
<link href="https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css" rel="stylesheet">
<script src="https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"></script>

## Move point

In Freehand drawing mode, click within a foot of track point activates direct dragging of point.
(Maybe a long click, with change of pointer to confirm mode.)

## Synthesizer

FFT generators for theta (or X, Y) & phi (or Z) as function of s.

## Test cases for edits.

You could do this you know, just not for the visuals.

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

---

# The cellar

## Segments

Integrated into the tree so they can be edited meaningfully.
Not doing it. Applying edits is tricky.

## Languages

Awaiting French support.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

## Out and Back

Make the final turn optional. 
> But is easy enough to delete.

## Land Use

Refetch data when more tracks loaded. Or provide button in the tool.
= Make the Land Use tool actually do something.

## Merge terrain and land use painting

(Experiment proved the idea (of roads partitioning polygons) but implementation was weak.
Roads should divide polygons, but care needed over directionality and crossing points.
It needs doing properly, including the start and finish cases.)

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

## Remove Actions

It's a challenge to avoid import loops, the interaction between tools and track (and graph) is hairy.
Whilst something of a burden, the Actions approach provides flexibility (yes, maybe too much).

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

## Video playback window

Sync'd to Orange?
https://www.w3schools.com/jsref/dom_obj_video.asp

## Segment output

Semantically, endpoints should be duplicated between segments, but apparently RGT barfs.
Thus, provide option to output duplicate points but leave default as is until RGT complies.
(Dan Connelly)



