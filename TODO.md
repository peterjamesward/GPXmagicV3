
# BUGS & DEBT (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. 
> (May have to wait until we have that tool!)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Curve Former should reset on track load.

BUG: Classic bend smoother consumes all memory on certain looped routes where
orange and purple are (possibly) co-linear.
? Infinite arc segments?

BUG: Pointers may be not reset on track load.

# WIP

1. Segment info (toggle with whole track?)
2. Gradient problems
3. Steep climbs


---

# BACKLOG, roughly in order ...

## Tools

1. Limit gradients
2. Visual options
3. Intersections
4. Tip Jar / Buy me a coffee.
5. Map Satellite style choice
6. Memory usage (incl limiting actions such as dumping Undo stack).
7. SVG import
8. Move & Stretch
9. Fly-through
10. Graph Theory
11. Use Strava segment data
12. Loops (includes Out and Back) (+ impact on others, such as Bezier).
13. Samir's bend detector (amount of angular change within X linear metres).
14. Clothoids (promoted because may be similar structurally to bezier & centroid)
15. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
16. Something akin to Graphs, stemming from Muriel's route.

## Error messages

Using an action DisplayMessage to show modal dialog from any tool. 
Add a non-modal message error for info.

## Plan view

Same as v2. Use 3d-scene. Orthographic camera.

## First person view

Same as v2. Use 3d-scene. Improve (i.e. damp motion) camera tracking?

## Terrain

Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

## Laziness, optimisation

Don't render anything that's not visible.

Disable Live Preview for large tracks or large selections.
Better, a user-chosen threshold of number of points for live preview.
> Add this to "global state" which needs to pass aroud, like Imperial.

---

# Parked

## Profile preview

This needs previewData to have a PeteTree so we can derive distances.
> We can always derive startDistance using startFrom.

This will also allow elision, so ability to handle much larger previews.

Should distinguish between new (sequential) track points and (isolated) features
on existing track. Latter is bends, gradients, points to delete.

Hence, not just one-size-fits-all "preview".

Then again, it's not really worth worrying about. If we have excessive points
in a preview list, we can easily elide them by skipping through the list in the
view; a PeteTree is OTT.

## Other 

2-way drag should correct for azimuth. NOT POSSIBLE - multiple views!

