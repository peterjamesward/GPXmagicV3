
# BUGS & DEBT (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. 
> Tool confirms.

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Curve Former should reset on track load.
BUG: Pointers are not reset on track load.

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear.

BUG: Bezier spline whole track does not undo properly, 
     but only at low tolerances. Seems to retain the spline.

# WIP

## Display file and track names and allow file name edit

---

# BACKLOG, roughly in order ...

## Tools

1. SVG import
2. Use local Bezier approximation to spot fix bend and gradient issues (?)
3. Memory usage (incl limiting actions such as dumping Undo stack).
4. Out and Back without graphs!
5. Limit gradients
6. Map Satellite style choice
7. Intersections
8. Tip Jar / Buy me a coffee.
9. Move & Stretch
10. Fly-through
11. Graph Theory (renamed)
12. Use Strava segment data
13. Loops (inc. impact on others, such as Bezier).
14. Terrain.

New stuff:
1. Samir's bend detector (amount of angular change within X linear metres).
2. Clothoids (promoted because may be similar structurally to bezier & centroid)
3. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).

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

Could optimize Gradient problems by keeping a list of all > X and subsetting.
But is not too slow now with 973K points, so why bother?.