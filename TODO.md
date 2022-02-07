
# BUGS & DEBT (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. 
> (May have to wait until we have that tool!)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

Curve Former should reset on track load.

# WIP

Classic bend smoother 
Bend smoother logic carried over perfectly.
Need to update control when pointers are moved.
Need apply.
(fix Samir's bug on looped routes == same point exactly on different lap?)

---

# BACKLOG, roughly in order ...

## Tools

1. Drag on map
2. Nudge 
3. Segment info
4. Clothoids (promoted because may be similar structurally to bezier & centroid)
5. Move & Stretch (2-way drag should correct for azimuth)
6. Gradient problems
7. Steep climbs
8. Intersections
9. Visual options
10. SVG import
11. Fly-through
12. Limit gradients 
13. Graph Theory
14. Use Strava segment data
15. Loops (includes Out and Back) (+ impact on others, such as Bezier).
16. Map Satellite style choice
17. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
18. Something akin to Graphs, stemming from Muriel's route.
19. Memory usage (incl limiting actions such as dumping Undo stack).
20. Samir's bend detector (amount of angular change within X linear metres).

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

