
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
DEBT: Map sends more click messages each time we click. They are debounced but, still, why??

# WIP

2. Centroid average

---

# BACKLOG, roughly in order ...

## Tools

3. Curve Former (2-way drag should correct for azimuth)
4. Classic bend smoother
5. Drag on map
6. Nudge
7. Move & Stretch (2-way drag should correct for azimuth)
8. Gradient problems
9. Steep climbs
10. Intersections
11. Fly-through
12. Limit gradients
13. Segment info
14. Graph Theory
15. Use Strava segment data
16. Loops (includes Out and Back) (+ impact on others, such as Bezier).
17. Visual options
18. SVG import
19. Map Satellite style choice
20. Super smoothing  (think GPXsmoother, but different)
21. Something akin to Graphs, stemming from Muriel's route.
22. Track synthesise (think Moog for GPX)
23. Memory usage
24. Samir's bend detector
25. Clothoids

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

_Correctly_ fix the replaceRange so that it just rebuilds the tree.

---

