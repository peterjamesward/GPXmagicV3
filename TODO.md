
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Curve Former should reset on track load.
BUG: Pointers are not reset on track load.

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear.

--

# WIP

**Display file and track names** and allow file name edit

---

# BACKLOG, roughly in order ...

## Tools

2. SVG import
3. Use local Bezier approximation to spot-fix bend and gradient issues (?)
4. Memory usage (incl limiting actions such as dumping Undo stack).
5. Out and Back without graphs, inc. variable offset.
6. Limit gradients
7. Map Satellite style choice
8. Intersections
9. Simplify.
10. Tip Jar / Buy me a coffee
11. Move & Stretch
12. Fly-through
13. Plan view
14. Graph Theory (renamed)
15. Use Strava segment data
16. Loops (inc. impact on others, such as Bezier).
17. Lift and Shift
18. Split and Join
19. Terrain.

New stuff:
1. Samir's bend detector (amount of angular change within X linear metres). (Option on Bend Problems)
2. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).

## Keyboard shortcuts

> Let the user select a shortcut key for any tool.
> Pressing the key opens the tool.
> I like the idea but I think a "Set shortcuts" configuration would work better.

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
ut is not too slow now with 973K points, so why bother?.
