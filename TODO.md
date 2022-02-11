
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Curve Former should reset on track load.
BUG: Pointers are not reset on track load.

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear.

--

# WIP

Out and Back without graphs, inc. variable offset.
> No need for graphs:
1. Extended centre line gives midpoints of turn-around arcs,
2. Outward route shifted one way, using Nudge logic,
3. Return route is reverse, shifted oppositely,
4. Arcs are made from parallel case of Bend Smoother.
5. Assemble into a new tree.

---

# BACKLOG, roughly in order ...

## Tools

2. Limit gradients
3. Simplify.
4. Map Satellite style choice
5. Intersections
6. Plan view
7. Fly-through
8. Move & Stretch
9. 1CQF
10. Graph Theory (renamed)
11. Use Strava segment data
12. Loops (inc. impact on others, such as Bezier).
13. Lift and Shift
14. SVG import
15. Terrain.
16. Split and Join

New stuff:
1. Samir's bend detector (amount of angular change within X linear metres). (Option on Bend Problems)
2. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).

## Keyboard shortcuts

> Let the user select a shortcut key for any tool.
> Pressing the key opens the tool.
> I like the idea but I think a "Set shortcuts" configuration would work better.

## Eliding previews

Elide the creation of large previews.

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
