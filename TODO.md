
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Curve Former should reset on track load.
BUG: Pointers are not reset on track load.

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something.

BUG: Points on the map should be elided to match the route line.

???: There's this thing where map hit detect on 300K route is _very_ slow
though eventually completes. I wonder if we'd be better off (i.e. more consistent)
by using something closer to a whole-track fold. But some hybrid of a "keep track of the
closest so far", with a measure of culling -- no need to descend into subtrees that are 
clearly less promising.

--

# WIP

Addenda video - steep climbs and how these are shown on the Profile view. 
Plus global settings.

Samir's bend detector (amount of angular change within X linear metres)
> This goes in bend problems and uses a fold similar to the direction detection in Radiused'.

---

# BACKLOG, roughly in order ...

## Tools

2. Add whole track option w/out preview for Interpolate
3. 1CQF (i.e. Simplify x N / Interpolate / Bezier )
4. Limit gradients
5. Intersections
6. Fly-through
7. Move & Stretch
8. Graph Theory (renamed)
9. Use Strava segment data
10. Loops (inc. impact on others, such as Bezier).
11. Lift and Shift
12. SVG import
13. Terrain
14. Map style choice (Satellite if possible)
15. Split and Join
16. Texture for the ground plane.

New stuff:
2. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
3. JB loop detector (see below)

## Keyboard shortcuts

> Let the user select a shortcut key for any tool.
> Pressing the key opens the tool.
> I like the idea but I think a "Set shortcuts" configuration would work better.

## Eliding previews

Elide the creation of large previews. (mini-PeteTree?)

## First person view & Flythrough

Same as v2. Use 3d-scene. Improve (i.e. damp motion) camera tracking?

## Terrain

Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

---

# Parked

## Laziness, optimisation

Don't render anything that's not visible.

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
