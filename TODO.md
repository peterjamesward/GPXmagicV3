
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Curve Former should reset on track load.
BUG: Pointers are not reset on track load.

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something.

BUG: Points on the map should be elided to match the route line.
> They are but seems that edits spoil this.

--

# WIP

Addenda video - steep climbs and how these are shown on the Profile view. 
Plus global settings. Plus localised render detail.

---

# BACKLOG, roughly in order ...

## Tools

2. 1CQF (i.e. Simplify x N / Interpolate / Bezier )
3. Limit gradients
4. Intersections
5. Fly-through
6. Move & Stretch
7. Graph Theory (renamed)
8. Use Strava segment data
9. Loops (inc. impact on others, such as Bezier).
10. Lift and Shift
11. SVG import
12. Extract all text for translation
13. Use localised number formatting everywhere (for French use of , and .)
14. Terrain
15. Map style choice (Satellite if possible)
16. Split and Join
17. Texture for the ground plane.

New stuff:
1. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).

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
