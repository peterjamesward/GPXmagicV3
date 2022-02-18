
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

DEBT: Memory update messages should originate from the TrackInfoBox tool.

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something.

--

# WIP

1. Fly-through

---

# BACKLOG, roughly in order ...

## Tools: old, updated, & new

2. Use Strava segment data
3. Move & Stretch
4. Loops (inc. impact on others, such as Bezier)
5. Intersection detection ((?? JB loop detection ??))
6. Graph Theory (renamed)
7. Option to show MR rendering cutoff.
8. Map style choice (Satellite if possible)
9. Option to add 80m at Start, 200m at end for start/end gates in RGT
10. Tooltips (where useful)
11. Extract all text for translation (Muriel)
12. Use localised number formatting everywhere (for French use of , and .)
13. Ability to point-smooth transitions over a range
14. Chart can only show one Preview; how do we determine which?
15. Chart preview for Centroid, Bezier, point smoother
16. Improve default zoom
17. Terrain (with texture)
18. Tools that require a range should say so when there isn't one! (David Ogle)
19. Gaussian (including necessary interpolation)
20. Split and Join
21. Update the log dashboard to view v3 activity.
22. Draggable tools?
23. Move tool: reverse geocode map click with https://positionstack.com 

New stuff:
1. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
> This could be like a meta-box, or a "build your own 1CQF", in which
> we pipeline existing features, just like 1CQF.
> E.G. simplify > limit > interpolate > centroid.

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

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

## Icons

Look at Font Awesome. May be worth swapping.

