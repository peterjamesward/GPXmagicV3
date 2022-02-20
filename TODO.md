
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something.

BUG: Dragging - Map points move slightly even when mouse does not.

--

# WIP

---

# BACKLOG, roughly in order ...

## Tools: old, updated, & new

1. Move & Stretch
2. Loops (inc. impact on Bezier & Centroid)
3. Intersection detection ((?? + loop detection ??))
4. Split and Join
5. Graph Theory (renamed)
6. Map style choice (Satellite if possible)
7. Chart can only show one Preview; how do we determine which?
8. Chart preview for Centroid, Bezier, point smoother
9. Extract all text for translation (Muriel)
10. Use localised number formatting everywhere (for French use of , and .)
11. Ability to point-smooth transitions over a range
12. Terrain (with texture)
13. Tools that require a range should say so when there isn't one! (David Ogle)
14. Gaussian (including necessary interpolation)
15. Option to add 80m at Start, 200m at end for start/end gates in RGT
16. Option to show MR rendering cutoff.
17. Tooltips (where useful)
18. Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
19. Draggable tools?

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

## Eliding previews

Elide the creation of large previews. (mini-PeteTree?)

## Terrain

Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.

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
But is not too slow now with 973K points, so why bother?.

## Icons

Look at Font Awesome. May be worth swapping.

