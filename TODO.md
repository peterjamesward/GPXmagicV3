
# BUGS & DEBT (for an 'easy' day)

DEBT: Map sends more click messages each time we click. 
> They are debounced but, still, why??

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

--

# WIP

BUG: Dragging - Map points move slightly and edit is registered even when mouse does not move.
> Drag should not start until there's actual movement beyond some threshold ("sticky").
> Note that the "dragging" status is just wrong. v2 is not like this.

---

# BACKLOG, roughly in order ...

## Tools: old, updated, & new

1. Move & Stretch
2. Loops (inc. impact on Bezier & Centroid)
3. Intersection detection ((?? + loop detection ??))
4. Split and Join
5. Graph Theory (renamed)
6. Switch to Perspective on first track load.
7. Persist Display preferences.
8. --- Cut-off for release
9. Map style choice (Satellite if possible)
10. Chart can only show one Preview; how do we determine which?
11. Chart preview for Centroid, Bezier, point smoother
12. Extract all text for translation (Muriel)
13. Use localised number formatting everywhere (for French use of , and .)
14. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
15. Terrain (with texture maybe)
16. Ability to point-smooth transitions over a range
17. Tools that require a range should say so when there isn't one! (David Ogle)
18. --- Cut-off
19. Option to add 80m at Start, 200m at end for start/end gates in RGT
20. Option to show MR rendering cutoff.
21. Tooltips (where useful)
22. Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
23. Draggable tools?

New stuff:
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

David Ogle: In V3 the trackpoints are pretty small - they get lost a bit in the black lines between the trackpoints. Could be worth making the trackpoints a bit bigger and/or making the joining lines a different colour (maybe grey would do it).

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

## Graphs

David Ogle: After using graph theory you end up with out and back trackpoints in the same position... say the orange marker is near the end of the course and I select a trackpoint nearby, it always selects the one at the start of the course, instead of the one I was trying to select near the end. So I'm thinking if you've got 2 trackpoints in the same position, on selection (mouse click), it'd be good to select the one closest to the current position.


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

