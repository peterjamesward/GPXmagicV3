
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

--

# WIP

## WebGL profile view

SVG overlay for Orange and Purple info.
SVG overlay for info follows mouse.

---

# BACKLOG, roughly in order ...

## Tools: old, updated, & new

1. Loops (inc. impact on Bezier & Centroid)
2. Split and Join
3. Intersection detection ((?? + loop detection ??))
4. Graph Theory (notes below and from DO's emails)
5. Map style choice (Just call setStyle with the URL - see index.html)

--- Cut-off for release
7. Extract all text for translation (Muriel)
8. Use localised number formatting everywhere (for French use of , and .)
9. Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
10. Terrain (with texture maybe)
11. Ability to point-smooth transitions over a range
12. Tools that require a range should say so when there isn't one! (David Ogle)

--- Cut-off
14. Option to add 80m at Start, 200m at end for start/end gates in RGT
15. Option to show MR rendering cutoff.
16. Tooltips (where useful)
17. Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
18. Draggable tools?

New stuff:
> This could be like a meta-box, or a "build your own 1CQF", in which
> we pipeline existing features, just like 1CQF.
> E.G. simplify > limit > interpolate > centroid.

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

## Terrain

Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

## Graphs

David Ogle: After using graph theory you end up with out and back track points in the same position... say the orange marker is near the end of the course and I select a track point nearby, it always selects the one at the start of the course, instead of the one I was trying to select near the end. So I'm thinking if you've got 2 track points in the same position, on selection (mouse click), it'd be good to select the one closest to the current position.


---

# Parked

## Map

DEBT: Map sends more click messages each time we click.
> They are debounced but, still, why??

## Laziness, optimisation

Don't render anything that's not visible.
