
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

**BUG**: Sometimes will not display file open dialog. Also splitters stop working.
> Debuggers shows messages arriving, not obvious why they should not be processed.
> Can't save file when this happens, so rather poor show.

--

# WIP

## Intersection detection 

OK. Quadtree is magnitudes quicker. Wrong, but that's probably fixable.

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

---

# BACKLOG, roughly in order ...

## Tools: old, updated, & new

1. Graph Theory (notes below and from DO's emails)
2. Dragging the 3D view turns off the lock

--- _Cut-off for release_
- Info popup for all tools.
- SVG for previews on Profile - get nice smooth lines.
- SVG overlay on 3d views.
- Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Extract all text for translation (Muriel)
- Use localised number formatting everywhere (for French use of , and .)
- Terrain (with texture maybe)
- Ability to point-smooth transitions over a range
- Tools that require a range should say so when there isn't one! (David Ogle)

--- _Cut-off_
- Option to show MR rendering cutoff.
- Tooltips (where useful)
- Draggable tools?

New stuff:
> This could be like a meta-box, or a "build your own 1CQF", in which
> we pipeline existing features, just like 1CQF.
> E.G. simplify > limit > interpolate > centroid.

## Offset/nudge logic

Just don't blindly mitre. For each pair of RoadSection, see if the points will
"overlap" and don't emit them all. May need some interpolation for altitude or whatever.
> Perhaps just try Bezier on interior turns.

## Terrain

Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

## Small stuff

Maybe convert all markers to SVG with overlay.

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.
 
The preview traversal for Limit Gradients should be width-limited as we zoom in,
with depth reflecting the zoom level, as for the main track.

## Graphs

David Ogle: After using graph theory you end up with out and back track points in the same position... say the orange marker is near the end of the course and I select a track point nearby, it always selects the one at the start of the course, instead of the one I was trying to select near the end. So I'm thinking if you've got 2 track points in the same position, on selection (mouse click), it'd be good to select the one closest to the current position.


---

# Parked

## Map

DEBT: Map sends more click messages each time we click.
> They are debounced but, still, why??

## Laziness, optimisation

Don't render anything that's not visible.
