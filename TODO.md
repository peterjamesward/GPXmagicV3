
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: Hit detect is poor on 304K course (maybe not surprising but it should work).

# WIP

---

# BACKLOG, roughly in order ...

Make **DragPan** work on 3D view.

Third person click should not re-centre the view.
> Does so now because we are in "follow" mode.

Double-click, in contrast, always centres.
> More generally, don't centre _the view you click on_.
> Other views should centre when in "follow" mode.

Put all Font, Colour etc into a Palette/Style module for ease of change.

Need "restore to defaults" for tools.

## Profile & Charts
Details in Backlog.

## Map
Add non-draggable track point circles, in separate layer we can turn on and off
to replace by draggables when that control is open.

Map options tool? 
- Map style
- Map follow orange pointer
- Draggable points

## Tools

All existing tools to move across, with some rationalisation.
2-way drag should correct for azimuth.
Pane payout choices as control?
I think the top bar is still special: Load, Save, Donate. Is it?
"Where am I" - reverse geocode (rate limited)).

## Look and Feel
Configurable background colour.
Imperial measures option.

## Error messages
Using an action DisplayMessage to show modal dialog from any tool.

## Plan view
Same as before. Use 3d-scene. Orthographic camera.

## Profile
Not 3d, just drawing onto Canvas?
(But what about zoom and pan?)
Altitude change from start, end; trueLength.
> Combine with Charts, by adding the colours, ideally. See below.

## Charts
D3 with https://github.com/seliopou/elm-d3/blob/master/README.md ?
(Only need to write the JS once, then just pass data.)
https://observablehq.com/@d3/gradient-encoding
https://observablehq.com/@d3/line-with-tooltip
https://observablehq.com/@d3/pannable-chart
https://observablehq.com/@d3/zoomable-area-chart
... and there's this ... https://blog.scottlogic.com/2020/05/01/rendering-one-million-points-with-d3.html

https://medium.com/@ColinEberhardt/extending-d3-with-higher-order-components-d58cd40b7efd

Remember that most routes will be <100K, not 1M points!
Therefore, Canvas may be best option.
Or, given Elm, and the current charting library, and the nature of the tree, we could probably
show a decent zoomable plot that perhaps shows error bars or a candle plot for non-leaf tree entries.
Though, by the same argument, we could ship 10K points to JS no problem.
PARK THIS FOR NOW; we have options!

## Terrain
Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Graduated Rendering
Use a graduated drop off with distance, compute at each tree level?
On Map, work out the depth to fit (say) 1000 points within visible region.
> Not sure it's required.

---

# Parked (and probably abandoned)

## Journal edits to indexedDB?
Potential for recovery by replay.
Possible aid to isolating and reproducing bugs.

## PeteTreeTraversor
Data structure that keeps track of where you are in a tree.
When 'applied' to a tree, returns a tree element and the traversor (generator) for the next one.
Symmetry allows left->right and right->left traversal.
I sense that having this will make some filtering easier.
Also applies to rendering, where we have hand-coded R->L trversals.
> But just having a traversal function with callbacks is probly adequate.

### Multiple windows (demoted again; more value in better tool layout and customisation)
Tauri looks interesting as we would not need node.js
> https://tauri.studio/en/docs/getting-started/intro/

### View culling other than in WebGL.
- See if visible area can best be done by pre-selecting view elements, or left to GPU.
- (optimal culling view frustrum tricky combination of plane/bbox intersects and axis distance?)

### Non-WebGL markers?
Use a Canvas overlay for markers?
> Shall we test this idea with a popup, then adopt or abandon?
(Parked as needless optimisation given apparent speed of rendering.)

### New Plan view
Not 3d, just drawing on Canvas.
https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
Not sure, since we have the scene, so it's just orthographic view and we get free zoom & pan!

