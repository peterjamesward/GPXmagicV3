

# WIP

## Layout
Concept test of five (!) dock zones, with multiple adjustable splits.
> Looking OK. Need to scale internally, and ideally get the actual window size when we start.
> Should be able to reduce docks to marginal strip, with click to open/close.

> Try making the single tab available move around.
> Use localStorage to save splitter and tool tab layout (as v2).

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Zoom, Pan, Rotate.
Panning feels slippy. Is the scaling wrong?
All to do with getting the right focal point?
> Think about what should remain under the mouse as it moves, and make it so.

## Editing?
>> Take a couple of tools, do them as better tabs:
 - dockable any side, via pop-up tools icon
 - user choice of colour, ditto
 - preset layout options in "menu bar"
 - less space for view selection -- popup? e.g. feather icon with "eye".

 - fully pluggable tools (lots of functions to interact with tabs, like OO)
 - previews in all views, each tool is fully responsible (but with shared helpers).
 - validate Actions concept,
 - decide best approach to Undo & Redo (likely GPXSource delta to Undo, function to Redo)
 - (journaling in localStorage would allow recovery by replay?)

Let's start with Bend/Gradient problems, showing on 3D and map, then Delete.
> Thinking of not listing problems in the tab, because affects tab size greatly.

## Profile & Charts
Details in Backlog.

---

# Backlog

## Rendering
Use a graduated drop off with distance, compute at each tree level.
On Map, work out the depth to fit (say) 1000 points within visible region.

## Decide how to organise windows, panes, views.
What state lives where? (This relates to the multi-window decision.)
Also, need to see if we can live without PostUpdateAction, just let the tools do their thing.
First pane is special, because of map, and because you must have it.
Hence, one pane + list, or just four panes with visibility?

## Look and Feel
Clean.
Menu bar?
Chinese FlatUI palette?
Keep current track in indexDB to support a "restart/recover" option?
JB suggests heavily customisable tools layout to support differing "workflows".

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

Remember that most routes will be <10K, not 1M points!
Therefore, Canvas may be best option.
Or, given Elm, and the current charting library, and the nature of the tree, we could probably
show a decent zoomable plot that perhaps shows error bars or a candle plot for non-leaf tree entries.
Though, by the same argument, we could ship 10K points to JS no problem.
PARK THIS FOR NOW; we have options!

## Terrain
Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <loops>.
(Expand bounding boxes to allow for road width.)

## Undo/Redo
Revert to keeping GPXPoints for Undo, not reverse delta (2.7, not 2.8).
Use Session state, to avoid taking up ram, with unlimited Undo.

## (Algebraic) operations
As and when needed:
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes)  
  (note this is actually a top-down operation)

## Scale test
Test with 1M TP (ask Steve).
So far, looking awesome at 937K (!).

## iframe
Do an embedded version of Map with Round the World route as iframe for Steve?
Binary file format for fastest load?
> https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/

---

# Parked

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

