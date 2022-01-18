# BUGS

BUG: Change colour on closed tab shows preview.

BUG: Dubious steepest gradient on some routes.

BUG: Dubious (obviously wrong) direction changes reported.

# WIP

Save & Restore of splitter and tool settings (basis in v2).

## NEXT UP:

# BACKLOG, roughly in order

## Delete with a range (nearly trivial).
Use `count from start` and `count from end` to denote the portion for deletion.

## Undo, Redo. (Do journaling here?)
Decide on approach (more 2.7 than 2.8)

## Multiple views

## DEBT

Split **DomainModel** into core and various helpers.

Make **DragPan** work on 3D view.

Third person click should not re-centre the view.
> Does so now because we are in "follow" mode.

Double-click, in contrast, always centres.
> More generally, don't centre _the view you click on_.
> Other views should centre when in "follow" mode.

Put all Font, Colour etc into a Palette/Style module for ease of change.
Make Tool titles 'black' or 'white' using greyscale formula.

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



## Layout

Splitters need tweaking later. Might switch to pixel sizing.
Will need to go in localStorage.

## Tools

2-way drag corrects for azimuth.
Pane payout choices as control?
I think the top bar is still special: Load, Save, Donate. Is it?
"Where am I" - reverse geocode (rate limited)).

## Rendering
Use a graduated drop off with distance, compute at each tree level?
On Map, work out the depth to fit (say) 1000 points within visible region.
> Done that it RTWI80D.

## Decide how to organise windows, panes, views.
What state lives where? (This relates to the multi-window decision.)
First pane is special, because of map, and because you must have it.
Hence, one pane + list, or just four panes with visibility?

## Look and Feel
Configurable background colour.
Keep current track in indexDB to support a "restart/recover" option? 
> No - journal the edits.

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
Use Session state, to avoid taking up ram, with unlimited Undo?
> Possibility, if fast enough, that we replay journaled edits from last checkpoint (last save).
> (Because user can always load that file anyway.) Ah, but suppose you want to go further back?
> I guess we could ask the user to re-open the relevant baseline file?


---

# Parked (and probably abandoned)

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

