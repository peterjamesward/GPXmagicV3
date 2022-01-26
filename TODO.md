
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: In Grid view, Pane1 is notably smaller than the others.

# WIP

## Profile, Charts

I have decided to abandon the D3|D3FC route, and revert 
to the current 3d-scene approach with some changes:

Colour to look more like a conventional plot - lines and subtle shading.
Two views, one for altitude, one for gradient;
(Could put them in one scene, e.g. alt on YZ plane, gradient on XY;)
Both use orthographic views;
Use progressive rendering;
Show current as vertical orange line, through both views;
When zooming, decline the camera so that the vertical scale remains the same;
Track box diagonal to work out visible area;
Use visible area to create an SVG scales overlay;
Further SVG overlay for current point details.

---

# BACKLOG, roughly in order ...

## Graduated Rendering

Use a graduated drop off with distance, compute at each tree level?
For each node, take minimum distance from bounding box to current.
Set depth = (roughly) 20 - distance in km, down to 10.

## Map
Add non-draggable track point circles, in separate layer we can turn on and off
to replace by draggables when that control is open.
Try to reduce repaints on moving pointer. 
> Could we be lazy? Only repaint on slider thumb release with custom thumb events?

Map options tool? 
- Map style (outdoor, satellite)
- Draggable points

## Tools
Not all existing tools to move across; some rationalisation.
2-way drag should correct for azimuth.

## Error messages
Using an action DisplayMessage to show modal dialog from any tool.

## Memory monitor
Make a tool for this.
Low memory can trigger defensive actions: remove Undo entries, reduce rendering depth, reduce graphics.
If we are so short of memory that we can't create the output string, one option is to turn off graphics,
another to output in sections.

## Plan view
Same as v2. Use 3d-scene. Orthographic camera.

## Terrain
Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff
Put all Font, Colour etc into a Palette/Style module for ease of change.

## Loop detection
**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

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
> But just having a traversal function with callbacks is probly adequate; so this is internal.

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

