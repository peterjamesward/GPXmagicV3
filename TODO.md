
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: In Grid view, Pane1 is notably smaller than the others.
BUG: Hit detect on Map is slow, sometimes very slow. (Paris to Bree).

# WIP

## Profile rendering

Can we fix the altitude so that minZ stays at the bottom when zooming far out?
> Would make y scale a tad simpler.

SVG overlay scale.

SVG overlay tracks mouse movement, shows point data.

---

# BACKLOG, roughly in order ...

## Map

Add non-draggable track point circles, in separate layer we can turn on and off
to replace by draggables when that control is open.
(Only leaf points can be draggable!)

Map options tool? 
- Map style (outdoor, satellite)
- Draggable points
> These could just be popups on the map view.

## Tools

Not all existing tools to move across; some rationalisation.
2-way drag should correct for azimuth.

## Error messages
Using an action DisplayMessage to show modal dialog from any tool.

## Memory monitor

Make a tool for this.
Low memory can trigger defensive actions: remove Undo entries, reduce rendering depth, reduce graphics.
If we are so short of memory that we can't create the output string, one option is to turn off graphics,
another to output in sections. Another, to drop the scenes, reclaim some heap, defer writing to an 
action, then re-render. This would also work for edits, but hairy.

## Plan view

Same as v2. Use 3d-scene. Orthographic camera.

## Terrain

Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.

?? Move `scene(s)` into Pane Layout; they can be rendered only if visible.

## Graduated Rendering

Use a graduated drop off with distance, compute at each tree level?
For each node, take minimum distance from bounding box to current.
Set depth = (say) 20 - distance in km, down to 10. (Will never have 1M points.)

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

## Graph

Oh yes. Basically sound, unless there's something I've not yet thought about.

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
(Parked as needless optimisation given apparent speed of progressive rendering.)
