
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: In Grid view, Pane1 is higher than the others.
BUG: Last line at right hand edge in Profile is weird. (JS trace suggests a divide by zero somewhere!)
BUG: Map sends more click messages each time we click. They are debounced but, still.

# WIP

---

Time for a quick update video?

# BACKLOG, roughly in order ...

## Map

Add non-draggable track point circles, in separate layer we can turn on and off
to replace by draggables when requested.
(Only leaf points can be draggable; perhaps distinguish by colour.)

Map options tool? 
- Map style (outdoor, satellite)
- Draggable points
> These could just be popups on the map view. (Layers, Cross-arrow, resp.)

## Laziness

Don't render profile if not visible.

## Tools

Not all existing tools to move across; some rationalisation.
2-way drag should correct for azimuth.

## Error messages
Using an action DisplayMessage to show modal dialog from any tool. 
Add a non-modal message error for info.

## Memory monitor

Make a tool for this. Quite important and useful.
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

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

## Graph

Oh yes. Basically sound, unless there's something I've not yet thought about.


---

