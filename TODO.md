
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: In Grid view, Pane1 is notably smaller than the others.

BUG: Hit detect on Map is slow, sometimes very slow. (Paris to Bree).
>>> Something is blowing the stack (try zooming on Artemis to Bree profile)
> Possibly goes away with --optimise compile.
``` main.js:11929 Uncaught RangeError: Maximum call stack size exceeded
    at Function.f (main.js:11929:11)
    at A2 (main.js:56:28)
    at main.js:11925:11
    at main.js:15:54
    at A2 (main.js:56:44)
    at Function.f (main.js:11000:5)
    at A2 (main.js:56:28)
    at Function.f (main.js:11974:8)
    at A2 (main.js:56:28)
    at main.js:11925:11
```

# WIP

## Profile rendering

Not going to try coding now; too late, but can write _about_ what we need...
> It's literate programming, and it really helps.

OK. Both altitude and gradient are rendered with some exaggeration. 
This is because we can scale this _down_ at view time, but cannot increase it. 
The aim is for the altitude vertical scale always to run from minZ to maxZ, regardless of zoom. 

Camera zoom affects X and Y equally.
When the zoom level is low (far away, so the track looks small), the track view
collapses toward the view "lookingAt" point, centre of the viewport.

When the track is small enough to not fill the height, we want two things:
- The base of the track stays at the base of the view;
- The start of the track stays at the left of the view.

We achieve this by, at low zoom levels:
- Raising the "lookAt" point;
- Moving the "lookAt" point along the track 

In other words:
- The lookAt vertically must be centerZ when the height is filled;
- When the height is not filled, minZ will be (minZ - centreZ) / mpp pixels below centre,
- To move this down by the required halfheight - (minZ - centreZ)/mpp, we need to 
- move the lookingAt up by halfheight * mpp - (minZ - centreZ).
- Hence, simple `max` of these might suffice.

Horizontally:
- When current distance / metres per pixel < half viewport width, move lookAt along track.
- More simple, lookAtX is min of half viewport width * metres/pixel and current distance.
- Similar at the right hand side and end of track (max value for lookAtX).

Stop zooming out when track fills the window exactly.
> Use inverse of metres/pixel formula during scroll wheel message update.

Gradient Y scale should be constant; always occupying the height (there's no proportion to maintain).
This requires changing the rendering so that all furthest (low) zoom, it comes out right,
then we always correct using camera elevation. Hmm. For a 100km course, and a 2:1 aspect ration,
that implies a +/- 25km scale. Might as well put % as km.
Would the cosine logic hold up, zooming in on that? We can but try.

## STATUS

Min zoom is good. 
Need to put numbers and ticks inside the axes, and fix margin.
Also fix initial and default zoom.
Will need to refresh zoom after resize, in case it breaks.
Then sort out the min/max for focal point, now ends are fixed.
Panning should then not be able to move the start/end from their respective edges.

Render current point lines using SVG, maybe dotted to both axes.

Display distance, altitude, gradient for current point.

Set initial zoom for profile at the min zoom level determined above.

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
