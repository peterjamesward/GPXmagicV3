
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: Last line at right hand edge in Profile is weird. (JS trace suggests a divide by zero somewhere!)
BUG: Map sends more click messages each time we click. They are debounced but, still.
BUG: Undo delete of 100,000 points and it doesn't render well. Suspect tree unbalanced.

# WIP

Restore Previews!!

Time for a quick update video?

---

# BACKLOG, roughly in order ...

## Tools

Not all existing tools to move across; some rationalisation.
2-way drag should correct for azimuth.
Need to think about new "general purpose make-it-so smoother".

### Order of play

1. Bezier
2. Centroid average
3. Curve former
4. Classic bend smoother
5. Nudge
6. Move & Stretch
7. Gradient problems
8. Steep climbs
9. Intersections
10. Flythrough
11. Limit gradients
12. Segment info
13. Graph
14. Loops (inc Out and Back)
15. Visual options

## Error messages

Using an action DisplayMessage to show modal dialog from any tool. 
Add a non-modal message error for info.

## Plan view

Same as v2. Use 3d-scene. Orthographic camera.

## First person view

Same as v2. Use 3d-scene. Orthographic camera.

## Map

Draggable points
> These could just be popups on the map view. (Layers, Cross-arrow, resp.)

## Terrain

Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Loop detection

**JB**: I have been getting a few Partner event gpx's lately that do a loop... but then continue around for say 25% of it before finishing which when a map is first loaded i do not notice until i start working on it... it would be nice if when a map is first loaded the points show a different colour when there is another course on top.. ie orange for the first lap but if it continues say red until it finishes..

## Graph

Oh yes. Basically sound, unless there's something I've not yet thought about.

## Laziness, optimisation

Don't render anything that's not visible.

## Memory monitor

Make a tool for this. Quite important and useful.
Low memory could one day trigger defensive actions: remove Undo entries, reduce rendering depth, reduce graphics.
If we are so short of memory that we can't create the output string, one option is to turn off graphics,
another to output in sections. Another, to drop the scenes, reclaim some heap, defer writing to an
action, then re-render. This would also work for edits, but hairy.

---

