
# BUGS (for an 'easy' day)

BUG: Dubious steepest gradient on some routes. (May have to wait until we have that tool!)
BUG: In Grid view, Pane1 is notably smaller than the others.

**BUG**: Hit detect on Map is slow, sometimes very slow. (Paris to Bree).
Can take tens of seconds but seems to recover.
Does each successive search take longer ?????

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

Need:
* pan
* click 
* double click
* Can we differentially colour the area under the line? (I suspect not.)
* Add in the last point to the rendering after the fold
* Layout needs work
* Resizing

Drag might be nuisance but the library exposes events so we should be good.
https://elm-charts.org/documentation/interactivity/zoom

---

# BACKLOG, roughly in order ...

## Map

Add non-draggable track point circles, in separate layer we can turn on and off
to replace by draggables when requested.
(Only leaf points can be draggable; perhaps distinguish by colour.)

Map options tool? 
- Map style (outdoor, satellite)
- Draggable points
> These could just be popups on the map view. (Layers, Cross-arrow, resp.)

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

## Improvement for drag detect

Note this little pattern that looks for movement rather than use a timer:
```elm
    OnMouseMove offset ->
      case model.dragging of
        CouldStillBeClick prevOffset ->
          if prevOffset == offset then
            model
          else
            { model | center = updateCenter model.center prevOffset offset
            , dragging = ForSureDragging offset
            }

        ForSureDragging prevOffset ->
          { model | center = updateCenter model.center prevOffset offset
          , dragging = ForSureDragging offset
          }

        None ->
          model
```

---

