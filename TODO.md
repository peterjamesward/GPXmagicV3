

# WIP

Lazy selective re-render to reduce map flicker? (aka 2.8)

### Round the world problems
1. Crosses the data line, so we get weird bounding boxes.
2. Defeats the assumption of locality for conformal projection.
> Need to compute deltaLongitude from start.
> Need "nearness" to use subtended angle or earth distance when culling.

### Map
Do an embedded version as iframe for Steve?
Put track on Map only when track is shown!
Possibly try a more graduated selective rendering. (Maybe 100km box at 14 deep?)
Fix that initial map size problem (was a pain on V1).

---

# Backlog

### Decide how to organise windows, panes, views.
What state lives where?
Also, need to see if we can live without PostUpdateAction, just let the tools do their thing.
First pane is special, because of map, and because you must have it.
Hence, one pane + list, or just four panes with visibility?

### Multiple windows
Check out Electron (again), see if that will allow multiple windows.
> Keep putting this back. I worried that the internal comms might kill it,
> but it remains an attractive option.
> Second window would be view only.

### Look and Feel
Clean.
Chinese FlatUI palette?
Keep current track in indexDB to support a "restart/recover" option.

### Plan view
Same as before. Use 3d-scene. Orthographic camera.

### Profile
Not 3d, just drawing onto Canvas?
(But what about zoom and pan?)
Altitude change from start, end; trueLength.
Combine with Charts, by adding the colours, ideally.

### Rotate, Zoom, Pan
This is copy & paste.

### Terrain
Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <loops>.
(Expand bounding boxes to allow for road width.)

### Undo/Redo
Revert to keeping GPXPoints for Undo, not reverse delta (2.7, not 2.8).
Use Session state, to avoid taking up ram, with unlimited Undo.

### (Algebraic) operations
As and when needed:
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes)  
  (note this is actually a top-down operation)

### Scale test
Test with 1M TP (ask Steve).
So far, looking awesome at 380K.

### Tools
More progress along the "plug-in" tool pattern:
- Tab Open and Close methods, and makePreview.
- Generic type somehow for storing tool state (JSON session state?)

---

# Parked

### Culling?
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

