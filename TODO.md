

# WIP

---

# Backlog

### Decide how to organise windows, panes, views.
What state lives where?
Also, need to see if we can live without PostUpdateAction, just let the tools do their thing.
First pane is special, because of map, and because you must have it.
Hence, one pane + list, or just four panes with visibility?

### Map
Mapbox should play well with PeteTree rendering.
Need to think about where and how to generate commands.

### Plan view
Same as before. Use 3d-scene. Orthographic camera.

### Profile
Not 3d, just drawing onto Canvas?
(But what about zoom and pan?)
Altitude change from start, end; trueLength.
Combine with Charts, by adding the colours, ideally.

### Terrain
Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <loops>.
(Expand bounding boxes to allow for road width.)

### Rotate, Zoom, Pan
This is copy & paste.

### Undo/Redo
Revert to keeping GPXPoints for Undo, not reverse delta (2.7, not 2.8)

### (Algebraic) operations
As and when needed:
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes)  
  (note this is actually a top-down operation)

### Scale test
Test with 1M TP (ask Steve).
So far, looking awesome at 300K.

### Tools
More progress along the "plug-in" tool pattern:
- Tab Open and Close methods.
- Generic type somehow for storing tool state (JSON session state?)

### Culling?
- See if visible area can best be done by pre-selecting view elements, or left to GPU.
- (optimal culling view frustrum tricky combination of plane/bbox intersects and axis distance?)

### Multiple windows
Check out Electron (again), see if that will allow multiple windows. 
> (Dual monitor support would be nice but not as good as being light and fast.)

---

# Parked

### Non-WebGL markers?
Use a Canvas overlay for markers?
> Shall we test this idea with a popup, then adopt or abandon?
(Parked as needless optimisation given apparent speed of rendering.)

### New Plan view
Not 3d, just drawing on Canvas.
https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
Not sure, since we have the scene, so it's just orthographic view and we get free zoom & pan!

