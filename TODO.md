

# WIP

Will go back to **conformal** projection.
Add option to switch to **Web Mercator**.
Keep the longitude extents, as needed for Map display culling and hit detection.

## Zoom, Pan, Rotate.
Copy old stuff when conformal projection is back.

### Map / General rendering
Use a graduated drop off with distance, compute at each tree level.
On Map, work out the depth to fit (say) 1000 points within visible region.

### iframe
Do an embedded version as iframe for Steve?
Binary file format for fastest load?
> https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/

---

# Backlog

### Multiple windows
Check out Electron (again), see if that will allow multiple windows.
> Keep putting this back. I worried that the internal comms might kill it,
> but it remains an attractive option.
> Second window would be view only.

### Decide how to organise windows, panes, views.
What state lives where?
Also, need to see if we can live without PostUpdateAction, just let the tools do their thing.
First pane is special, because of map, and because you must have it.
Hence, one pane + list, or just four panes with visibility?

### Look and Feel
Clean.
Chinese FlatUI palette?
Keep current track in indexDB to support a "restart/recover" option?
JB suggests heavily customisable to support differing "workflows".

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
So far, looking awesome at 937K (!).

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

