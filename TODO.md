

# WIP

Figure out how to do actions and commands without import loops.
May involve quite a lot of wrappping and delegation (if you raise a msg, you handle it).
> All messaging in & out is map-related (ignoring OAuth), so a well-sorted MapView.elm might fix it all.

Projection distorted (been there before).

Hit detect needs to set current. (Does, but does not repaint, I think.)

Map is messed up (not surprising).

## Zoom, Pan, Rotate.
Copy old stuff when conformal projection is back, but cleaner.

### Map / General rendering
Use a graduated drop off with distance, compute at each tree level.
On Map, work out the depth to fit (say) 1000 points within visible region.

### Editing?
Should probably try an edit function soon, to get the feel of working with PeteTree.
Also, to decide how to replace `processPostUpdateAction`.

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
- Highly configurable (John Bytheway).

### iframe
Do an embedded version of Map with Round the World route as iframe for Steve?
Binary file format for fastest load?
> https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/

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

