

# WIP

### Notes
Do a write-up on Medium about why this structure is so cool (in this context).

---

# Backlog

### Decide how to organise windows, panes, views.
What state lives where?

### Plan view
Not 3d, just drawing, SVG or onto Canvas (better).
https://package.elm-lang.org/packages/joakin/elm-canvas/latest/

### Profile
Not 3d, just drawing, SVG or onto Canvas (better).
Altitude change from start, end; trueLength.

### Terrain
Terrain 1 = Simple tree walk, in many cases will just work but not always.
Terrain 2 = Tree walk combined with whole (visible) tree query, because <loops>.
(Expand bounding boxes to allow for road width.)

### Rotate, Zoom, Pan
This is copy & paste.

### Undo/Redo
Revert to keeping GPXPoints for Undo, not reverse delta (2.7, not 2.8)

### (Algebraic) operations
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes)  
  (note this is actually a top-down operation)

### Non-WebGL markers?
Use an SVG overlay for markers? 
No, but maybe previews?
And not SVG but Canvas.

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


