

# WIP

### Selective rendering
Add selective rendering near current.

### Notes
Do a write-up on Medium about why this structure is so cool (in this context).

### Culling
- See if visible area can best be done by pre-selecting view elements, or left to GPU.
- (optimal culling view frustrum tricky combination of plane/bbox intersects and axis distance?)

---

# Backlog

### Click detection

### Terrain

### Undo/Redo
Keep GPXPoints for Undo, not reverse delta (2.7, not 2.8)

### Queries
- Box content query
- Box nearest query
- Queries with filters
- Queries with folds

### (Algebraic) operations
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes)  
  (note this is actually a top-down operation)

### SVG markers?
Use an SVG overlay for markers? No, but maybe previews?

### Scale test
Test with 1M TP (ask Steve).

### Tools
More progress along the "plug-in" tool pattern:
- Tab Open and Close methods.
- Generic type somehow for storing tool state (JSON session state?)

### Multiple windows

Check out Electron (again), see if that will allow multiple windows. (Dual monitor support wbn).

Put back visual options  
... but allow override for each window (too complex?).

---


