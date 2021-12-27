

# 2.9 :: Optimisations?

## WIP

Add position slider, maintain current position.
Add selective rendering near current.
Add position marker to rendering (if re-rendering, no point trying SVG).

## What? Why?

Yes, because Elm is great compared to Racket.
> Indications are that going back to basics on data structure is better than viable, it's awesome.

Put all GPXPoints into a binary tree. At each level hold:
- (height gained, lost, max gradient up/down, ...)

Use this single structure for **everything**:
- Selective rendering
- Click detection
- Terrain
- i.e. Replaces SpatialIndex (even if less optimal).

This is could be good.
All edits are "local" (only "up the tree").

Keep GPXPoints for Undo, not reverse delta.

Queries
- Box content query
- Box nearest query
- Queries with filters
- Queries with folds

(Algebraic) operations
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes) (note this is actually a top-down operation)

- Render to specified tree depth (done, nice).
- Render to varying depth based on bounding box
- See if visible area can best be done by pre-selecting view elements, or left to GPU.
- (optimal culling view frustrum tricky combination of plane/bbox intersects and axis distance?)

Possibly vary rendering with zoom level (maybe not constantly), and even on rotate/drag end.

Marker is a "zip" along a tree path, or just a number? (Number)
- Purple is locked to Orange unless dropped, so no Maybe there?

Test with 1M TP (ask Steve).

More progress along the "plug-in" tool pattern:
- Tab Open and Close methods.
- Generic type somehow for storing tool state (JSON session state?)

Use an SVG overlay for markers? No, but maybe previews?


---


