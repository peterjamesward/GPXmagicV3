

# 2.9 :: Optimisations?

## What? Why?

Yes, because Elm is great comxpared to Racket.
> Indications are that going back to basics on data structure is better than viable, it's awesome.

Put all GPXPoints into a binary tree. At each level hold:
- (height gained, lost, max gradient up/down, ...)
  Use this for **everything**.
- Selective rendering
- Click detection
- Terrain
- i.e. Replaces SpatialIndex (even if less optimal).

This is could be good.
All edits are "local" (only "up the tree").

Keep GPXPoints for Undo, not reverse delta.

Operations on a PeteTree
- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves (and all their aggregate nodes)
- Box content query
- Box nearest query
- Queries with filters
- Queries with folds
- Render to specified tree depth
- Render to varying depth based on bounding box
- See if visible area can best be done by pre-selecting view elements, or left to GPU.
- (culling view frustrum tricky combination of plane/bbox intersects and axis distance?)


Possibly vary rendering with zoom level (maybe not constantly), and even on rotate/drag end.

Marker is a "zip" along a tree path, or just a number?
- Purple is locked to Orange unless dropped, so no Maybe there.

Test with 1M TP (stretch and interpolate, not easy to create such a track!) .

More progress along the "plug-in" tool pattern:
- Tab Open and Close methods.
- Generic type somehow for storing tool state (JSON session state?)

Use an SVG overlay for markers, maybe previews?


---


