

# 2.9 :: Optimisations?

## What? Why?
> Yes, because Elm is great comxpared to Racket.
> I can't be sure about slippy maos or equivalent of scene3d in Racket, Haskell or F#.
> So my thoughts of native app are formally on hold, pending revelation.

Put all GPXPoints into a binary tree. At each level hold:
- Total length
- Point count
- Bounding box
- (height gained, lost, max gradient up/down, ...)
  Use this for **everything**.
- Selective rendering
- Click detection
- Terrain
- i.e. Replaces SpatialIndex (even if less optimal).

This is could be good.
All edits are "local" (only "up the tree").
Keep GPXPoints for Undo, not reverse delta.


- Delete a leaf/leaves
- Insert a leaf/leaves
- Update leaves
- Box content query
- Box nearest query
- Queries with filters
- Queries with folds
- Render to specified tree depth
- Render to varying depth based on bounding box
- See if visible area can best be done by pre-selecting view elements, or left to GPU.


Possibly vary rendering with zoom level (maybe not constantly), and even on rotate/drag end.
Marker is a "zip" along a tree path.
- Purple is locked to Orange unless dropped, so no Maybe there.

Test with 1M TP (stretch and interpolate, easy).

More progress along the "plug-in" tool pattern:
- Tab Open and Close methods.
- Generic type somehow for storing tool state (JSON session state?)

Use an SVG overlay for markers, maybe previews.

WHen reducing track detail, don't construct the reduced track and then render. Instead,
skip along when rendering. (With new list, don't recurse to the leaves).

---


