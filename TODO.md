
# BUGS

## Named segments

Something certainly wrong with writing segments. Seen it on Hillingdon with three segments.
1. Create a segment A.
2. Create a segment B beyond A.
3. Select segment A in segment list.
4. Move pointers to beyond segment B.
5. "Update" segment A with new locations.

A: Erroneous error message displayed.
B: Written GPX has points out of order.

Maybe am assuming something about the order of the segments not changing.

--- 

# WIP

---

# BACKLOG

## Adding timestamps with physics

Yes, that.

## On refactoring Action interpeter in Main.

Main difference is how the pointers are repositioned.
There's only two or three cases, if not a single general one.

## Load from URL

> CORS issues.

## Languages

Awaiting French support from Muriel.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

## Technical debt

Tagged types for Point v Line indices to avoid confusion.

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Remove tool-specific stuff in Main.performActions. (WIP)

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Land use 3D rendering

Experiment proved the idea but not the implementation.
Roads should divide polygons, but care needed over directionality and crossing points.
It needs doing properly, including the start and finish cases.

## SVG profile rendering

Info on mouse move.
Scales?

## Usability

Drag Curve Former circle directly in Plan View. (Add an SVG "handle" to hit detect.)
Ditto for Move & Stretch, possibly Nudge.
Provide info text capability on top bar and on view panes.

## Tools: old, updated, & new

- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Use localised number formatting everywhere (for French use of , and .)

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

 
---
