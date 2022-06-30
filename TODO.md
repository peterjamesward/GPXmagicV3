
# BUGS

--- 

# WIP

---

# BACKLOG

## Route maker

Delete Place in context menu if exactly two roads (can happen after Road delete) 
-OR- do this automatically after Road delete.
In either case, combine two Roads into one, allowing for directions of course.

> Needs debugging.

## Load

Download from URL.
> CORS issues.

## Tools organisation

**Filter** by tag:
- Curves,
- Gradients,
- Issues,
- ???

Each tool may have more than one tag.

## Languages

Awaiting French support from Muriel.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

## Technical debt

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Remove tool-specific stuff in Main.performActions.

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
Specific areas within tools as needed.

## De-noise / Simplify

This could be improved. Should preferentially remove any points where the triangle has a large
deflection and a short baseline, not just based on area. Also, an effective quality filter
would be to restrict the angular change in both planes. Will do this prior to curvature analysis.
> Obviated by smart smoother?

## Tools: old, updated, & new

- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Use localised number formatting everywhere (for French use of , and .)

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

 
---
