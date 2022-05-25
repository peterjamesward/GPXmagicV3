
# BUGS

(Nothing urgent)

--- 

# WIP

## Route Maker

Use a SpatialIndex to look for any collinear points from "other" road segments and use them
to divide each segment into two (or more) sections. Then when we run the neighbour counting
we should have consistent results.

    divideAtCollinearPoints : SpatialIndex -> RoadSection -> List RoadSection/GPXSource

- With variable tolerance on the co-linearity test, this may work better with IRL rides.
- Altitude is by interpolation on "current" section, not from outlier.

Improve traversal direction display (with an arrow on each section?).

Retrofit this approach into Intersections.

---

# BACKLOG

## Tools organisation

**Filter** by tag: Curves, Gradients, Issues etc. Each tool may have more than one tag.

## Languages

Awaiting French support from Muriel.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

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

## Technical debt

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Try to move out the residual tool-specific stuff in Main.performActions.

Put all Font, Colour etc into a Palette/Style module for ease of change.

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
