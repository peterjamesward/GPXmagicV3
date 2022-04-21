
# WIP

## I18N

Awaiting French support from Muriel.

## Land use 3D rendering

Divide polygons along road edges.
I suspect that OSM may not be consistent about polygon direction.
How do I know? Can I deal with it?
Maybe I can, because I can figure out the direction of the polygon edge that crosses the road.
In fact, this may make it easier to just "follow the edges".

---

# BACKLOG

## RGT: Timed segments

If Christian can supply tags, tool to list and edit Time Segments, defined by markers
and nameable; these will be embedded in the output GPX.

## RGT: Integrate with track DB back-end

Launch with track URL, save does a POST.

## RGT: Black-box mode

Runs server-side (Node.js?), receives track via POST, returns smoothed.
> `elm-serverless` can do this, may be other ways.

## SVG profile rendering

Info on mouse move.
Scales?

## Route Builder

- Improve traversal direction display (with an arrow on each section?).
- Add "split edge at pointer" function, when editing an edge (?).
- Improve behaviour with IRL rides (use code from Intersections?)

## Usability

Drag Curve Former circle directly in Plan View. (Add an SVG "handle" to hit detect.)
Ditto for Move & Stretch, possibly Nudge.
Provide info text capability on top bar and on view panes.
Specific areas within tools as needed.

## Technical debt

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Try to move out the residual tool-specific stuff in Main.performActions.

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

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.
 
---
