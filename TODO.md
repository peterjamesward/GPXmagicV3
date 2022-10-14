
# BUGS

--- 

# WIP

## Use POIs for named segments

1. Distinguish between Auto and Manual segments.
2. Do not erase Manual.
3. Edit Auto becomes Manual.
4. Do not erase Auto when disable auto mode.
5. Try highlight showing all starts & ends.

---

# BACKLOG

## Estimate duration

Show estimated time for route given wattage and rider weight.
Not full physics, just allow for gradient on each section.

## On refactoring Action interpreter in Main.

Main difference is how the pointers are repositioned.
There's only two or three cases, if not a single general one.

## Load from URL

> CORS issues.

## Languages

Awaiting French support.
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
