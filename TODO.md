
# BUGS

--- 

# WIP

Jan Deca:

> Hello Peter, I wanted to log a quick feature request. In the current Split&Join tool of GPXmagic 
> you can happily split routes (which is amazing). The tool currently splits a route in say X equal 
> chunks. It would be awesome to be able to set how the route is split. For example, I'm currently 
> building a 160km road, which has a big climb between 70-90 km. The current tool would split the 
> road at 80km, i.e., halfway up the climb. There are workarounds by using, e.g., the delete tool, 
> but it'd be awesome to have the possibility to tell the tool to split the road at 90km, in order 
> to keep the climb intact. Thanks a bunch!

= Add "Split at Orange" button.

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
