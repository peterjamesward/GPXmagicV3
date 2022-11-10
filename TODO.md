
# BUGS

--- 

# WIP

---

# BACKLOG

## Load from URL

This needs sorting as RGT may get round to providing an "Edit" link one day.
Could also ask the `magicroads.com` people to add an "open in GPXmagic" link.

## Make Map changes declarative; tidy Action code

This would largely but not entirely obviate Main.performActionCommands.

Since the old and new track (not just the tree) are available post-update,
I could 'diff' them and output the commands required.

Don't need to diff the tree, as any track change requires re-drawing the whole route on the map.
(Presuming in Elm that tree /= tree'.)

What changes are there that reflect on the map?
See MapPortController and Main.performActionCommands.
Any changes to route, pointers, preview, and of course the map settings.

**This is a worthwhile 'experiment'**

It may also be the key to unlock the Actions space, which has become untidy.
e.g. Main.performActionsOnModel is largely about positioning pointers post-edit!
There are some cross-module cases solved with Actions to avoid import cycles,
but they might factor out.

**Approach**

1. Rework MapController so comparison of states emits the commands; better: MapOptions.
2. Purge the Actions code so that model is updated directly.
3. Repeat for other actions.

## Estimate ride duration

Show estimated time for route given wattage and rider weight.
Not full physics. momentum-free, just allow for gradient on each section.

## On refactoring Action interpreter in Main.

Main difference is how the pointers are repositioned.
There's only two or three cases, if not a single general one.

## Languages

Awaiting French support.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

## Technical debt

Tagged types for Point v Line indices to avoid confusion.

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

## Tools: old, updated, & new

- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Use localised number formatting everywhere (for French use of , and .)

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

 
---
