
# BUGS

Title bar display not showing track name when loaded from URL.

Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

Delete should display "Sorry" message unless one leaf remains.

No preview for single point nudge without fade.

--- 

# WIP

## Refactor Main -> Tools

I hope this will open the door to a much cleaner control flow, perhaps deprecating
actions (yes, after all that) and using the track/newTrack diff to drive command generation.

---

# BACKLOG

## Dark mode

Apply background to all tools.

## Route algebra

Open multiple GPX routes (file, Strava) and combine them in Route Maker.

May not even be that hard, given Route Maker exists.

## Replace MapBox?

https://github.com/klaftertief/slippery-slope

Motivation is to be free of some dodgy JavaScript. 
Free. Forever. 
Or Haskell, I suppose.
Bloody good though, pure Elm.
Not sure how to build it, but I cloned it anyway.

https://github.com/lucamug/elm-map
Might make more sense - more recently updated.

## Picture button

Add button to images to allow screen capture. Because "easy".

## Test cases for edits.

You could do this you know, just not for the visuals.

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

## Languages

Awaiting French support.
Possible sign-ups for German, Dutch, Spanish.
Need more work on number formats.

## Technical debt

Tagged types for Point v Line indices to avoid confusion.

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Put all Font, Colour etc into a Palette/Style module for ease of change.

## Land use 3D rendering

Experiment proved the idea (of roads partitioning polygons) but implementation was weak.
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

# The cellar

## Redo Profile (again)?

https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/
No: renders to SVG not Canvas, so disappointing really.

