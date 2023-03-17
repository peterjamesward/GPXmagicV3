
# BUGS

**Stretch** Redo of Stretch operation sometimes gives error on track (e.g. Lacets).

**Delete** Delete should display "Sorry" message unless one leaf remains.

BUG: "Error: Invalid value for <circle> attribute cy="NaN"" -- what triggers this?

BUG: Right split not being saved/restored?

--- 


# WIP

## Route Builder

4. Map view not re-rendering after route conversion.
> More a question of the original route not being removed after decomposition!
> Some actions reliably show the right track on the map; be consistent!
5. Undo for create new track from route.
> Consider a Tracks level Undo mini-stack. Store Tracks not Graph.
6. Any change to track collection must invalidate nodes.
7. Check all flows and accompanying text.
8. Video(s).

---

# BACKLOG

## Segment output

Semantically, endpoints should be duplicated between segments, but apparently RGT barfs.
Thus, provide option to output duplicate points but leave default as is until RGT complies.
(Dan Connelly)

## Scientific notation in GPX

If not too hard, support for import (not output) of really, really small longitude and latitude.
May be as simple as changing the regex.
> Decimal is (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)
> Float is (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee](\+|-)?[0-9]+)? |(\+|-)?INF|NaN

## Tools summary

Order alphabetically. Ability to jump into tool directly.

## Refactor Main -> Tools

I hope this will open the door to a much cleaner control flow, perhaps deprecating
actions (yes, after all that) and using the track/newTrack diff to drive command generation.

## Camera

Full control of camera in relation to marker.

## Picture button

Add button to images to allow screen capture. Because "easy".

## Test cases for edits.

You could do this you know, just not for the visuals.

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

## Street view

Nope, can't be done in an iFrame. Google stops it.

## LIDAR

Direct reading of LIDAR data.
-- Nope. Not enough data, tricky to access, mostly not free.

## Replace MapBox?

https://github.com/klaftertief/slippery-slope

Motivation is to be free of some dodgy JavaScript.
Free. Forever.
Or Haskell, I suppose.
Bloody good though, pure Elm.
Not sure how to build it, but I cloned it anyway.

https://github.com/lucamug/elm-map
Might make more sense - more recently updated.

## Route joining

Open multiple GPX routes (file, Strava) and combine them in Route Maker.
May not even be that hard, given Route Maker exists.
But, routing tools exist.

## Video playback window

Sync'd to Orange?
https://www.w3schools.com/jsref/dom_obj_video.asp


