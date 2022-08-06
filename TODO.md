
# BUGS

--- 

# WIP

## Timestamps (Jonathan Colledge)

> I wonder if you might consider adding timestamp functionality to GPX Magic -
> this would open up the user base to all the Kinomap video makers and would make
> GPX Magic a one-stop shop for GPX video synchronisation.

> Here is a video that summarises the issues in the workflow: https://youtu.be/mN7MZwTizAY

> The only thing not mentioned in the video is how I slow the GPX down (say if I
> was slowing the video from 60 fps to 30 fps) - I would interpolate to 0.5 s intervals,
> then relabel them all at 1 second intervals, thus slowing the GPX to match a slowed video.

Time for a point is a valid coordinate similar to altitude. 
It can be interpolated analogously.
Will compute intervals in tree as this may be useful later for interpolation.

Edits preserve start and end times of range. 
Points preserve their time.
New points used an interpolated time between the extremes (like altitude on bend smoothing).

Use markers to designate a range for a time-wise "nudge" adjustment.
e.g. after delete, bring remaining track earlier in time.
Maybe default from Orange to track end.
(May need option to Shift or Stretch.)

Function to interpolate at 1s boundaries.

Option to change (“scale”) the interval to one of 0.5, 1.0, 2.0 seconds. 
This will just scale the intervals internally. The Orange and Purple pointers will show time 
when available. Purpose is to allow videos to run at half-speed (e.g. car recorded).

Might allow adding timestamps to an untimed GPX based on track length and average speed.
Or even a very simple physics model, because it's there.

---

# BACKLOG

## On refactoring Action interpeter in Main.

Main difference is how the pointers are repositioned.
There's only two or three cases, if not a single general one.

## Load from URL

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
