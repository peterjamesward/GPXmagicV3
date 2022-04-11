
# BUGS

BUG: Classic bend smoother consumes all memory on some looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

BUG: Undo single point smooth at track start removes a point.

NOT A BUG: Move & Stretch does not move marked points. 
> This is intentional. May not be right though.

BUG?: (DavidO)
- Opened the Move/Stretch tool and set a range - this displayed a preview.
- Hid the tool using the Tools summary section 
  - (the tool was still expanded when I selected "Hidden") 
  - the Move/Stretch preview was still visible.
- Re-opened the file and the preview was still in place.
> Sounds like options not being reset on track load.

TWEAK:
For altitude and gradient smoothing, reduce the averaging window at each end of the range
to avoid "spikes" when it suddenly ends.

BUG: Now Map has full track, clicking should not redraw it!

--

# WIP

## Smoothing by averaging & limiting curvature

What effect does this have? If we replace curvature (the N in TNB) with the average of nearby
(possibly interpolated) points. We would have to extend the averaging range so that the vector
sum of the directions takes us "back on tracK" or as near as can be. Also, at the same time, to
ensure that the average curvature never exceeds the maximum allowed.

Can this be implemented point-wise? Would we ever be able to follow a track (e.g. Lacets)?

Appealing simplicity and symmetry, but subtleties. Say we are on a hairpin of > max curvature.
We set the curvature to max and then seek sufficient points either side such that the net
total curvature is equalled, AND (ideally) we are somewhere near the original track and direction!

Maybe this is achieved by working out how far this extends both sides but the implication is that
when we look at the other points, they must respect this range. That is, if the bend point is
at 100m, but the search extends over the range 60 .. 140, then when we are solving for 60, we
must look out as far as 100 (or 140?).

We can but try. See if it's even remotely feasible. Coding is easier than maths, for me today.

> Should almost certainly run until captured data is exhausted.
> Fix these then add backward pass and figure out how to combine them.
> This may involve a "shift" element, relating to new track length.
> Profile Preview

---

# BACKLOG

## Donations

Restore the PayPal link.

## Adaptive smoothing

1) Analysis of curvature (two planes)
2) Placement of arcs
3) Bezier for intermediates
4) Lines and arcs otherwise
5) Tangents between adjacent arcs
6) Splines between arc & straight

## De-noise / Simplify

This could be improved. Should preferentially remove any points where the triangle has a large
deflection and a short baseline, not just based on area. Also, an effective quality filter 
would be to restrict the angular change in both planes. Will do this prior to curvature analysis.

## Timed segments

If Christian can supply tags, tool to list and edit Time Segments, defined by markers
and nameable; these will be embedded in the output GPX.

## Integrate with track DB back-end

Launch with track URL, save does a POST.

## Black-box mode

Runs server-side (Node.js?), receives track via POST, returns smoothed.
> `elm-serverless` can do this, may be other ways.

## Route Builder

- Zoom on Route mouse-centred?
- Improve traversal direction display (with an arrow on each section?).
- Add "split edge at pointer" function, when editing an edge (?).
- Improve behaviour with IRL rides (use code from Intersections?)

## Usability

Drag Curve Former circle directly in Plan View. (Add an SVG "handle" to hit detect.)
Ditto for Move & Stretch, possibly Nudge.
Provide info text capability on top bar and on view panes.
Specific areas within tools as needed.

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

## Tools: old, updated, & new

- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Extract all text for translation (Muriel)
- Use localised number formatting everywhere (for French use of , and .)
- Ability to point-smooth transitions over a range
- "Tip of the day" tool? (Davie Ogle will write tips.)

## SVG profile rendering

Info on mouse move.
Scales?

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.
 
---

# Parked

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

## Offset/nudge logic

Don't just mitre. For each pair of RoadSection, see if the points will
"overlap" and don't emit them all. May need some interpolation for altitude or whatever.

## Map

DEBT: Map sends more click messages each time we click.
> They are debounced but, still, why??

## Laziness, optimisation

Don't render anything that's not visible.

