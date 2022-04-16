
# WIP

## I18N

Awaiting French support from Muriel.

---

# BACKLOG

## Technical debt

Variant of "request from local storage" that takes a wrapped message so that the return value
can be directed to a tool or a view.

Try to move out the residual tool-specific stuff in Main.performActions.

## De-noise / Simplify

This could be improved. Should preferentially remove any points where the triangle has a large
deflection and a short baseline, not just based on area. Also, an effective quality filter 
would be to restrict the angular change in both planes. Will do this prior to curvature analysis.
> Obviated by smart smoother?

## RGT: Timed segments

If Christian can supply tags, tool to list and edit Time Segments, defined by markers
and nameable; these will be embedded in the output GPX.

## RGT: Integrate with track DB back-end

Launch with track URL, save does a POST.

## RGT: Black-box mode

Runs server-side (Node.js?), receives track via POST, returns smoothed.
> `elm-serverless` can do this, may be other ways.

## Route Builder

- Improve traversal direction display (with an arrow on each section?).
- Add "split edge at pointer" function, when editing an edge (?).
- Improve behaviour with IRL rides (use code from Intersections?)

## Usability

Drag Curve Former circle directly in Plan View. (Add an SVG "handle" to hit detect.)
Ditto for Move & Stretch, possibly Nudge.
Provide info text capability on top bar and on view panes.
Specific areas within tools as needed.

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

## Adaptive smoothing

Not needed now smart smoother rules.
1) Analysis of curvature (two planes)
2) Placement of arcs
3) Bezier for intermediates
4) Lines and arcs otherwise
5) Tangents between adjacent arcs
6) Splines between arc & straight

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

