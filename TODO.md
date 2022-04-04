
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

BUG: Undo single point smooth at track start removes a point.

BUG: Map is not showing full detail after 1CQF, maybe, needs checking.
> Awaiting script from JB.

--

# WIP

## Land use

- Display HTTP & parse errors in tool (e.g. 504)
- SVG overlay with place names -- you know you want to.

## Minimum radius (whole track or range thereof)

Just had a thought. If we use `Point3d.circumcenter` along the track (range), and if _anywhere_ the
circumcircle has less than `radius`, we displace the points (if needed) outwards to the desired radius.
Note that each point (except extremes) is part of three triangles so we would (I guess) use the mean
(centroid) of the possible displacements.
> Do this first, then the localised solution falls out)

## Minimum radius (localised)

Current scheme is "OK-ish".

If successful, add multiple case
- (with single Undo for the multiple case)

---

# BACKLOG

## Land use display option

- Report if GET fails or times out or parse fails.
- Add a control that lists named features, select to centre. (Can also show errors.)

## 1CQF / New smoothing

Allow the use of markers for partial application.
> This could be like a meta-box, or a "build your own 1CQF", in which
> we pipeline existing features, just like 1CQF.
> E.G. simplify > limit > interpolate > centroid.

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

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.
 
---

# Parked

## Offset/nudge logic

Just don't blindly mitre. For each pair of RoadSection, see if the points will
"overlap" and don't emit them all. May need some interpolation for altitude or whatever.

## Map

DEBT: Map sends more click messages each time we click.
> They are debounced but, still, why??

## Laziness, optimisation

Don't render anything that's not visible.

