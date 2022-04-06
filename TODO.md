
# BUGS

BUG: Classic bend smoother consumes all memory on some looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

BUG: Undo single point smooth at track start removes a point.

BUG: Map is not showing full detail after 1CQF, maybe, needs checking.
> Awaiting script from JB.

BUG: Move & Stretch does not move marked points. 
> This is intentional. May not be right though.

BUG: (DavidO)

- Opened the Move/Stretch tool and set a range - this displayed a preview.
- Hid the tool using the Tools summary section 
  - (the tool was still expanded when I selected "Hidden") 
  - the Move/Stretch preview was still visible.
- Re-opened the file and the preview was still in place.

TWEAK:
For altitude and gradient smoothing, reduce the averaging window at each end of the range
to avoid "spikes" when it suddenly ends.

--

# WIP

## The Holy Grail of smoothers

See private notes (sorry if you're following on github).
 
---

# BACKLOG

## Timed segments

If Christian can supply tags, tool to list and edit Time Segments, defined by markers
and nameable; these will be embedded in the output GPX.

## Integrate with track DB back-end

Launch with track URL, save does a POST.

## Black-box mode

Runs server-side (Node.js?), receives track via POST, returns smoothed.

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

