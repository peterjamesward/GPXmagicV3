
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

BUG: Undo single point smooth at track start removes a point.

--

# WIP

## Route Builder

- Unified Undo/Redo for graph & track operations.
> Simple union type, track edits wrapped with Edge index.

- Add "edit road"; changes active Track so road can be edited.
> Properly, we're always editing a graph edge, but that means that we should
> move TrackLoaded _inside_ Graph, and have a current track. (edges : Dict Int TrackLoaded).
> Which is not too hard.
> Perhaps, in graph mode (i.e. Analyzed) set state to AlwaysOpen as a reminder of mode.
> Could also Disable inappropriate tools.
> Similarly, disable Save GPX (?)

- Transition route walking with offset (see Out & Back for the fold).
> Easiest is just to concatenate the tracks and use that fold.
> I.e., don't try to smooth the junctions.

(Now, equal to v1/2.)

- Add lollipops when offset < min radius. (also on out and back)

- Improve traversal direction with an arrow on each section.

- Add "split edge at pointer" function.

- Save/Load graph (in a single file, not necessarily XML, maybe CBOR, maybe text).

- Centre Plan view on selected traversal.


---

# BACKLOG

## Usability

Drag curve former circle directly in Plan View.
Ditto for Move & Stretch, possibly Nudge.

## Tools: old, updated, & new

- Optionally show OSM land-use polygons on ground plane.

--- _Cut-off for release_
- Terrain as from v2 but with texture maybe
- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Extract all text for translation (Muriel)
- Use localised number formatting everywhere (for French use of , and .)
- Ability to point-smooth transitions over a range

--- Cut off completely

"Tip of the day" tool? (Davie Ogle will write tips.)

## Help

Provide info text capability on top bar and on view panes.
Specific areas within tools as needed.

## SVG profile rendering

Info on mouse move.
Scales?

## New smoothing

> This could be like a meta-box, or a "build your own 1CQF", in which
> we pipeline existing features, just like 1CQF.
> E.G. simplify > limit > interpolate > centroid.

John Bytheway:
> I still find myself using GPX Smoother, I find it easier to see the
difference using the different algorithms made on the course and which
I need to use where to keep the course as faithful as possible whilst
making them smoother. I would really like the options they use, my go
to is the Slope box smoothing followed by the Kalman filter then
elevation box smoothing, but its not just those options that make it
good its the visual way of seeing the changes with the Elevation
chart, Slope and Elevation profile making it easy to see exactly what
the changes are doing.

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

Could be based on OSM land use data.

## Small stuff

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.
 
---

# Parked

## Offset/nudge logic

Just don't blindly mitre. For each pair of RoadSection, see if the points will
"overlap" and don't emit them all. May need some interpolation for altitude or whatever.
> Perhaps just try Bezier on interior turns.

## Map

DEBT: Map sends more click messages each time we click.
> They are debounced but, still, why??

## Laziness, optimisation

Don't render anything that's not visible.

## more

- Option to show MR rendering cutoff.
- Tooltips (where useful)
- Draggable tools?
