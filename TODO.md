
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

BUG: Undo single point smooth at track start removes a point.

BUG: Map is not showing full detail after 1CQF, maybe, needs checking.
> Is only rendering; asked JB to check.
> May fix if we just reposition Orange based on distance.

--

# WIP

## Route Builder

- "Walk route" must go on Undo stack, with previous Graph state. (Edges may have been edited.)
- "Analyse" should go on Undo with previous Track.
> Be wary of import loops!!

- Redo.

- Update Map after walking new route.

- Zoom on Route mouse-centred?

(Now, >= v1,v2)

- Add lollipops when offset < min radius. (also on out and back)

- Option to close loop if S == F.

- Improve traversal direction display with an arrow on each section.

- Add "split edge at pointer" function.


---

# BACKLOG

## 1CQF

Allow the use of markers for partial application.

## Usability

Drag curve former circle directly in Plan View.
Ditto for Move & Stretch, possibly Nudge.

## Tools: old, updated, & new

- Optionally show OSM land-use polygons on ground plane. (Not sure about Terrain.)

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
