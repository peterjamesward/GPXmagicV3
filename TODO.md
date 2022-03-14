
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

**BUG**: Sometimes will not display file open dialog. Also splitters stop working.
> Debuggers shows messages arriving, not obvious why they should not be processed.
> Can't save file when this happens, so rather poor show.

**BUG**: Undo single point smooth at track start removes a point.

**BUG**: When using "Move start/finish to current point" it creates a gap.

**BUG**: Bend/Gradient problems "centre view on this issue" button -
if the map is draggable it doesn't centre the view on the issue.

**BUG**: Text in the Undo/Redo buttons should match the new tool names (for example,
after inserting points it still says "Undo interpolate").

**BUG**: Plan view is really zoomed out by default. Could it match the zooming on the 
Perspective view by default? It currently takes loads of clicks to get it to a usable zoom level.

**BUG**: The horizontal scrollbar issue the guy on FB mentioned is due to this:
<<looks like wrapperRow still has a -3px margin>>
 
--

# WIP

## Route Builder

~~Clone graph view from plan view.~~
~~Pass Graph into the View.~~
~~Add SVG overlays for Nodes ...~~
~~... and Edges.~~
~~> Down-sample edges (depth 5-ish).~~
~~Graph needs a referenceLonLat.~~

~~Test concept of popup action menus on the view.~~
~~> onClick method on `inFront` => message => update => Maybe MousePosition => moveLeft/Down.~~

~~Hit detect for Nodes and Edges.~~
~~Proforma popup menus for Node and Edge.~~

~~Use circular arc for edge?~~
Transition v2 Node detection / canonical Edge code.
Save/Load graph (in a single file, not necessarily XML, maybe CBOR maybe text).
Add "switch editing track". 
Highlight selected on Route view.
Add "delete Edge".
Add "delete Node" ( 1-in, 1-out only; joins trees )
Add "coalesce Nodes".
Add "split edge at pointer" function.
Controls to build traversals.
Link traversals to display (active Edge).
Remove and add traversal functions - ideally with tools on the view.
Port/Redo route walking with offset (better inside bends please).
Add lollipops when offset < min radius.
Idea: Since we render edges at limited depth, why not precompute spline approx for <smoothness>.


---

# BACKLOG

## Usability

Drag curve former circle directly in Plan View.
Ditto for Move & Stretch, possibly Nudge.

## Tools: old, updated, & new

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

Done centred window. Need to provide option for distance (or other) weighting. This can be later.

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
