
# BUGS

BUG: Classic bend smoother consumes all memory on certain looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

**BUG**: Sometimes will not display file open dialog. Also splitters stop working.
> Debuggers shows messages arriving, not obvious why they should not be processed.
> Can't save file when this happens, so rather poor show.

--

# WIP

## SVG profile rendering

PeteTree previews for Bezier, Centroid, Nudge, Limit Gradients and new Filters.

Previews on Gradient curve also.
Gradient should not be sloped, should be steps.
Info on mouse move.
Zero line for gradient.
Scales?

## Profile smoothing

To deal with Vuew GPX Smoother.

- **Limit Gradient** = as current but option to not redistribute, just to clamp.
- **Elevation box** smoothing is simple running average of altitude.
- **Slope box** smoothing does running average of gradients.
- **Kalman** is what it is; not complex and seems to give some good results by damping gradient changes.

- But not:
- Savitzky-Golay is more complex least-squares based thing (not referenced by JB).
- Elevate points is vertical Nudge.
- Flatten points (is Limit Gradients without the redistribution)


---

# BACKLOG, roughly in order ...

## Tools: old, updated, & new

1. Graph Theory (notes below and from DO's emails)
2. Info popup for all tools.

--- _Cut-off for release_
- Out & back lower return by 1cm.
- Terrain as from v2 but with texture maybe
- SVG for previews on Profile - get nice smooth lines.
- SVG overlay on 3d views.
- Super smoothing  (think GPXsmoother, but different, key feature is ability to "fix" regions).
- Non-customisable keyboard alternatives for Load/Save/Undo/Redo/Fwd/Back/Purple (maybe 1-5 for views)
- Extract all text for translation (Muriel)
- Use localised number formatting everywhere (for French use of , and .)
- Ability to point-smooth transitions over a range
- Tools that require a range should say so when there isn't one! (David Ogle)

--- _Cut-off_
- Option to show MR rendering cutoff.
- Tooltips (where useful)
- Draggable tools?

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

## Terrain

Tree walk combined with whole (visible) tree query, because <track loops>.
(Expand bounding boxes to allow for road width.)

## Loops

- Centroid average to work over S/F on loop
- Bezier smoothing to work over S/F on loop

## Texture for the ground plane, road surface

See https://github.com/ianmackenzie/elm-3d-scene/blob/1.0.1/examples/Texture.elm
https://ambientcg.com/view?id=Grass004
https://ambientcg.com/view?id=Asphalt001
Credit: Contains assets from ambientCG.com, licensed under CC0 1.0 Universal.

## Small stuff

Maybe convert all markers to SVG with overlay.

Put all Font, Colour etc into a Palette/Style module for ease of change.
> Search for FlatUI references.
 
## Graphs

**David Ogle**: 
After using graph theory you end up with out and back track points in the same position... 
say the orange marker is near the end of the course and I select a track point nearby, 
it always selects the one at the start of the course, instead of the one I was trying to 
select near the end. So I'm thinking if you've got 2 track points in the same position, 
on selection (mouse click), it'd be good to select the one closest to the current position.

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
