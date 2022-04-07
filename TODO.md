
# BUGS

BUG: Classic bend smoother consumes all memory on some looped routes where
     orange and purple are (possibly) co-linear, or something. (Samir bug)

BUG: Undo single point smooth at track start removes a point.

NOT A BUG: Move & Stretch does not move marked points. 
> This is intentional. May not be right though.

BUG: (DavidO)

- Opened the Move/Stretch tool and set a range - this displayed a preview.
- Hid the tool using the Tools summary section 
  - (the tool was still expanded when I selected "Hidden") 
  - the Move/Stretch preview was still visible.
- Re-opened the file and the preview was still in place.
> Sounds like options not being reset on track load.

TWEAK:
For altitude and gradient smoothing, reduce the averaging window at each end of the range
to avoid "spikes" when it suddenly ends.

--

# WIP

## Adaptive smoothing

To some extent, smoothing is "a search problem". Conceptually, we could look at all possible 
positions of track points and find a "near optimal" solution based on a metric that accounts for 
match to source track, gradient limits, curvature limits, whatever. Of course, it's a huge search 
space but we could constrain it to reasonable perturbations of existing positions and use some 
combination of (say) simulated annealing, force directed algorithms, genetic search algorithms.

We can do better. Track smoothing is not a global problem, we can work on relatively small regions.
If we impose a minimum radius of 10m, say, and bends more than pi radians are very rare, then we
need be concerned only with a track length in the order of 31.4 metres.

So here's the latest idea. Conceptually, we think of track points as being movable in the sense
of being on springs, and being "hoops" that can rotate in 3d. Our challenge is to take a section
of material with some flexibility that embodies our rules such as minimum radius, maximum gradient,
&c. This is, say, 30m or so long. For that 30m section, we optimise. We then advance it (by how
much -- 10m, 20m, 30m?) and repeat.

We observe, for example, that when this encounters a tight bend of less than minimum radius, the
solution is likely to involve a deflection in the straight before the bend. This is good. The worm 
"remembers" the route is has taken for track points digested before and the re-distribution of the
delta-theta and delta-phi allows for this.

Our optimisation function must ensure that the worm cannot become stuck; there is always a solution
but it may require us to compromise our goals or lose some elements of the original. It's not meant
to be (and cannot be) perfect in both regards.

Should perform fairly well with IRL noise. If it encounters a point out of reach, that point could
be noise, or the previous points could be noise. A length of 30-50m should be enough to serve as
a low-pass filter (effectively what it is, but not exactly as it works symmetrically).

Another possibility is to include a penalty for self-contact, especially in the contrary direction.
This would provide an integrated mechanism for maintaining separation on switchbacks.

Expressed like this, maybe it's not a search problem but a tractable scheme that encounters
obstacles (track points that are outside the worm's normal tolerance) and redistributes the 
imposed force by reconfiguring along its length. So it's a back-tracking algorithm. The worm
"shits out" new track points every metre or so (configurable, possibly depending on curvature).

For more fun, if I focus the implementation on the worm state, rather than a "conventional" fold
over the track, I can animate it one track point at a time. Or not.

Default to whole track but could also work between markers.
Note it also dispenses trivially with duplicate points.

---

# BACKLOG

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

