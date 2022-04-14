module Locations.UK exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import FormatNumber.Locales exposing (usLocale)


ukOptions =
    { country = Country "United Kingdom of Great Britain and Northern Ireland" "GB" "🇬🇧"
    , locale =
        { usLocale
            | thousandSeparator = ","
            , negativePrefix = "-" -- This is the printable hyphen, safe for use in GPX files!
        }
    , textDictionary = textDictionary
    }


textDictionary : Dict String (Dict String String)
textDictionary =
    Dict.fromList
        [ ( "main"
          , Dict.fromList
                [ ( "loadgpx", """Load GPX file""" )
                , ( "savegpx", """Save GPX file""" )
                , ( "notrack", """No track loaded""" )
                , ( "unnamed", """Unnamed track""" )
                , ( "import", """Other file options""" )
                , ( "askgpx", """
Select GPX file.

If the File Open dialog does not appear, please reload the page in the browser and try again.
""" )
                , ( "loading", """Loading GPX file ...""" )
                , ( "noload", """Sorry, unable to load that file""" )
                , ( "nogpx", """Could not make a track. Are you sure that's a GPX file?""" )
                , ( "nowrite", """Sorry, unable to write the file""" )
                , ( "nosvg", """Sorry, could not extract SVG paths""" )
                , ( "message", """Message""" )
                , ( "dismiss", """Dismiss""" )
                , ( "1CQF", "One-click Quick Fix!" )
                , ( "default", """Restore default tool layout""" )
                , ( "imperial", """Use Imperial measures""" )
                , ( "metric", """Use Metric measures""" )
                ]
          )
        , ( "panes"
          , Dict.fromList
                [ ( "layout", """Choose layout""" )
                , ( "one", """Large one""" )
                , ( "tall", """Cupboards""" )
                , ( "flat", """Drawers""" )
                , ( "grid", """Grid of four""" )
                , ( "Map", """Map""" )
                , ( "Perspective", """Perspective""" )
                , ( "Rider", """Rider""" )
                , ( "Profile", """Profile""" )
                , ( "Plan", """Plan""" )
                , ( "Route", """Route""" )
                , ( "About", """About""" )
                , ( "locked", """Locked to Orange""" )
                , ( "unlocked", """Draggable""" )
                , ( "drag", """Click to prevent point dragging""" )
                , ( "nodrag", """Click to allow point dragging""" )
                , ( "mapstyle", """Choose map style""" )
                ]
          )
        , ( "graph"
          , Dict.fromList
                [ ( "label", """Route maker""" )
                , ( "info", """
We follow the route looking for places and road sections that are used more than once.
This allows us to divide the route into a list of Roads, where each Road goes from from one
Place to other (or the same Place).

Once we've done that, you'll be able to change the route you take between places.
Use the **Route** view to help construct a new route.

You can also select a single road for editing using (most of) the normal tools. Any changes
you make here will be reflected in the resulting route so all the altitudes will agree.
""" )
                , ( "offset", """Offset the generated road using this route as the centre-line.""" )
                , ( "radius", """When passing a place, will attempt use this to create the bend.""" )
                , ( "render", """
Create a single road, using your route and offsetting the road
from the centre line (if you want to avoid collisions with oncoming avatars). As the same
road section is used for each passage, there should be no height differences (apart from
the 1cm difference we put in to avoid flicker in RGT.
""" )
                , ( "manyNodes", """
Hmm, that's a lot of Places. Route maker works best with 
GPX files from a route planner, not from recorded rides. That could be the issue here.
""" )
                , ( "edit", """
Please use any of the other views to edit this section of track.
Any changes you make will be reflected here.

**NOTE** Please do not use _Save GPX file_ while editing a section, as that will save only the active section.
""" )
                ]
          )
        , ( "bends"
          , Dict.fromList
                [ ( "label", """Bend problems""" )
                , ( "usepoint", """At a point""" )
                , ( "useradius", """With a radius""" )
                , ( "summary", """Summary""" )
                , ( "list", """List""" )
                , ( "change", """Direction change {0}º""" )
                , ( "radius", """Radius {0}""" )
                , ( "prev", """Move to previous""" )
                , ( "this", """Move pointer to this issue (Is the padlock on?)""" )
                , ( "next", """Move to next""" )
                , ( "none", """None found""" )
                , ( ".of.", """{0} of {1}, {2}º""" )
                , ( ".radius.", """{0} of {1}, radius {2}º""" )
                , ( "smooth", """Smooth these points""" )
                , ( "adjust", """Widen current bend""" )
                , ( "info", """
Find points where the road direction changes significantly, or find
sections of track that may be a bend with a small radius.

From here, you can jump directly to the sections of track and use other tools to fix the problems.
""" )
                , ( "autofix", """
Smooth each of these individually using the single point _Smooth with arcs_. 
Use that tool to change the number of points that are added to smooth each point.

You should use this only for trivial fixes; there are better tools for smoothing
serious issues. This tool can even make things worse.
""" )
                , ( "locate", """
These buttons will move the Orange pointer through the list of issues.

**Note**: this will only centre the views which have the padlock closed.
""" )
                , ( "widen", """
Nudge the points on the bend(s) outwards to increase the radius.

You may get better results from using the _Smooth with Arcs_ or _Radiused Bends_ tools.
""" )
                ]
          )
        , ( "gradients"
          , Dict.fromList
                [ ( "label", """Gradient problems""" )
                , ( "usepoint", """Sudden change""" )
                , ( "climbs", """Steep climbs""" )
                , ( "descents", """Steep descents""" )
                , ( "summary", """Summary""" )
                , ( "list", """List""" )
                , ( "change", """Direction change {0}º""" )
                , ( "radius", """Radius {0}""" )
                , ( "prev", """Move to previous""" )
                , ( "this", """Move pointer to this issue (Is the padlock on?)""" )
                , ( "next", """Move to next""" )
                , ( "none", """None found""" )
                , ( ".of.", """{0} of {1} is {2}º""" )
                , ( ".radius.", """{0} of {1}, radius {2}º""" )
                , ( "smooth", """Smooth these points""" )
                , ( "adjust", """Widen current bend""" )
                , ( "threshold", """Threshold {0}%""" )
                , ( "info", """
Find points where the gradient changes significantly, or is a noticeably
steep ascent or descent.

From here, you can jump directly to the sections of track.
You can use a quick-fix here to simply round off the points or (usually better),
use _Profile Smoother_, _Smooth with Splines_, _Simplify_ or _Nudge_.
""" )
                , ( "autofix", """
Smooth each of these individually using the single point _Smooth with arcs_. Use that
tool to change the number of points that are added to smooth each point.

You should use this only for trivial fixes; there are better tools for smoothing
serious issues.
""" )
                ]
          )
        , ( "arcs"
          , Dict.fromList
                [ ( "label", """Smooth with arcs""" )
                , ( "info", """
Find a circular arc to replace an existing bend, by moving the Orange and
Purple markers to find an acceptable solution. 

Set the spacing to control the fineness of the replacement bend.

This will also enforce a uniform gradient along the new arc.

_Radiused bends_ offers a slightly different approach.
""" )
                ]
          )
        , ( "splines"
          , Dict.fromList
                [ ( "label", """Smooth with splines""" )
                , ( "tension", """Tension: {0}""" )
                , ( "tolerance", """Tolerance: {0}""" )
                , ( "through", """Through existing points""" )
                , ( "approx", """Approximating existing points""" )
                , ( "whole", """Applies to whole track""" )
                , ( "part", """Applies between markers""" )
                , ( "apply", """Apply""" )
                , ( "info", """
Splines are a common way to engineer a smooth curve. They work in three dimensions
and so can help with gradient and direction changes.

We have two variants:

- Passing through existing points is useful when you have relatively few points and
you want them not to move, but need more points to define a curve.

- Using existing points as a guide if useful when you have many points but don't
actually want or need to pass through them.
""" )
                ]
          )
        , ( "centroid"
          , Dict.fromList
                [ ( "label", """Centroid average""" )
                , ( "info", """
 A simple way to remove "noise" from a track is by taking the average of each
 point with its neighbours. This tool does that in three dimensions, but lets you
 decide whether to apply this to the position or altitude.
 
 You can choose any "weighting" between the original points and the averaged points.
 
 You can use this repeatedly to spread the averaging effect over more points.
 
 If you're interested, the average we use is the centroid of the triangle defined
 by a point and its neighbours.
 """ )
                ]
          )
        , ( "radius"
          , Dict.fromList
                [ ( "label", """Radiused bends""" )
                , ( "info", """
 Switchback? Roundabout? What you need is to construct your own bend with a
 desired radius. This tools lets you do that, plus nice entry and exit lines.
 
 I recommend using Plan view, so that you can see what you're doing with the tool.
 When you drag on the black circle control, you see a circle moving on the Plan view.
 Adjust the radius with the top slider and imagine you're "pushing" this circle into
 the bend to shape it.
 
 The _Joining radius_ slider changes the radius of the entry and exit lines.
 
 Generally this tool increases the radius, but some points may need to be pulled in,
 so use "Include outliers" to do that. This will reveal yet another slider.
 
 If you have sections of track close together, the software can't always tell where you
 intend the bend to be; in these cases, it may help to place the Orange and Purple markers
 either side of the bend, meaning "this section of track contains the bend."
 
 Finally, you can either use a constant gradient over the new bend, or you can ask it
 to respect the existing altitudes, inasmuch as that is possible.
 """ )
                ]
          )
        , ( "delete"
          , Dict.fromList
                [ ( "label", """Delete""" )
                , ( "sorry", """Sorry, I can't let you do that.""" )
                , ( "single", """Delete single point""" )
                , ( "many", """Delete between and including markers""" )
                , ( "info", """
If you've got a single point -- or more -- out of line, sometimes the best thing
to do is just Delete them.

Delete a single point by placing the Orange marker, or several points by using both Orange
and Purple. Delete includes the points where the markers are.

Don't worry, it won't let you delete the whole track.
""" )
                ]
          )
        , ( "display"
          , Dict.fromList
                [ ( "label", """Display settings""" )
                , ( "Curtain", """Curtain style""" )
                , ( "None", """None""" )
                , ( "Plain", """Plain""" )
                , ( "Coloured", """Coloured""" )
                , ( "Land Use", """Land Use""" )
                , ( "Flat", """Flat""" )
                , ( "3D", """3D""" )
                , ( "road", """Road surface""" )
                , ( "Ground", """Ground""" )
                , ( "Centre line", """Centre line""" )
                , ( "Place names", """Place names""" )
                , ( "noterrain", """Terrain off""" )
                , ( "quality", """Terrain quality""" )
                , ( "info", """
Change how the road appears in the 3D views. You can select to see the road surface,
a dropped "curtain" either plain or shaded to show gradient, There's also a centre line
for the road matching the gradient colour. For good measure, you can turn off the green
"ground" plane and see the route from below.
""" )
                , ( "landuse"
                  , """
You can fetch Open Street Map data that describes the land use.

The colours used are shown in the _Land use_ tool, which will also show any errors
obtaining the data.

You can show these "flat" or "sloped" Be warned, when "sloped", it may obscure the road!
"""
                  )
                ]
          )
        , ( "tools"
          , Dict.fromList
                [ ( "label", """Tool summary""" )
                , ( "left", """Move to left""" )
                , ( "right", """Move to right""" )
                , ( "onleft", """Left""" )
                , ( "onright", """Right""" )
                , ( "hidden", """Hidden""" )
                , ( "notrack", """Controls will appear here when a track is loaded.""" )
                , ( "blank", """           """ )
                , ( "info", """
Quickly place tools on the left or right of the display, or hide them completely.

Also, access a quick description of what the tool does and how (and when) to use it.
""" )
                ]
          )
        , ( "action"
          , Dict.fromList
                [ ( "deleteN", """deletion of points""" )
                , ( "delete1", """delete one point""" )
                , ( "spline", """smooth with splines""" )
                , ( "centroid", """smooth with 3D average""" )
                , ( "radius", """radiused bend""" )
                , ( "arc", """circular arc""" )
                , ( "map", """move on map""" )
                , ( "nudge", """nudge""" )
                , ( "outback", """out and back""" )
                , ( "simplify", """simplify""" )
                , ( "insert", """insert points""" )
                , ( "1CQF", """one-click quick fix""" )
                , ( "limit", """limit gradients""" )
                , ( "altitudes", """smooth altitudes""" )
                , ( "gradients", """smooth gradients""" )
                , ( "scale", """rotate and scale""" )
                , ( "elevations", """use altitudes from map""" )
                , ( "segment", """insert segment from Strava""" )
                , ( "stretch", """move and stretch""" )
                , ( "close", """close loop""" )
                , ( "reverse", """reverse track direction""" )
                , ( "start", """move start points""" )
                , ( "pens", """add RGT rider pens""" )
                , ( "append", """append GPX track""" )
                , ( "straighten", """straighten""" )
                , ( "autofix", """autofix""" )
                , ( "route", """turn route into new track""" )
                , ( "widen", """widen bend""" )
                , ( "smart", """smart smoother""" )
                , ( "unknown", """(what was that?)""" )
                ]
          )
        , ( "essentials"
          , Dict.fromList
                [ ( "label", """Essentials""" )
                , ( "point", """Point {0} at {1}""" )
                , ( "lift", """Lift purple marker""" )
                , ( "drop", """Drop purple marker""" )
                , ( "note", """Use Orange and Purple markers to select track for editing.""" )
                , ( "noundo", """Nothing to Undo""" )
                , ( "noredo", """Nothing to Redo""" )
                , ( "undo", """Undo {0}""" )
                , ( "redo", """Redo {0}""" )
                , ( "info", """
Most of the editing tools require either a single point or a range of points to work on.

The top buttons in this tool allow you to move an Orange marker along the track to select a single point.

There is a button to place a Purple marker at the current position, and then to move the Purple marker.
This defines a range which you can then use for tools that require it.

Below the pointer controls are the Undo and Redo buttons. These let you go back and forward over the
previous ten edits. Once you make a different change, you can only Undo.
""" )
                ]
          )
        , ( "fly"
          , Dict.fromList
                [ ( "label", """Flythrough""" )
                , ( "info", """
It's often useful to see the track as the rider would see in in RGT. We don't have the
sophisticated scenery that RGT offers, but you can set the ride in motion and adjust the speed
to get a quick feel for how it might ride.
""" )
                ]
          )
        , ( "insert"
          , Dict.fromList
                [ ( "label", """Insert points""" )
                , ( "info", """
Sometimes you need more track points to create the precise shape you need, perhaps
by _Nudge_-ing them or dragging them on a map. Also, when you're using some of the track
smoothing tools, it can improve the outcome to have points more closely spaced.

This tool will enforce a _maximum_ spacing between points. This can mean that the resulting
spacing varies from one section to another.
""" )
                ]
          )
        , ( "intersections"
          , Dict.fromList
                [ ( "label", """Intersections""" )
                , ( "info", """
This helps to find places where one road section crosses another, or where a piece
of road is used more than once, either in the same or the opposite direction.

This is gateway to thinking of a route as something that can be navigated differently,
but that's where _Route builder_ comes to play.
""" )
                ]
          )
        , ( "stretch"
          , Dict.fromList
                [ ( "label", """Move and Stretch""" )
                , ( "info", """
Think of this as _Nudge_, supercharged. Use the markers to define a section of track,
then the circular 2-way drag control to move this section freely. There's a height adjustment
as well.

The real value of this tool is "Stretch" mode. Suppose you have a series of switchbacks
but they're too tightly packed for RGT Magic Roads. In stretch mode, you move a white marker
along the track between the Orange and Purple. Now, when you drag the 2-way control, the
white marker follows the drag and the track either side "stretches" to follow it.

With a bit of practice, you can add some separation to the switchbacks. It probably has
other uses as well, but this was the justification.
""" )
                ]
          )
        , ( "affine"
          , Dict.fromList
                [ ( "label", """Move,Scale,Rotate""" )
                , ( "info"
                  , """
Want to ride Ventoux in the Sahara? Want your local loop to be a bit longer, or a lot?

Move, Scale & Rotate lets you perform some simple transformations on the whole route. It's maths.

Scale and Rotate are fairly obvious but Move requires you to use the Map view to identify where
you want your route. The coordinates of the last map click are displayed in the tool. Without the
Purple marker, the centre point of the route is moved to the last map click position. With the
Purple marker, the Purple marker is moved there; this can give you more control over placement.
"""
                  )
                ]
          )
        , ( "nudge"
          , Dict.fromList
                [ ( "label", """Nudge""" )
                , ( "info", """
Sometimes all it takes is a little _Nudge_.

A track point, or a run of points, is slightly out of line from where you want it (them).
With Nudge, you can move a single point, or range of points, sideways and up or down.

What "sideways" means is a but subtle with bends. Nudge moves along what a carpenter would 
recognise as the mitre line, effectively half of the turn. **Be aware** than closely spaced
points nudged together can overlap on the inside of a bend. GPXmagic will not stop this.

The optional _Fade_ slider lets you gradually blend the Nudged section with the neighbouring track.
""" )
                ]
          )
        , ( "bytheway"
          , Dict.fromList
                [ ( "label", """Out and Back""" )
                , ( "info"
                  , """
Also known as the **Bytheway special**, this tool makes it a snap to turn any route into
an "Out and Back" by reversing direction at the end and returning to the start.

If you want to have separate tracks, use the _Offset_ slider to move the tracks to the left
or right of the centre-line (the original track). If you prefer no offset, you risk head-on
collisions with other riders; your choice. Note that, as with _Nudge_, beware of closely-spaced
points and sharp bends.

The return leg is 1cm lower than the outbound leg to avoid surface flickering in RGT when you
have no offset.
"""
                  )
                ]
          )
        , ( "profilesmooth"
          , Dict.fromList
                [ ( "label", """Profile smoothing""" )
                , ( "info", """
There's no one way to achieve a smooth altitude profile. It depends whether you begin with a recorded
or a planned ride; it depends whether you favour accuracy or smoothness; whether you want
precise elevations or moderate gradients.

This tool offers (currently) three methods to control altitude, each can work over a range or
the whole track.

1. Limit ascents and descents to set maxima. This will simply remove any gradients outside
the limits you set. This will affect the altitude for the rest of the course.

2. Replace the _altitude_ at each point with the average of a range of nearby points. Note that this
does not factor in the distance between points; it's a straight numerical average. It's basically
the "altitude box smoother" in another tool.

3. Replace the _gradient_ at each point with the numerical average gradient over nearby points.
Note that changing the gradient at even one point affects all subsequent points. This produces
quite pleasing results in many cases.
""" )
                ]
          )
        , ( "simplify"
          , Dict.fromList
                [ ( "label", """Simplify""" )
                , ( "info"
                  , """
Recorded "IRL" rides contain a lot of GPS "noise". GPS is accurate only to a few metres,
more sampling will not change this. Some of the other tools can help to reduce this noise,
but it can be more effective to simply remove some (many) of the points that don't really
contribute much to the shape of the route. Those that can be discarded, should be.

This tool assigns to each point a value representing its contribution, defined by the area
of the triangle it makes with its neighbours. Those with the smallest 20% contribution are
identified, then a check is made to avoid removing adjacent points.

This proves in practice rather effective at removing "noise" without detracting from the
shape of the route.
"""
                  )
                ]
          )
        , ( "split/join"
          , Dict.fromList
                [ ( "label", """Split & Join""" )
                , ( "info", """
Got an excessively long route? Why not ride in it sections?

The _Split_ option here divides the track into roughly equal sections to achieve a set maximum length.
(There's a slight variance due to track point spacing.) Optionally, add the RGT start and end pens
at the joins to make sure you do the whole original ride.

Conversely, if you have two GPX files that are in real-life nearby, you can append a second route
to your current route. **Note** this will not attempt to match locations, directions or altitude.

Remember, you can move any route to any place in the world with _Move & Scale_, so can can
use this to Everest your fave climbs.
""" )
                ]
          )
        , ( "loop"
          , Dict.fromList
                [ ( "label", """Start/Finish""" )
                , ( "info", """
RGT will recognise a loop if the two ends are sufficiently close. But it won't
make it smooth for you.

Here's a few options:

1. Add a spline to join the start and finish, completing a loop
2. Move the Start/Finish to somewhere else on the loop
3. If it's not a loop, add 60m before the start and 140m after the end for the RGT rider pens.
""" )
                ]
          )
        , ( "straight"
          , Dict.fromList
                [ ( "label", """Straightener""" )
                , ( "info", """
Sometimes you just want a straight to be straight and it's tedious to get rid of
all the wriggles. Sure, you could delete some of the points, but you might want to
keep some altitude changes.

Straightener is simple and single-minded. It takes all the points in the range and lines
them up. It will either retain their altitudes or impose a constant gradient. Note that
keeping the altitudes and squishing the points up increases gradients.
""" )
                ]
          )
        , ( "strava"
          , Dict.fromList
                [ ( "label", """Strava""" )
                , ( "info", """
The Strava tool has two functions:

1. Import a route direct from Strava for smoothing

2. Place a Strava segment into your current route. Some people like the "accuracy" of segments.

You must first authenticate with Strava using the button on the top bar; do that before loading a GPX.
""" )
                ]
          )
        , ( "info"
          , Dict.fromList
                [ ( "label", """Information""" )
                , ( "track", """Track""" )
                , ( "point", """Point""" )
                , ( "points", """Points""" )
                , ( "memory", """Memory""" )
                , ( "length", """Length""" )
                , ( "ascent", """Ascent""" )
                , ( "descent", """Descent""" )
                , ( "climbing", """Climbing""" )
                , ( "descending", """Descending""" )
                , ( "steepest", """Steepest""" )
                , ( "number", """Number""" )
                , ( "distance", """Distance""" )
                , ( "longitude", """Longitude""" )
                , ( "latitude", """Latitude""" )
                , ( "altitude", """Altitude""" )
                , ( "bearing", """Bearing""" )
                , ( "gradient", """Gradient""" )
                , ( "none", """Not available""" )
                , ( "limit", """Heap limit""" )
                , ( "size", """Heap size""" )
                , ( "heap", """Used heap""" )
                , ( "notrack", """Information will show here when a track is loaded.""" )
                , ( "info", """
View information about the current point, the whole track, and memory usage.
""" )
                ]
          )
        , ( "landuse"
          , Dict.fromList
                [ ( "label", """Land use""" )
                , ( "info", """
Displays the colour legend for land use data, and a list of named places.
""" )
                ]
          )
        , ( "smart"
          , Dict.fromList
                [ ( "label", """Smart smoother""" )
                , ( "info", """
Smart smoother applies three constraints to the track:

1. A minimum turning radius - this applies to all turns but is most noticeable on hairpins which
will become circular arcs (rather like _Radiused Bends_)

2. How quickly you wish to enter and leave bends, because in real life you don't just
jerk on the handlebars, it takes time to lean into a bend. The result is approximately
a "Clothoid" or "Euler spiral".

3. Gradients, up and down. Yes, we already have _Limit Gradients_ but you get this for free.

On a sequence of hairpins, you need to find an acceptable combination of minimum radius and
lead-in to get the horizontal separation about right.

On a long track, the numerical technique used means the smoothed track can increasingly
deviate from the original track; you may prefer to work in sections.

It's also _really_ good at removing noise from IRL rides.
""" )
                , ( "radius", """Bends with a radius smaller than this will be replaced by a circular arc.""" )
                , ( "transition", """The length of track it takes to lean into a bend, or get back on the straight.""" )
                , ( "gradient", """The maximum steepness of any ascent or descent.""" )
                ]
          )
        ]
