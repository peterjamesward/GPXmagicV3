module About exposing (aboutText)


aboutText =
    """# GPXmagic v3.5.0 (66821ff2, 2023-07-05 13:45)

**GPXmagic V3 works best with Safari, Firefox & Chrome.**

## 3.15.0

Simplify now works between the pointers if the Purple marker is dropped.

Plan view has a new "finger painting" option.
This is preliminary work for some upcoming changes but it may appeal to some users.
Best watch the video.

Better attempt at dragging the track around in Third Person.

## 3.14.2

Show correct preview when switching modes in Smooth with Arcs.

Reduce default spacing and change range to 0.5 -- 10.0 metres.

## 3.14.1

Curly-wurly uses planar arcs to give better results on sharp bends with gradient transitions.

## 3.14.0

**New smoothing option**: "Curly-Wurly" to be found in the _Smooth with arcs_ tool.

This attempts to negotiate the known track points by stringing together a series of curves with a
changing radius. Sometimes, it will swing rather too wide. It's maths, not magic.

## 3.13.5

Improved dragging in 3D view. More consistent with Plan view.

## 3.13.4

When map is selected in 3D views, the green ground plane is hidden so you have no need to go into
Display Settings to turn it off.

## 3.13.3

Resolved Chrome problem with using Route Builder on some routes.

If anyone cares, the code was designed to be tail-recursive and not consume stack. The compiler didn't spot
this, so stack was being used up. Safari and Firefox seemed to have adequate stack, Chrome not so much. I
have rewritten the code with a fold that is definitely tail-recursive.

## 3.13.2

Fixed error in index.html that prevented 3.13.1 from working.

Added map option to Rider view.

## 3.13.1

Adjust zoom level for clearer map in third person view.

## 3.13.0

A present from the code fairies. Thanks to some excellent unrelated work by Martin Stewart, we can
now combined a (simplified) map with the Plan and 3-D views. There is a new toggle on the column of
buttons that appears in the upper right of these views. You will need to turn off "Ground" in the
_Display settings_ tool. It uses the exact same data from mapbox as the main map, but a drawing routine
that integrates much better with GPXmagic. It's not as good as the main map, but it's better than none,
and may get better.

NOTE: This map was not intended for use in 3D views, so let's enjoy it for what it is.

Main map updated to use mapbox GL JS 2.15.0.

Internal tidy up to make code cleaner and more maintainable.

Some visuals that were hidden but still updated have now been removed.
This shouldn't make any difference but may result in a small performance gain.

As ever, there's a risk that new things break old things. If you encounter problems, please try to
isolate them, make them repeatable, and let me know.

## 3.12.3

Compromising on gradient colour palette by using the v3 basis but slightly improving saturation for clarity.

## 3.12.2

Fixes extreme slowness of 3.12.1. Had left some experimental code in there.

## 3.12.1

Late update from the pixel fairies. In **Display Settings** there is now a slider that changes the size of the
dots used for live previews. Reducing the size makes them less obtrusive/offensive/obvious. Please note that
this will change immediately in the 3D views but on the Map, will only update when the preview needs to be
changed, such as when you change a tool setting or move the pointers.

The code that finds tight radius bends in **Bend Problems** (radius mode) has been re-written and is now
both clearer and more robust.

## 3.12.0

The pixel fairies have crawled over the UI and given it a spring-clean. It's somewhat
reminiscent of 2.9.5, which should please some. Under the "gear wheel" global options menu
at top left, you can elect to have all the tools on the right (like v2) or have them
left and right (v3). You can still have light or dark colour themes and all that.

Top bar buttons have been slightly reorganised and harmonised.

Strava button is disabled when tracks are loaded, to prevent accidents.

Display settings is organised more tidily.

"Tools summary" is now "Tools options". The "Names only" display is more compact. The
tool reflect the single/dual dock option.

The Orange and Purple marker tools ("Essentials") are cleaner, and have the old v2 scroller as well.

Visual clutter is reduced by showing the video link and colour options only on open tools.

The pane layout menu is replaced by a simple, and I hope obvious, set of buttons.

The "About" view has gone. The release notes can be accessed from the "About this..." button
on the top bar, until you load a track when it is replaced by "Clear loaded route(s)".

## 3.11.12

Fixed bug in Route Builder that was causing too many "Key Places". It's better, not perfect.

## 3.11.11

Changed IP info provide to ip-info.io, which seems to support https for free, within limits.

## 3.11.10

New option in global settings (the gear wheel at top left) will show on the map the approximate
locations of GPXmagis users in the last 7 days (unless they use https).

## 3.11.9

Logs your IP details for the sole purpose of aggregate usage recording.
(No personal details are stored, there is no GDPR implication.)

## 3.11.8

Another Display Setting disables map rotation. Note that disabling this will not clear
any current rotation, but you can do that by clicking on the compass icon on the map.

## 3.11.7

New Display Settings allow you to:

* Choose Globe or Mercator map projection
* Allow or prevent map from being tilted

## 3.11.6

Improve hit detection when multiple tracks are loaded.

## 3.11.5

Extra button in Route Builder allows you to skip the "Snap" step.

## 3.11.4

Profile tool buttons do not overlap the chart.

Internal improvements in tool controller, may slightly improve UI response.

## 3.11.3

Fix colours of top bar popups in dark mode.

## 3.11.2

Fix display of previews in Map view after each edit.
Fix nasty Nudge bug when nudging the first point with non-zero fade.

## 3.11.1

Popup windows match colour theme. (You can read the text in Dark mode.)

## 3.11.0

### IMPORTANT

In 3.11 you can load more than one track.
Use the Clear button, or refresh your browser, to unload a previous track.

**Tool Summary** enhancements

This means you can run a really lean setup with all the tools hidden until you need them.

> Thanks to David Ogle for suggestions.

- Separate settings for which dock a tool lives in and whether a tool is visible.
- Click on the checkbox to hide or show a tool.
- Click on the tool name to open that tool, even if hidden (closes the tool if open).
- Option to sort tools alphabetically
- Option to show only the tool names, with the same click-to-open behaviour.
- These options are saved across sessions.

**Route Builder** replaces Route Maker

The purpose is to allow for more flexibility in composing new routes by combining old routes
and (when required) joining them by drawing on the Map. It's complicated but (I suggest) actually
more consistent conceptually than its predecessor.

- Opening a new track does not replace the current one;
- Drawing a track on the map does not replace the current track;
- _Clear loaded route(s)_ button in the top bar clears loaded route(s);
- Route Builder lists all tracks loaded, only one at a time is "active" for editing;
- Each track has its own Undo/Redo stacks;
- Routes can be hidden & revealed from the tool;
- All Route Builder functionality is migrated to this tool;
- Detection of nearby points works over all (visible) tracks, making it easier to join them;
- "Snap to nearby" will align common sections of road, on one or more tracks;
- Analysis of common sections works across tracks; reducing multiple tracks to "atomic' parts;
- You can rename track sections by editing the track name at the top of the page;
- The Undo button within the tool applies across the collection of tracks.

## Acknowledgements

* Thanks to all those who've provided support, comments, bug reports, and help along the way.

* Thanks to RGT for the Magic Roads concept and an excellent indoor cycling platform.

## Legal guff

Compatible with Strava, for the purpose of loading route and segment data.

GPXmagicV3 is open source at https://github.com/peterjamesward/GPXmagicV3

Contains numerous libraries under various licence terms, all of which are available in source
form via https://package.elm-lang.org.

Map component provided by MapBox.com.

Land use data courtesy of Open Street Map via the Overpass API.

Icons from www.flaticon.com/free-icons/.

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

Cookie policy is "use no cookies". This may not apply to third-party components.
    """
