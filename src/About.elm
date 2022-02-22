module About exposing (..)


aboutText =
    """

# GPXmagic v3.0.5 (32e5bc30)

## In this release

* Map starts in "points aren't draggable mode"
* When points are draggable, ignores clicks that do not move a point
* Switches from "Info" to "Perspective" when track is loaded
* Fixed bug in the saving of Display settings
* Nudge sliders have 5cm (2in) increments, and you can use arrow keys one you've click a slider

## Still to-do from v2

1. Move & Stretch
2. Loops (inc. impact on Bezier & Centroid)
3. Intersection detection ((?? + loop detection ??))
4. Split and Join
5. Graph Theory (renamed)

## Legal guff

> peterjamesward/GPXmagicV3 is licensed under the
> Creative Commons Zero v1.0 Universal license

Source code available: https://github.com/peterjamesward/GPXmagicV3

    """
