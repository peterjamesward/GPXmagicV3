module About exposing (..)


aboutText =
    """

# GPXmagic v3.0.3 (049e9275)

## In this release

* Radius bend preview removed when track is changed.
* Orange and Purple pointers are reset when track is changed.
* Radiused bends sometimes got the bend direction wrong.
* First-person "Rider view" added.
* Fetch elevations from map has transitioned. Caveats about accuracy apply.
* SVG file loading transitioned.

## Still to-do from v2

1. Fly-through
2. Use Strava segment data
3. Move & Stretch
4. Loops (inc. impact on others, such as Bezier)
5. Intersection detection ((?? JB loop detection ??))
6. Graph Theory (renamed)
7. Option to show MR rendering cutoff.

## Legal guff

> peterjamesward/GPXmagicV3 is licensed under the
> Creative Commons Zero v1.0 Universal license

Source code available: https://github.com/peterjamesward/GPXmagicV3

    """
