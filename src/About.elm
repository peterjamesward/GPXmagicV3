module About exposing (..)


aboutText =
    """
# GPXmagic v3.0.8 (b2d61024)

## In this release ...

* Blue buttons make a long-awaited return
* Looped track now transitioned from v2, now called "Start/Finish"
* Closing a loop now uses a Bezier spline to introduce a curve at the start/finish
* For non-looped tracks, Start/Finish provides to add RGT start and end pens by simply extending the track linearly
* "Split & Join" transitioned from v2
* "Intersections" transitioned from v2 and now highlights sections of track that are re-used, and in which direction
* The Profile view has lost its colours again, but gained the ability to show previews
* "Out and Back" lowers the return leg by 1cm to avoid track flicker on RGT (thanks David Ogle)
* Bezier splines and Centroid average preview on Profile view (for whole track only)
* "Limit gradients" renamed to "Smooth Profile"
* "Smooth Profile" provides a gradient limiting function on the whole track
* "Smooth Profile" provides a "rolling average" altitude smoother on the whole track
* "Smooth Profile" provides a "rolling average" gradient smoother on the whole track
* All the above give a live preview on Profile view, and others
* Tools are coloured to allow easier search; you can of course change these colours
* All tools default to upper right dock, similar to v2.

## Coming up, not necessarily in order ...

1. Graph theory
2. Terrain
3. French

## Legal guff

> peterjamesward/GPXmagicV3 is licensed under the
> Creative Commons Zero v1.0 Universal license

Source code available: https://github.com/peterjamesward/GPXmagicV3

    """
