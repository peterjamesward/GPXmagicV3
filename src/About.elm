module About exposing (..)


aboutText =
    """
# GPXmagic v3.1.1

(7f551b2e)

## In this release ...

* When you're building a route in _Route maker_ and you want to "double back" along the same Road,
you can now click on the Place and add a loop back to the same Place. Use the Radius slider in the
Route maker tool to adjust the radius. You can then add this loop to your route; be sure to check
the direction you use!

* _Route maker_ will warn if there are many Places (more than 10% of trackpoints). Remember, it's
intended for use with _planned_ routes, not _recorded_ rides. (Thanks David, again.)

* The joins at Places are better but I advise you to always check and smooth them after you have
created the new route.

* Some bug fixes.

## Coming up, not necessarily in order ...

1. Terrain
2. Land use (what?)
3. French (quoi?)

## Legal guff

> peterjamesward/GPXmagicV3 is licensed under the
> Creative Commons Zero v1.0 Universal license

Source code available: https://github.com/peterjamesward/GPXmagicV3

    """
