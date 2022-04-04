module About exposing (..)


aboutText =
    """
# GPXmagic v3.1.6

(77b5e292)

## In this release ...

* New _Land use_ tool simply displays the colour legend. (More to come.)

* The _Bend problems_ tool, in _With radius_ mode, has a new "Widen bend" button.
This attempts to widen the bend by the simple act of "kicking out" the track points
to roughly the desired radius. May need more than one attempt, depending on the
shape of the bend. (The GPXmagic elves are working on something better.)

## Legal guff

> peterjamesward/GPXmagicV3 is licensed under the
> Creative Commons Zero v1.0 Universal license

Source code available: https://github.com/peterjamesward/GPXmagicV3

Compatible with Strava, for the purpose of loading route and segment data.

Map component provided by MapBox.com

Land use data courtesy of Open Street Map via the Overpass API.

Your IP address may be logged for the purpose of aggregate usage recording; no personal details are stored.

No cookies are used, though many chocolate digestives were consumed whilst writing.


    """
