
# Thoughts on smoothing via tree

Does the tree suggest a different approach to elevantion smoothing?

A recursive, top-down approach?

In general, goals are;

- replicate holistic shape of the course, seen as altitude = f distanceFromStart.
- limit gradient to given % (perhaps except very rarely, so a 99% figure)
- limit rate of change of gradient to given %/m
- preserve altitudes at maxima and minima where possible

Idea is tp perceive sub trees as sections that portray an outline to be filled in
with increasing detail. Replace and refine by sections that conform to limits.

Add points as needed, especially to smooth gradient changes, but also to 'spread'
climbs to preserve altitude changes.

No fears of having, say, points every metre for 100km.

Only preserve "significant" altitude changes - must also eliminate "noise" but not all undulations.

> Gadz, this may work!

May need more in tree; we have aleady trueLength, altitude change, gradient (and peak).