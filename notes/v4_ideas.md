
> There may never be a V4. V3 seems pretty much "feature complete", no-one 
> is crying out for more functions, performance seems to be adequate, the 
> main driver would be personal pride in creating something of quality.

That said, there are ideas that have been bouncing around, possibly even 
since the early days.

The tree was a great thing in v3. Without it, long routes were just not 
possible; the ease of elision at various level of detail underpins the SVG 
profile, the 3D views with local enhancement. Sadly not the Map view, 
although that is probable fixable (it was a question of perceived quality 
and would need to work in the background).

Yet, the tree structure takes no heed of any inherent "shape" in the route, 
and that may be an area to exploit -- or at least to explore.

Consider the loop that takes in the Lacets de Montvernier. How would we 
describe that to another cyclist?

> It's mostly straight along the valley, then a steep climb up a series of 
> tight switchbacks, before it flattens out for a bit over a plateau, then a 
> long twisty but largely open descent to return to the start.

See? We divide the route into sections, each of a specific character. And 
this is a recursive process, as the "series of tight switchbacks" is 
enhanced as:

> A climb of about 2.5km, averaging 10%, made up of 17 hairpin turns, 
> starting to the right and then alternating, joined by straights that 
> decrease from about 150 at the bottom to about 50m at the top. The road is 
> only about 4m wide at most.

Hence we now have, essentially, with "obvious" notation:

> R180(r4), S150, L180(r4), S100, L180(r4), ... R180(r5), S200.

Each of the elements here will have several point, yet the whole sequence 
can also be collapsed to:

> Switchback Climb, 2.5km, 10%, 17 turns, min radius 4m.

and the whole loop to:

> Loop 12.8km, 350m ascent in one climb of 2.5km.

Which is a good summary and the first expansion would repeat our initial 
description.

I guess the questions are:
* Is that _useful_?
* Would it have similar benefits to the tree?
* Is it "implementable"?

