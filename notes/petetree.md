
# Optimization, part 4

On the errors of clinging on

Previously, I have written about adding a simple quad-tree to GPXmagic. This allowed some operations to
work in time (roughly) proportional to the logarithm of the number of points.

My fundamental structure remained a list of GPS track points. Why not, that's the experience of riding
your bike or walking. You pass one point, then another.

Despite the simplicity, all evidence showed that the program struggles with a few tens of thousands of
points. The quad-tree was a temporary alleviation of symptoms. It was also a clear signal that the 
naive structure is not always the best, nor is that obvious.

I made a bad decision very early. I gave each point a numerical index, and put this into the list.
Seemed to make sense; given a point, I knew its index, given an index, I could find the point.
I also put "distanceFromStart" into the data for each point. This was great, I could easily access
pertinent information. I added "directionChange", with decent motivation, as finding sharp deviations
is key to the process of smoothing.

Upshot of these and similar choices was that loading a file required several passes over the list of
points, with code fragments such as: 

```elm
    firstPassSetsForwardLooking =
        List.map2
            deriveForward
            preFilterDuplicates
            (List.drop 1 preFilterDuplicates)
            ++ List.drop (List.length preFilterDuplicates - 1) preFilterDuplicates
```

It's all quite innocuous. It's the accretion that costs, and increasingly so as the list grows. What's
not obvious is the amount of memory allocation, de-allocation, and garbage collection behind the scenes 
for this code. Even that `List.length` is a walk along the whole list.

It's also noteworthy that "index" and "distanceFromStart" are **bad**. If I delete the second point in a
route, I have to update _all_ the remaining points. Is this sensible? No. Is it avoidable? Let's find out.

I did a "quick" test with a file of 304,000 points:

> Version 2.7.13 : <test broke Chrome, no results>  
> Version 2.8.1 : about 4 minutes, memory usage unavailable

(Hard to get data when the test breaks the tools.)

Finally, in the mindset of working on V3, I decided to try something different. I was musing about
using some blend of skip-list or difference-lists. One night, laying in the dark, as one does, I decided
to try a simple binary tree, with each node augmented with key data aggregated from its children.

```elm
type alias RoadSection =
    -- A piece of road, in local, conformal, space.
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    { startsAt : LocalPoint
    , endsAt : LocalPoint
    , boundingBox : BoundingBox3d Meters LocalCoords
    , trueLength : Quantity Float Meters
    , skipCount : Int
    }


type
    PeteTree
    -- Absurdly simple tree may work.
    = Leaf RoadSection
    | Node
        { nodeContent : RoadSection
        , left : PeteTree
        , right : PeteTree
        }
```

Much of interest here. There's no "index" for a point, but by tallying "skipCount" we can derive
a point from its index, or the index of a point. If I remove a point, I need update only its ancestor
nodes in the tree, not all the remaining points. Likewise, trueLength replaces "distanceFromStart".

Surely, though, Pete, building this structure will take a long time. You're basically doubling the 
space you need for the leaf nodes (if you know how to sum a geometric progression).

Well, the answer took a while. For a fair test, I needed to be able to load a file, populate the tree
structure, render it in graphics similar to v2, provide basic navigation by slider or click on the image.
These prove that our index mechanism works, and that we can search the tree efficiently without 
also having a quad-tree.

We can trivially use the structure to render at any depth, so implemented from the get-go selective
rendering giving more detail around the current point without the shenanigans involved in v2.

> Version 3 test : 4.5 seconds, 265MB heap

What? Yes, 4.5 seconds, not minutes. According to the Chrome developer tools, only 1.5s of this is
my script so (by inference) 3 seconds is just loading the file contents. I had to ask a friend to 
check on Windows. Same result: sub five seconds for 333,000 points.

Of course, this is a rather fundamental change, and all the edit functions need a re-write. Am I sorry?
No way!

Don't hang on to your early decisions, just because they seem innocent.

