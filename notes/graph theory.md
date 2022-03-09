# My own thoughts

Let user select track point match tolerance.
Let user manually coalesce two track-points.
Let user manually remove unwanted track-point locally.
Apply 1cm drop on all traversals bar the first for each leg.
Use some of DO's terminology.
Allow only a few edits on one edge at a time.
Use SVG overlays to label edges for easier selection.

# Emails from David Ogle

> After using graph theory you end up with out and back track points in the same position...
> say the orange marker is near the end of the course and I select a track point nearby,
> it always selects the one at the start of the course, instead of the one I was trying to
> select near the end. So I'm thinking if you've got 2 track points in the same position,
> on selection (mouse click), it'd be good to select the one closest to the current position.


Here goes... few ideas for improvements:

- Rename the tool name from "Graph Theory" to "Track Builder".

- Simplify the naming:
  -- "Node" to be called "Track Connector".
  -- "Edge" to be called "Track Section".

- Rename the buttons:
  -- "Convert to Graph" to "Convert to Track Sections"
  -- "Convert from Graph" to "Build Track"
  -- "Remove traversal last in list" to "Remove last traversal"

- Add a tooltip to the "Offset" slider to explain what that's used for.
  (you make reference to it in the info pop-up, but most won't see that)

- Add an info label to explain that a "Traversal" is a movement from one Track Connector to another.
  (guessing there might be quite a few users that won't immediately understand what is meant by Traversal)

- Add a "Track Progression" heading to the traversal list to make it obvious what the list is.

- When the track is zoomed out and you click "Show" on the Traversal list it's difficult to see the Traversal highlighted (you need to be pretty zoomed in to see the arrows) - improve it so you can easily see the Traversals and Track Connectors when zoomed out.


To take it to the next level:

- When building the track ("Convert from Graph") automatically nudge down the "Backwards" sections by 1cm (to avoid the road flickering issue).
  (or offer it as an option next to "Offset"... although I can't think why you'd ever not want to do it)

- Number the Track Connectors.
  (ideally show the Track Connector number on the red icon when zoomed in enough)

- Modify the Traversal list so From/To is using the Track Connector numbers.
  (using this you could possibly do away with showing "Direction")

- Add the option to navigate between the Track Connectors (using forward/back buttons like you do for the "Gradient Problems" tool).

- Add the option to delete a Track Connector (delete button between the Track Connector forward/back buttons?).
  (deleting the Track Connector would automatically extend the adjacent Track Section(s) as required)

- The "Add traversal at Orange marker" button confused me when I first used the tool. If you number the Track Connectors I think it'd be more intuitive to do this:
  -- Using the Track Connector forward/back buttons it'd give you an active Track Connector - if the active Track Connector is the last "To" position in the traversal list then you could offer contextual "Add Traversal" buttons:

  Basic example of an out and back with a roundabout:

    1) The active Track Connector is #1 (at the start) - you'd only show 1 button:
       a) "Add Forward Traversal (from 1 to 2)"

  On button click it adds the Traversal and automatically moves to Track Connector #2...

    2) Track Connector #2 is next to a roundabout - you'd show 2 buttons:
       a) "Add Loop Traversal (from 2 back to 2)"
       b) "Add Backward Traversal (from 2 to 1)"

  On button (a) click it adds the Traversal and the active Track Connector is still #2...

    3) You'd show the same 2 buttons:
       a) "Add Loop Traversal (from 2 back to 2)"
       b) "Add Backward Traversal (from 2 to 1)"

  On button (b) click it adds the Traversal and automatically moves back to Track Connector #1.

  This would display the following Traversal list:

<image.png>

Dave

No worries - it started as a couple of notes and snowballed a bit 😀

One thing I missed out of my notes... the big frustration with it when I first used it was Track Connectors (nodes) appearing in the wrong place, or not appearing where I wanted them, with no obvious way to solve it. I eventually realised you had to go into map mode and drag the trackpoints around a bit to sort it - but again, can't see many people bothering to do that. Something that could potentially work:

- When Converting to Graph offer 2 options (radio buttons could work for this):
    1) Auto-detect Track Connectors (how it works now)
    2) Manually set Track Connectors

- If "Manually set Track Connectors" is selected simply set 2 Track Connectors - 1 at the start and 1 at end. Then allow the user to click on any trackpoint and "Add Track Connector".
  This would make it really quick/easy to set the Track Sections you want.

Guessing there will be a few complications around adding Track Connectors, but it'd be great if it would eventually work like that.

I know you like your album analogies... if you managed to nail all this, V3 could be your Sgt Peppers 😁

Dave

>  I wonder how many people use it, it seems somewhat niche.

I'm guessing very few use it at the moment as they find it too technical/intimidating to use (even Samir, who builds loads of magic roads, doesn't use it yet!). I honestly think with the changes it could end up being pretty popular though.

> There are challenges in auto-detect. 

Understood. That'd be the beauty of being able to manually add/delete Track Connectors... even if the auto-detect hasn't set it up as you require you can easily modify.

> BTW, the problem with using connector numbers to route is how to resolve when there are two or more valid sections, which is why I ended up using the edge to select the next direction.

You could possibly give each Track Section a unique identifier, then specify that in the contextual "Add Traversal" buttons where necessary, e.g.:

a) "Add Backward Traversal (from 10 to 9)"
b) "Add Forward Traversal (from 10 to 11 on Section C)"
c) "Add Forward Traversal (from 10 to 11 on Section D)"

With the contextual "Add Traversal" buttons approach it keeps it really simple (the users are guided through the possible movements).

Dave