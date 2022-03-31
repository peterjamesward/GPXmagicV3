# GPXmagicV3
GPXmagic version 3

Version 3 is a substantial rewrite and upgrade for GPXmagic.

It's based on a totally new core data structure -- what I call a "densely aggregated binary tree".
Compared to a list of track points (as in v1 and v2), this gives:

- Easy approximate rendering at any detail, including variable level of detail;
- In-situ editing for small changes;
- Single traversal for building and most other operations;
- O(log N) access to any point by distance or index number;
- (Small) constant speed access for common requests such as length, direction;
- Able to handle orders of magnitudes more track points without excessive memory use or slow-down.

Besides that, there's a consistent structure for adding new tools based around a 
simple and extensible internal command language. So far, this seems flexible, stable and not too convoluted.

