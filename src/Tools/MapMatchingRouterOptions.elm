module Tools.MapMatchingRouterOptions exposing
    ( Intersection
    , Leg
    , Matching
    , Matchings
    , Options
    , RouteState(..)
    , Step
    , matchingsDecoder
    )

import Json.Decode as D


type RouteState
    = RouteIdle
    | RouteDrawing
    | RouteComputing
    | RouteShown
    | RouteFailed


type alias Options =
    { numPoints : Int
    , routeState : RouteState
    }



-- Types for receiving route as GeoJson
{- Here is the essence of the JSON returned from matching.
   {
     "matchings": [
       {
         "legs": [
           {
             "steps": [
               {
                 "intersections": [
                   {
                     "location": [
                       -4.196806,
                       50.376248
                     ]
                   }
                 ],
                 "maneuver": {
                   "location": [
                     -4.196806,
                     50.376248
                   ]
                 }
               },
             ],
           }
         ],
       }
     ],
   }
-}


type alias Matchings =
    { matchings : List Matching }


matchingsDecoder : D.Decoder Matchings
matchingsDecoder =
    D.map Matchings
        (D.field "matchings" (D.list matchingDecoder))


type alias Matching =
    { legs : List Leg }


matchingDecoder : D.Decoder Matching
matchingDecoder =
    D.map Matching
        (D.field "legs" (D.list legDecoder))


type alias Leg =
    { steps : List Step }


legDecoder : D.Decoder Leg
legDecoder =
    D.map Leg
        (D.field "steps" (D.list stepDecoder))


type alias Step =
    { intersections : List Intersection
    , maneuver : Maneuver
    }


stepDecoder : D.Decoder Step
stepDecoder =
    D.map2 Step
        (D.field "intersections" (D.list intersectionDecoder))
        (D.field "maneuver" maneuverDecoder)


type alias Intersection =
    { location : List Float }


intersectionDecoder : D.Decoder Intersection
intersectionDecoder =
    D.map Intersection
        (D.field "location" (D.list D.float))


type alias Maneuver =
    { location : List Float }


maneuverDecoder : D.Decoder Maneuver
maneuverDecoder =
    D.map Maneuver
        (D.field "location" (D.list D.float))
