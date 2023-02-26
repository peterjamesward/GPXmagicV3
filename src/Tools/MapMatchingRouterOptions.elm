module Tools.MapMatchingRouterOptions exposing (Options, RouteState(..))


type RouteState
    = RouteIdle
    | RouteDrawing
    | RouteComputing
    | RouteShown
    | RouteAdopted


type alias Options =
    { numPoints : Int
    , routeState : RouteState
    }
