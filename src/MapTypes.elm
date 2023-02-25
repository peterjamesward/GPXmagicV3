module MapTypes exposing (..)


type alias MapInfo =
    -- Mainly used to set the map up.
    { mapZoom : Float -- track values from user map interactions.
    , centreLon : Float
    , centreLat : Float
    }


type alias MapClickLocation =
    -- Introduced to debounce map messages.
    { lastClickLon : Float
    , lastClickLat : Float
    }


type MapState
    = MapDivNeeded
    | MapNotLoaded
    | MapReady
    | MapBusy
