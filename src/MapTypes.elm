module MapTypes exposing (..)


type alias MapInfo =
    -- Mainly used to set the map up.
    { mapZoom : Float -- track values from user map interactions.
    , centreLon : Float
    , centreLat : Float
    }


type alias MapState =
    -- Introduced to debounce map messages.
    { lastClickLon : Float
    , lastClickLat : Float
    }

