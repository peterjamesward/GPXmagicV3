module ViewMapContext exposing (..)


type alias MapContext =
    { mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , followOrange : Bool
    , draggable : Bool
    , mapStyleMenuOpen : Bool
    , mapStyle : MapStyle
    }


type MapStyle
    = MapBasic
    | MapStreets
    | MapSatellite
    | MapSatelliteStreets
    | MapOutdoors
    | MapLight
