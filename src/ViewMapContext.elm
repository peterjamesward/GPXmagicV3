module ViewMapContext exposing (..)


type alias MapContext =
    { mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , followOrange : Bool
    , draggable : Bool
    , mapStyleMenuOpen : Bool
    , mapStyle : MapStyle
    }


default : MapContext
default =
    { mapClickDebounce = False
    , lastMapClick = ( 0, 0 )
    , followOrange = True
    , draggable = False
    , mapStyleMenuOpen = False
    , mapStyle = MapOutdoors
    }


type MapStyle
    = MapBasic
    | MapStreets
    | MapSatellite
    | MapSatelliteStreets
    | MapOutdoors
    | MapLight
