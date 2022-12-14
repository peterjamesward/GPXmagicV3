module LandUseDataTypes exposing (LandUseData, LandUseDisplay(..), LandUseNode, LandUseStatus(..), LandUseTags, LandUseWay, OSMLandUseData, OSMLandUseElement(..), OSMLandUseNode, OSMLandUseWay, emptyLandUse)

import Dict exposing (Dict)
import DomainModel exposing (EarthPoint)


type LandUseDisplay
    = LandUseHidden
    | LandUsePlanar
    | LandUseSloped


type alias OSMLandUseData =
    { elements : List OSMLandUseElement }


type alias LandUseData =
    { nodes : List LandUseNode
    , ways : List LandUseWay
    , rawData : OSMLandUseData
    , places : Dict String EarthPoint
    , status : LandUseStatus
    }


emptyLandUse : LandUseData
emptyLandUse =
    { nodes = []
    , ways = []
    , rawData = { elements = [] }
    , places = Dict.empty
    , status = LandUseNoTrack
    }


type LandUseStatus
    = LandUseNoTrack
    | LandUseWaitingOSM
    | LandUseWaitingMap
    | LandUseOK
    | LandUseError String


type OSMLandUseElement
    = OSMNode OSMLandUseNode
    | OSMWay OSMLandUseWay


type alias OSMLandUseNode =
    { type_ : String
    , id : Int
    , lat : Float
    , lon : Float
    , tags : LandUseTags
    }


type alias LandUseNode =
    { at : EarthPoint
    , tags : LandUseTags
    }


type alias OSMLandUseWay =
    { type_ : String
    , id : Int
    , nodes : List Int -- or List LandUseNode ??
    , tags : LandUseTags
    }


type alias LandUseWay =
    { nodes : List LandUseNode
    , tags : LandUseTags
    }


type alias LandUseTags =
    Maybe (Dict String String)
