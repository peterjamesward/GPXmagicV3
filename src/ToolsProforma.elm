module ToolsProforma exposing (..)

import Color exposing (Color)
import DomainModel exposing (PeteTree)
import Element exposing (Element)
import FlatColors.AussiePalette
import Json.Encode as E
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)


type ToolState
    = Expanded
    | Contracted
    | Disabled


type ToolDock
    = DockUpperLeft
    | DockLowerLeft
    | DockUpperRight
    | DockLowerRight
    | DockBottom
    | DockNone


type ToolType
    = ToolTrackInfo


type alias ToolEntry =
    { toolType : ToolType
    , label : String
    , info : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , tabColour : Element.Color
    , isPopupOpen : Bool
    }


tools : List ToolEntry
tools =
    -- One list or five, or six? Try one.
    [ toolEntryForTrackInfoBox
    ]


toolEntryForTrackInfoBox : ToolEntry
toolEntryForTrackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Summary info"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperLeft
    , tabColour = FlatColors.AussiePalette.beekeeper
    , isPopupOpen = False
    }
