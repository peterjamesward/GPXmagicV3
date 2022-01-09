module ToolsProforma exposing (..)

import Color exposing (Color)
import DomainModel exposing (PeteTree)
import Element exposing (Element)
import Json.Encode as E
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)


type ToolState
    = Expanded Bool
    | Contracted
    | Disabled


type ToolDock
    = DockUpperLeft
    | DockLowerLeft
    | DockUpperRight
    | DockLowerRight
    | DockBottom
    | DockNone


type alias ToolEntry model msg =
    { label : String
    , info : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , view : model -> Element msg
    , tabOpened : model -> model
    , tabClosed : model -> model
    , trackChanged : model -> model
    , preparePreview : model -> model
    , preview3D : Maybe (PeteTree -> List (Entity LocalCoords))
    , previewProfile : Maybe (PeteTree -> List (Entity LocalCoords))
    , previewMap : Maybe (PeteTree -> E.Value)
    , tabColour : Color
    }
