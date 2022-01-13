module Actions exposing (..)

-- This wee DSL allows any tool to ask Main to update the model and display stuff
-- (including on the Map) without the tools needing knowledge of the model or ports.

import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element


type ToolAction msg
    = SetCurrent Int
    | ShowPreview String PreviewShape Element.Color (List ( EarthPoint, GPXSource ))
    | HidePreview String
    | DelayMessage Int msg
    | NoAction

type PreviewShape
    = PreviewCircle
    | PreviewLine
