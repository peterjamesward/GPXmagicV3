module Actions exposing (..)

-- Not sure, thinking about putting post-update stuff here so it's commonly accessible.

import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element


type ToolAction msg
    = SetCurrent Int
    | ShowPreview String Element.Color (List ( EarthPoint, GPXSource ))
    | HidePreview String
    | DelayMessage Int msg
    | NoAction

