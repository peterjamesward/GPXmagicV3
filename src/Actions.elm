module Actions exposing (..)

-- Not sure, thinking about putting post-update stuff here so it's commonly accessible.

import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element
import MapPortsController
import TrackLoaded exposing (TrackLoaded)


type ToolAction msg
    = SetCurrent Int
    | ShowPreview String Element.Color (List ( EarthPoint, GPXSource ))
    | HidePreview String
    | DelayMessage Int msg
    | NoAction


updateAllDisplays : TrackLoaded -> Cmd msg
updateAllDisplays track =
    --if track.viewMode == ViewMap then
        Cmd.batch
            -- Must repaint track on so that selective rendering works.
            [ MapPortsController.addTrackToMap track
            , MapPortsController.centreMapOnCurrent track
            ]

    --else
    --    Cmd.none


setCurrentPosition : Int -> TrackLoaded -> Cmd msg
setCurrentPosition position model =
    { model | currentPosition = position }
        |> updateAllDisplays
