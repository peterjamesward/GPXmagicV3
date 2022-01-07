module Actions exposing (..)

-- Not sure, thinking about putting post-update stuff here so it's commonly accessible.

import MapPortsController
import ModelRecord exposing (Model(..), ModelRecord)
import SceneBuilder exposing (render3dView)
import ViewingMode exposing (ViewingMode(..))


updateAllDisplays : ModelRecord -> ( ModelRecord, Cmd msgB )
updateAllDisplays model =
    ( model |> renderModel
    , if model.viewMode == ViewMap then
        Cmd.batch
            -- Must repaint track on so that selective rendering works.
            [ MapPortsController.addTrackToMap model
            , MapPortsController.centreMapOnCurrent model
            ]

      else
        Cmd.none
    )


renderModel : ModelRecord -> ModelRecord
renderModel model =
    { model | scene = render3dView model }
