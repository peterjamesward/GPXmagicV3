module Actions exposing (..)

-- Not sure, thinking about putting post-update stuff here so it's commonly accessible.

import ModelRecord exposing (Model (..), ModelRecord)
import MapPortsController
import SceneBuilder exposing (render3dView)
import ViewingMode exposing (ViewingMode(..))


updateAllDisplays : (MapPortsController.MapMsg -> msgB) -> ModelRecord -> ( Model, Cmd msgB )
updateAllDisplays  msgWrapper model =
    ( model
        |> renderModel
        |> Model
    , if model.viewMode == ViewMap then
        Cmd.batch
            -- Must repaint track on so that selective rendering works.
            [ MapPortsController.addTrackToMap model msgWrapper
            , MapPortsController.centreMapOnCurrent model msgWrapper
            , MapPortsController.deferredMapRepaint msgWrapper
            ]

      else
        Cmd.none
    )


renderModel : ModelRecord -> ModelRecord
renderModel model =
    { model | scene = render3dView model }
