module Actions exposing (..)

-- Not sure, thinking about putting post-update stuff here so it's commonly accessible.

import Delay exposing (after)
import ModelRecord exposing (Model (..), ModelRecord)
import Msg exposing (Msg(..))
import PortController
import SceneBuilder exposing (render3dView)
import ViewingMode exposing (ViewingMode(..))


updateAllDisplays : ModelRecord -> ( Model, Cmd Msg )
updateAllDisplays model =
    ( model
        |> renderModel
        |> Model
    , if model.viewMode == ViewMap then
        Cmd.batch
            -- Must repaint track on so that selective rendering works.
            [ PortController.addTrackToMap model
            , PortController.centreMapOnCurrent model
            , after 10 RepaintMap
            ]

      else
        Cmd.none
    )


renderModel : ModelRecord -> ModelRecord
renderModel model =
    { model | scene = render3dView model }
