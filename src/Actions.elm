module Actions exposing (..)

-- Not sure, thinking about putting post-update stuff here so it's commonly accessible.

import DomainModel exposing (PeteTree)
import LocalCoords exposing (LocalCoords)
import MapPortsController
import Scene3d exposing (Entity)
import SceneBuilder exposing (render3dView)
import ViewingMode exposing (ViewingMode(..))


updateAllDisplays :
    { model
        | viewMode : ViewingMode
        , scene : List (Entity LocalCoords)
        , currentPosition : Int
        , referenceLonLat : DomainModel.GPXSource
        , renderDepth : Int
        , trackTree : Maybe PeteTree
    }
    ->
        ( { model
            | viewMode : ViewingMode
            , scene : List (Entity LocalCoords)
            , currentPosition : Int
            , referenceLonLat : DomainModel.GPXSource
            , renderDepth : Int
            , trackTree : Maybe PeteTree
          }
        , Cmd msg
        )
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


setCurrentPosition :
    Int
    ->
        { model
            | viewMode : ViewingMode
            , scene : List (Entity LocalCoords)
            , currentPosition : Int
            , referenceLonLat : DomainModel.GPXSource
            , renderDepth : Int
            , trackTree : Maybe PeteTree
        }
    ->
        ( { model
            | viewMode : ViewingMode
            , scene : List (Entity LocalCoords)
            , currentPosition : Int
            , referenceLonLat : DomainModel.GPXSource
            , renderDepth : Int
            , trackTree : Maybe PeteTree
          }
        , Cmd msg
        )
setCurrentPosition position model =
    { model | currentPosition = position }
        |> updateAllDisplays


renderModel :
    { model
        | viewMode : ViewingMode
        , scene : List (Entity LocalCoords)
        , currentPosition : Int
        , referenceLonLat : DomainModel.GPXSource
        , renderDepth : Int
        , trackTree : Maybe PeteTree
    }
    ->
        { model
            | viewMode : ViewingMode
            , scene : List (Entity LocalCoords)
            , currentPosition : Int
            , referenceLonLat : DomainModel.GPXSource
            , renderDepth : Int
            , trackTree : Maybe PeteTree
        }
renderModel model =
    { model | scene = render3dView model }
