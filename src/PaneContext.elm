module PaneContext exposing (..)

import LocalCoords exposing (LocalCoords)
import MapTypes
import MapViewer
import Scene3d exposing (Entity)
import View3dCommonElements
import ViewGraphContext
import ViewMapContext
import ViewMode exposing (ViewMode)
import ViewProfileChartContext


type PaneLayout
    = PanesOne
    | PanesLeftRight
    | PanesUpperLower
    | PanesOnePlusTwo
    | PanesGrid


type SliderState
    = SliderIdle
    | SliderMoved


type alias PaneLayoutOptions =
    { paneLayout : PaneLayout
    , popupVisible : Bool
    , pane1 : PaneContext
    , pane2 : PaneContext
    , pane3 : PaneContext
    , pane4 : PaneContext
    , sliderState : SliderState
    , scene3d : List (Entity LocalCoords)
    , mapState : MapTypes.MapClickLocation
    , viewBeforeRouteViewForced : Maybe ViewMode
    , mapData : MapViewer.MapData
    }


type PaneId
    = Pane1
    | Pane2
    | Pane3
    | Pane4


type alias PaneContext =
    { paneId : PaneId
    , activeView : ViewMode
    , thirdPersonContext : Maybe View3dCommonElements.Context
    , firstPersonContext : Maybe View3dCommonElements.Context
    , mapContext : Maybe ViewMapContext.MapContext
    , profileContext : Maybe ViewProfileChartContext.ProfileContext
    , planContext : Maybe View3dCommonElements.Context
    , graphContext : Maybe ViewGraphContext.GraphContext
    }


paneIdToString : PaneId -> String
paneIdToString paneId =
    case paneId of
        Pane1 ->
            "1"

        Pane2 ->
            "2"

        Pane3 ->
            "3"

        Pane4 ->
            "4"
