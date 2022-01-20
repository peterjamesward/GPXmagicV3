module PaneLayoutManager exposing (..)

import Element exposing (Element, none)
import ViewContext exposing (ViewContext, ViewMode)


type PaneType
    = PaneWithMap
    | PaneNoMap


type PaneLayout
    = PanesOne
    | PanesLeftRight
    | PanesUpperLower
    | PanesOnePlusTwo
    | PanesGrid


type PaneId
    = Pane1
    | Pane2
    | Pane3
    | Pane4


type alias PaneContext =
    { paneId : PaneId
    , activeView : ViewMode
    , thirdPersonContext : ViewContext
    }


type alias Options =
    { paneLayout : PaneLayout
    , popupVisible : Bool
    }


defaultOptions =
    { paneLayout = PanesOne
    , popupVisible = False
    }


type Msg
    = SetPaneLayout PaneLayout


paneLayoutMenu : (Msg -> msg) -> Options -> Element msg
paneLayoutMenu msgWrapper options =
    none
