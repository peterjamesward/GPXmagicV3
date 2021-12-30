module ViewingContext exposing (..)

import Angle exposing (Angle)
import Length
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import ViewingMode exposing (ViewingMode)


type alias ViewingContext =
    -- The information we need to paint a scene on the screen.
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , distance : Quantity Float Length.Meters
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : Point3d Length.Meters LocalCoords
    , viewingMode : ViewingMode
    , contextId : Int -- ( 0 = Plan, 1 = First Person, 2 = Profile, 3 = Third person)
    , waitingForClickDelay : Bool
    , mapClickToDrag : Bool
    }


type DragAction
    = DragNone
    | DragRotate
    | DragPan
    | DragProfile
    | DragPlan


defaultViewingContext : ViewingContext
defaultViewingContext =
    { azimuth = Angle.degrees -90.0
    , elevation = Angle.degrees 30.0
    , distance = Length.meters 100.0
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = 12.0
    , defaultZoomLevel = 12.0
    , focalPoint = Point3d.origin
    , viewingMode = ViewingMode.ViewThird
    , contextId = 0
    , waitingForClickDelay = False
    , mapClickToDrag = True
    }


newViewingContext : ViewingMode -> ViewingContext
newViewingContext mode =
    { defaultViewingContext | viewingMode = mode }

