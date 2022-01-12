module ViewContext exposing (..)

import ViewContextThirdPerson exposing (ContextThirdPerson)
import ViewMap


type ViewMode
    = ViewInfo
    | ViewThird
    | ViewFirst
    | ViewPlan
    | ViewProfile
    | ViewMap


type ViewContext
    = ThirdPersonContext ContextThirdPerson
    | MapContext ViewMap.MapContext
    | InfoContext
