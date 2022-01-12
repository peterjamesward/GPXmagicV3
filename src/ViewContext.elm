module ViewContext exposing (..)

import ViewContextThirdPerson exposing (ThirdPersonContext)
import ViewMap


type ViewMode
    = ViewInfo
    | ViewThird
    | ViewFirst
    | ViewPlan
    | ViewProfile
    | ViewMap


type ViewContext
    = ThirdPersonContext ThirdPersonContext
    | MapContext ViewMap.MapContext
    | InfoContext
