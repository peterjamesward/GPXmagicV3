module ViewDerivativesContext exposing (..)

import Angle exposing (Angle)
import DomainModel exposing (EarthPoint)


type DragAction
    = DragNone
    | DragPan


type alias DerivativesContext =
    { dummy : Int }
