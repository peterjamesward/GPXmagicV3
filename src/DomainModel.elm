module DomainModel exposing (..)

import Angle exposing (Angle)
import Length
import Quantity exposing (Quantity)

type alias GPXPoint =
    -- Rigorous third time around.
    { longitude : Angle
    , latitude : Angle
    , altitude : Quantity Float Length.Meters
    }