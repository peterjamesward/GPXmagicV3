module Spherical exposing (findBearingToTarget, metresPerDegree, metresPerPixel, range)

-- Need some coordinate mangling
-- https://www.movable-type.co.uk/scripts/latlong.html

import Angle exposing (Angle)


meanRadius =
    6371000


metresPerDegree =
    --78846.81
    meanRadius * pi / 180.0


metresPerPixelAtEquatorZoomZero =
    78271.484


metresPerPixel : Float -> Angle -> Float
metresPerPixel zoomLevel latitude =
    Angle.cos latitude * metresPerPixelAtEquatorZoomZero / 2.0 ^ zoomLevel



-- Equirectangular approximation


range : ( Angle, Angle ) -> ( Angle, Angle ) -> Float
range lonLat1 lonLat2 =
    let
        ( lat1, lon1 ) =
            ( Angle.inRadians <| Tuple.second lonLat1
            , Angle.inRadians <| Tuple.first lonLat1
            )

        ( lat2, lon2 ) =
            ( Angle.inRadians <| Tuple.second lonLat2
            , Angle.inRadians <| Tuple.first lonLat2
            )

        x =
            (lon2 - lon1) * cos ((lat1 + lat2) / 2)

        y =
            lat2 - lat1
    in
    meanRadius * sqrt (x * x + y * y)


findBearingToTarget ( lat1, lon1 ) ( lat2, lon2 ) =
    let
        y =
            sin (lon2 - lon1) * cos lat2

        x =
            cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos (lon2 - lon1)
    in
    atan2 y x



-- Find new lat long after travelling d metres on given bearing.
