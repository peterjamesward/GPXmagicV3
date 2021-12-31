module GpxParser exposing (..)

import Angle
import Direction3d
import DomainModel exposing (EarthVector, GPXSource, makeEarthVector)
import Length
import Quantity
import Regex
import Spherical
import Vector3d


asRegex t =
    -- Helper to make a regex pattern.
    Maybe.withDefault Regex.never <| Regex.fromString t


parseTrackName xml =
    case Regex.find (asRegex "<name>(.*)<\\/name>") xml of
        [] ->
            Nothing

        x :: _ ->
            case x.submatches of
                [] ->
                    Nothing

                n :: _ ->
                    n


parseGPXPoints : String -> List EarthVector
parseGPXPoints xml =
    let
        trkpts =
            Regex.find (asRegex "<trkpt((.|\\n|\\r)*?)trkpt>") xml |> List.map .match

        latitude trkpt =
            Regex.find (asRegex "lat=\\\"([\\d\\.-]*)\\\"") trkpt |> matches

        longitude trkpt =
            Regex.find (asRegex "lon=\\\"([\\d\\.-]*)\\\"") trkpt |> matches

        elevation trkpt =
            Regex.find (asRegex "<ele>([\\d\\.-]*)<\\/ele>") trkpt |> matches

        trackPoint trkpt =
            case ( latitude trkpt, longitude trkpt, elevation trkpt ) of
                ( (Just lat) :: _, (Just lon) :: _, (Just ele) :: _ ) ->
                    Just
                        { longitude = Angle.degrees lon
                        , latitude = Angle.degrees lat
                        , altitude = Length.meters ele
                        }

                ( (Just lat) :: _, (Just lon) :: _, _ ) ->
                    Just
                        { longitude = Angle.degrees lon
                        , latitude = Angle.degrees lat
                        , altitude = Quantity.zero
                        }

                _ ->
                    Nothing


        earthVector trkpt =
            -- This just to remove anything with a weird combination of values.
            case ( latitude trkpt, longitude trkpt, elevation trkpt ) of
                ( (Just lat) :: _, (Just lon) :: _, (Just alt) :: _ ) ->
                    Just <| makeEarthVector (Angle.degrees lon) (Angle.degrees lat) (Length.meters alt)

                ( (Just lat) :: _, (Just lon) :: _, _ ) ->
                    Just <| makeEarthVector (Angle.degrees lon) (Angle.degrees lat) (Length.meters 0.0)

                _ ->
                    Nothing

        matches xs =
            List.map value xs

        value x =
            case x.submatches of
                (Just val) :: _ ->
                    String.toFloat val

                _ ->
                    Nothing

        trackPoints =
            trkpts
                |> List.map trackPoint
                |> List.filterMap identity
    in
    trkpts |> List.map earthVector |> List.filterMap identity
