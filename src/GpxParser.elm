module GpxParser exposing (..)

import Angle
import DomainModel exposing (GPXPoint, GPXTrack)
import Length
import Quantity
import Regex


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


parseGPXPoints : String -> GPXTrack
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

        ( longitudes, latitudes ) =
            ( trackPoints |> List.map .longitude
            , trackPoints |> List.map .latitude
            )

        ( minLon, maxLon ) =
            ( Quantity.minimum longitudes |> Maybe.withDefault Quantity.zero
            , Quantity.maximum longitudes |> Maybe.withDefault Quantity.zero
            )

        ( minLat, maxLat ) =
            ( Quantity.minimum latitudes |> Maybe.withDefault Quantity.zero
            , Quantity.maximum latitudes |> Maybe.withDefault Quantity.zero
            )

        referencePoint =
            { longitude = Quantity.interpolateFrom minLon maxLon 0.5
            , latitude = Quantity.interpolateFrom minLat maxLat 0.5
            , altitude = Quantity.zero
            }
    in
    { points = trackPoints
    , referenceLonLat = referencePoint
    }
