module LandUseDataOSM exposing (..)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (GPXSource)
import Http
import Json.Decode as D exposing (Decoder)
import LandUseDataTypes exposing (..)
import Length
import MapPortController
import Point3d
import Quantity
import String.Interpolate
import TrackLoaded exposing (TrackLoaded)
import Url.Builder as Builder
import Utils


apiRoot =
    "https://overpass.kumi.systems"


requestLandUseData : (Result Http.Error OSMLandUseData -> msg) -> TrackLoaded msg -> Cmd msg
requestLandUseData msg track =
    Http.request
        { method = "POST"
        , headers = []
        , url = Builder.crossOrigin apiRoot [ "api", "interpreter" ] []
        , body = Http.stringBody "text/plain" <| queryFromBoundingBox track
        , expect = Http.expectJson msg landUseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


processLandUseData : Result Http.Error OSMLandUseData -> TrackLoaded msg -> ( LandUseData, Cmd msg )
processLandUseData results track =
    case results of
        Ok landUse ->
            -- Just save raw, process when we have altitudes from Map.
            ( { emptyLandUse
                | rawData = landUse
                , status = LandUseWaitingMap
              }
            , fetchAltitudesFromMap landUse
            )

        Err error ->
            ( { emptyLandUse | status = LandUseError <| Utils.errorToString error }
            , Cmd.none
            )


fetchAltitudesFromMap : OSMLandUseData -> Cmd msg
fetchAltitudesFromMap raw =
    let
        rawNodes : List OSMLandUseNode
        rawNodes =
            List.filterMap
                (\element ->
                    case element of
                        OSMNode node ->
                            Just node

                        _ ->
                            Nothing
                )
                raw.elements

        gpxLike =
            rawNodes
                |> List.map
                    (\rawNode ->
                        { longitude = rawNode.lon |> Angle.degrees |> Direction2d.fromAngle
                        , latitude = rawNode.lat |> Angle.degrees
                        , altitude = Quantity.zero
                        , timestamp = Nothing
                        }
                    )
    in
    MapPortController.fetchElevationsForPoints gpxLike


applyAltitudes : List (Maybe Float) -> TrackLoaded msg -> LandUseData
applyAltitudes altitudes track =
    let
        justRaw =
            track.landUseData

        groundLevel =
            DomainModel.boundingBox track.trackTree
                |> BoundingBox3d.minZ
                |> Quantity.minus Length.foot

        rawNodes : List OSMLandUseNode
        rawNodes =
            List.filterMap
                (\element ->
                    case element of
                        OSMNode node ->
                            Just node

                        _ ->
                            Nothing
                )
                justRaw.rawData.elements

        nodeDict : Dict Int LandUseNode
        nodeDict =
            Dict.fromList <|
                List.map2
                    (\rawNode maybeAltitude ->
                        case maybeAltitude of
                            Just altitude ->
                                ( rawNode.id
                                , { at =
                                        DomainModel.pointFromGpxWithReference
                                            track.referenceLonLat
                                            { longitude = rawNode.lon |> Angle.degrees |> Direction2d.fromAngle
                                            , latitude = rawNode.lat |> Angle.degrees
                                            , altitude = Length.meters altitude
                                            , timestamp = Nothing
                                            }
                                  , tags = rawNode.tags
                                  }
                                )

                            Nothing ->
                                ( rawNode.id
                                , { at =
                                        DomainModel.pointFromGpxWithReference
                                            track.referenceLonLat
                                            { longitude = rawNode.lon |> Angle.degrees |> Direction2d.fromAngle
                                            , latitude = rawNode.lat |> Angle.degrees
                                            , altitude = groundLevel
                                            , timestamp = Nothing
                                            }
                                  , tags = rawNode.tags
                                  }
                                )
                    )
                    rawNodes
                    altitudes

        rawWays : List OSMLandUseWay
        rawWays =
            List.filterMap
                (\element ->
                    case element of
                        OSMWay way ->
                            Just way

                        _ ->
                            Nothing
                )
                justRaw.rawData.elements

        ways =
            List.map convertWay rawWays

        convertWay : OSMLandUseWay -> LandUseWay
        convertWay rawWay =
            { nodes =
                rawWay.nodes
                    |> List.map (\node -> Dict.get node nodeDict)
                    |> List.filterMap identity
            , tags = rawWay.tags
            }

        nodes =
            Dict.values nodeDict

        places =
            Dict.union
                (List.foldl addNamedNode Dict.empty nodes)
                (List.foldl addNamedWay Dict.empty ways)

        addNamedNode node names =
            case node.tags of
                Just tags ->
                    case Dict.get "name" tags of
                        Just hasName ->
                            Dict.insert hasName node.at names

                        Nothing ->
                            names

                Nothing ->
                    names

        addNamedWay way names =
            case way.tags of
                Just tags ->
                    case Dict.get "name" tags of
                        Just hasName ->
                            let
                                centroid =
                                    DomainModel.withoutTime <|
                                        Maybe.withDefault Point3d.origin <|
                                            Point3d.centroidN <|
                                                List.map (.at >> .space) way.nodes
                            in
                            Dict.insert hasName centroid names

                        Nothing ->
                            names

                Nothing ->
                    names
    in
    { nodes = nodes
    , ways = ways
    , places = places
    , rawData = { elements = [] } -- discard raw data
    , status = LandUseOK
    }


landUseDecoder : D.Decoder OSMLandUseData
landUseDecoder =
    D.map OSMLandUseData
        (D.at [ "elements" ] (D.list landUseElementDecoder))


landUseElementDecoder : D.Decoder OSMLandUseElement
landUseElementDecoder =
    D.oneOf
        [ D.map OSMNode landUseNodeDecoder
        , D.map OSMWay landUseWayDecoder
        ]


landUseNodeDecoder : D.Decoder OSMLandUseNode
landUseNodeDecoder =
    D.map5 OSMLandUseNode
        (D.field "type" D.string)
        (D.field "id" D.int)
        (D.field "lat" D.float)
        (D.field "lon" D.float)
        (D.maybe (D.field "tags" landUseTagsDecoder))


landUseWayDecoder : D.Decoder OSMLandUseWay
landUseWayDecoder =
    D.map4 OSMLandUseWay
        (D.field "type" D.string)
        (D.field "id" D.int)
        (D.field "nodes" <| D.list D.int)
        (D.maybe (D.field "tags" landUseTagsDecoder))


landUseTagsDecoder : D.Decoder (Dict String String)
landUseTagsDecoder =
    D.dict D.string


queryFromBoundingBox : TrackLoaded msg -> String
queryFromBoundingBox track =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema (DomainModel.boundingBox track.trackTree)

        ( sw, ne ) =
            ( DomainModel.withoutTime <| Point3d.xyz minX minY minZ
            , DomainModel.withoutTime <| Point3d.xyz maxX maxY maxZ
            )

        ( minLon, maxLon ) =
            ( Direction2d.toAngle <|
                .longitude <|
                    DomainModel.gpxFromPointWithReference track.referenceLonLat sw
            , Direction2d.toAngle <|
                .longitude <|
                    DomainModel.gpxFromPointWithReference track.referenceLonLat ne
            )

        ( minLat, maxLat ) =
            ( .latitude <| DomainModel.gpxFromPointWithReference track.referenceLonLat sw
            , .latitude <| DomainModel.gpxFromPointWithReference track.referenceLonLat ne
            )
    in
    String.Interpolate.interpolate
        """[out:json][timeout:30];
(
node["natural"]({0},{1},{2},{3});
way["natural"]({0},{1},{2},{3}) (if:length()>500);
node["landuse"]({0},{1},{2},{3});
way["landuse"]({0},{1},{2},{3}) (if:length()>500);
);
out body;
>;
out skel qt;"""
        [ String.fromFloat <| Angle.inDegrees minLat
        , String.fromFloat <| Angle.inDegrees minLon
        , String.fromFloat <| Angle.inDegrees maxLat
        , String.fromFloat <| Angle.inDegrees maxLon
        ]
