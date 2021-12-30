port module PortController exposing (..)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
import DomainModel exposing (PeteTree, convertLocalWithReference, mapStartAt)
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E
import Length
import LocalCoords exposing (LocalCoords)
import MapboxKey exposing (mapboxKey)
import Point3d
import SceneBuilder
import ViewingContext exposing (ViewingContext)


type MapStyle
    = MapStyleStreets
    | MapStyleOutdoors
    | MapStyleSatellite


type alias MapInfo =
    -- Mainly used to set the map up.
    { mapZoom : Float -- track values from user map interactions.
    , centreLon : Float
    , centreLat : Float
    }


port commandPort : E.Value -> Cmd msg


port messageReceiver : (E.Value -> msg) -> Sub msg


createMap : MapInfo -> Cmd msg
createMap info =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            ]


refreshMap : Cmd msg
refreshMap =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "Repaint" )
            , ( "token", E.string mapboxKey )
            ]


centreMap :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
    }
    -> Cmd msg
centreMap model =
    -- Centre map
    case model.trackTree of
        Just tree ->
            let
                { longitude, latitude, altitude } =
                    DomainModel.boundingBox tree
                        |> BoundingBox3d.centerPoint
                        |> convertLocalWithReference (mapStartAt tree)
            in
            commandPort <|
                E.object
                    [ ( "Cmd", E.string "Centre" )
                    , ( "token", E.string mapboxKey )
                    , ( "lon", E.float <| Angle.inDegrees longitude )
                    , ( "lat", E.float <| Angle.inDegrees latitude )
                    ]

        Nothing ->
            Cmd.none



--zoomMap : ViewingContext -> Cmd msg
--zoomMap context =
--    commandPort <|
--        E.object
--            [ ( "Cmd", E.string "Zoom" )
--            , ( "token", E.string mapboxKey )
--            , ( "zoom", E.float context.zoomLevel )
--            ]
--centreMapOnCurrent : Track -> Cmd msg
--centreMapOnCurrent track =
--    let
--        ( lon, lat, _ ) =
--            track.currentNode.xyz
--                |> withoutGhanianTransform track
--    in
--    commandPort <|
--        E.object
--            [ ( "Cmd", E.string "Centre" )
--            , ( "token", E.string mapboxKey )
--            , ( "lon", E.float lon )
--            , ( "lat", E.float lat )
--            ]
--toggleDragging : Bool -> Track -> Cmd msg
--toggleDragging isDragging track =
--    commandPort <|
--        E.object
--            [ ( "Cmd", E.string "Drag" )
--            , ( "Enable", E.bool isDragging )
--            , ( "points", trackPointsToJSON track ) -- Make track points draggable
--            ]
--requestElevations : Cmd msg
--requestElevations =
--    commandPort <|
--        E.object
--            [ ( "Cmd", E.string "Elev" )
--            ]


addTrackToMap :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
    }
    -> Cmd msg
addTrackToMap model =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable features.
    case model.trackTree of
        Just tree ->
            let
                { longitude, latitude, altitude } =
                    DomainModel.mapStartAt tree
            in
            commandPort <|
                E.object
                    [ ( "Cmd", E.string "Track" )
                    , ( "token", E.string mapboxKey )
                    , ( "lon", E.float <| Angle.inDegrees longitude )
                    , ( "lat", E.float <| Angle.inDegrees latitude )
                    , ( "zoom", E.float 10.0 )
                    , ( "data", SceneBuilder.renderMapJson model ) -- Route as polyline
                    , ( "points", E.null ) --trackPointsToJSON track ) -- Make track points draggable
                    ]

        Nothing ->
            Cmd.none



--addMarkersToMap :
--    Track
--    -> List E.Value
--    -> Cmd msg
--addMarkersToMap track previews =
--    let
--        realWorldPosition tp =
--            Track.withoutGhanianTransform track tp.xyz
--
--        encodePos ( lon, lat, ele ) =
--            E.object
--                [ ( "lon", E.float lon )
--                , ( "lat", E.float lat )
--                ]
--    in
--    commandPort <|
--        E.object
--            [ ( "Cmd", E.string "Mark" )
--            , ( "orange", encodePos <| realWorldPosition track.currentNode )
--            , case track.markedNode of
--                Just mark ->
--                    ( "purple", encodePos <| realWorldPosition mark )
--
--                Nothing ->
--                    ( "ignore", E.null )
--            , ( "previews", E.list identity previews )
--            ]


storageSetItem : String -> E.Value -> Cmd msg
storageSetItem key value =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.set" )
            , ( "key", E.string key )
            , ( "value", value )
            ]


storageGetItem : String -> Cmd msg
storageGetItem key =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.get" )
            , ( "key", E.string key )
            ]


storageListKeys : Cmd msg
storageListKeys =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.list" )
            ]


storageClear : Cmd msg
storageClear =
    commandPort <|
        E.object
            [ ( "Cmd", E.string "storage.clear" )
            ]


msgDecoder : Decoder String
msgDecoder =
    field "msg" string
