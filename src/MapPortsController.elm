port module MapPortsController exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import Direction2d
import DomainModel exposing (GPXSource, PeteTree, gpxFromPointWithReference, gpxPointFromIndex, leafFromIndex, pointFromIndex, sourceData, startPoint)
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Length
import MapboxKey exposing (mapboxKey)
import SceneBuilder
import TrackLoaded exposing (TrackLoaded)


type MapMsg
    = MapPortMessage E.Value


type alias MapInfo =
    -- Mainly used to set the map up.
    { mapZoom : Float -- track values from user map interactions.
    , centreLon : Float
    , centreLat : Float
    }


port mapCommands : E.Value -> Cmd msg


port mapResponses : (E.Value -> msg) -> Sub msg


createMap : MapInfo -> Cmd msg
createMap info =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            ]


refreshMap : Cmd msg
refreshMap =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Repaint" )
            , ( "token", E.string mapboxKey )
            ]


centreMap :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
        , referenceLonLat : GPXSource
    }
    -> Cmd msg
centreMap model =
    -- Centre map
    case model.trackTree of
        Just tree ->
            let
                { longitude, latitude, altitude } =
                    pointFromIndex model.currentPosition tree
                        |> gpxFromPointWithReference model.referenceLonLat
            in
            mapCommands <|
                E.object
                    [ ( "Cmd", E.string "Centre" )
                    , ( "token", E.string mapboxKey )
                    , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
                    , ( "lat", E.float <| Angle.inDegrees latitude )
                    ]

        Nothing ->
            Cmd.none


centreMapOnCurrent : TrackLoaded -> Cmd msg
centreMapOnCurrent track =
    let
        { longitude, latitude, altitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            ]



{-
   deferredMapRepaint msgWrapper =
       -- This is now in JS, where it quietly just works.
       after 50 (RepaintMap |> msgWrapper)
-}


update :
    MapMsg
    -> TrackLoaded
    -> List (ToolAction msg)
update mapMsg track =
    case mapMsg of
        MapPortMessage value ->
            processMapPortMessage track value



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


addTrackToMap : TrackLoaded -> Cmd msg
addTrackToMap track =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable features.
    let
        { longitude, latitude, altitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            , ( "zoom", E.float 10.0 )
            , ( "data", SceneBuilder.renderMapJson track ) -- Route as polyline
            , ( "points", E.null ) --trackPointsToJSON track ) -- Make track points draggable
            ]



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


msgDecoder : Decoder String
msgDecoder =
    field "msg" string


processMapPortMessage :
    TrackLoaded
    -> E.Value
    -> List (ToolAction msg)
processMapPortMessage track json =
    let
        jsonMsg =
            D.decodeValue msgDecoder json

        ( lat, lon ) =
            ( D.decodeValue (D.field "lat" D.float) json
            , D.decodeValue (D.field "lon" D.float) json
            )
    in
    case jsonMsg of
        Ok "click" ->
            --{ 'msg' : 'click'
            --, 'lat' : e.lat()
            --, 'lon' : e.lon()
            --} );
            case ( lat, lon ) of
                ( Ok lat1, Ok lon1 ) ->
                    let
                        gpxPoint =
                            { longitude = Direction2d.fromAngle <| Angle.degrees lon1
                            , latitude = Angle.degrees lat1
                            , altitude = Length.meters 0.0
                            }

                        index =
                            DomainModel.nearestToLonLat gpxPoint track.trackTree
                    in
                    [SetCurrent index]

                _ ->
                    []

        --( Ok "drag", Just track ) ->
        --    case draggedOnMap json track of
        --        Just undoEntry ->
        --            processPostUpdateAction
        --                model
        --                (PostUpdateActions.ActionTrackChanged TrackEditType.EditPreservesIndex undoEntry)
        --
        --        Nothing ->
        --            ( Model model, Cmd.none )
        --
        --( Ok "elevations", Just track ) ->
        --    case elevations of
        --        Ok mapElevations ->
        --            processPostUpdateAction model
        --                (PostUpdateActions.ActionTrackChanged
        --                    TrackEditType.EditPreservesIndex
        --                    (RotateRoute.buildMapElevations mapElevations track)
        --                )
        --
        --        _ ->
        --            ( Model model, Cmd.none )
        _ ->
            []
