port module MapPortsController exposing (..)

import Angle
import Delay exposing (after)
import Direction2d
import DomainModel exposing (GPXSource, PeteTree, gpxFromPointWithReference, gpxPointFromIndex, leafFromIndex, pointFromIndex, sourceData, startPoint)
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Length
import MapboxKey exposing (mapboxKey)
import ModelRecord exposing (Model(..))
import SceneBuilder


type MapMsg
    = MapPortMessage E.Value
    | RepaintMap
    | ClearMapClickDebounce


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


centreMapOnCurrent :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
        , referenceLonLat : GPXSource
    }
    -> Cmd msg
centreMapOnCurrent model =
    case model.trackTree of
        Just tree ->
            let
                { longitude, latitude, altitude } =
                    gpxPointFromIndex model.currentPosition tree
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


deferredMapRepaint msgWrapper =
    after 50 (RepaintMap |> msgWrapper)


update :
    MapMsg
    ->
        { model
            | trackTree : Maybe PeteTree
            , lastMapClick : ( Float, Float )
            , mapClickDebounce : Bool
            , currentPosition : Int
            , renderDepth : Int
            , referenceLonLat : GPXSource
        }
    -> (MapMsg -> msg)
    ->
        ( { model
            | trackTree : Maybe PeteTree
            , lastMapClick : ( Float, Float )
            , mapClickDebounce : Bool
            , currentPosition : Int
            , renderDepth : Int
            , referenceLonLat : GPXSource
          }
        , Cmd msg
        )
update mapMsg model msgWrapper =
    case mapMsg of
        ClearMapClickDebounce ->
            ( { model | mapClickDebounce = False }
            , Cmd.none
            )

        MapPortMessage value ->
            processMapPortMessage model value msgWrapper

        RepaintMap ->
            ( model, refreshMap )



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
        , referenceLonLat : GPXSource
    }
    -> Cmd msg
addTrackToMap model =
    -- This is to add the route as a polyline.
    -- We will separately add track points as draggable features.
    case model.trackTree of
        Just tree ->
            let
                { longitude, latitude, altitude } =
                    gpxPointFromIndex model.currentPosition tree
            in
            mapCommands <|
                E.object
                    [ ( "Cmd", E.string "Track" )
                    , ( "token", E.string mapboxKey )
                    , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
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


msgDecoder : Decoder String
msgDecoder =
    field "msg" string


processMapPortMessage :
    { m
        | trackTree : Maybe PeteTree
        , lastMapClick : ( Float, Float )
        , mapClickDebounce : Bool
        , currentPosition : Int
        , renderDepth : Int
        , referenceLonLat : GPXSource
    }
    -> E.Value
    -> (MapMsg -> msg)
    ->
        ( { m
            | trackTree : Maybe PeteTree
            , lastMapClick : ( Float, Float )
            , mapClickDebounce : Bool
            , currentPosition : Int
            , renderDepth : Int
            , referenceLonLat : GPXSource
          }
        , Cmd msg
        )
processMapPortMessage model json msgWrapper =
    let
        jsonMsg =
            D.decodeValue msgDecoder json

        ( lat, lon ) =
            ( D.decodeValue (D.field "lat" D.float) json
            , D.decodeValue (D.field "lon" D.float) json
            )
    in
    case ( jsonMsg, model.trackTree ) of
        ( Ok "click", Just tree ) ->
            --{ 'msg' : 'click'
            --, 'lat' : e.lat()
            --, 'lon' : e.lon()
            --} );
            case ( model.mapClickDebounce, lat, lon ) of
                ( False, Ok lat1, Ok lon1 ) ->
                    let
                        gpxPoint =
                            { longitude = Direction2d.fromAngle <| Angle.degrees lon1
                            , latitude = Angle.degrees lat1
                            , altitude = Length.meters 0.0
                            }

                        index =
                            DomainModel.nearestToLonLat gpxPoint tree

                        updatedModel =
                            { model
                                | lastMapClick = ( lon1, lat1 )
                                , mapClickDebounce = True
                                , currentPosition = index
                            }
                    in
                    ( updatedModel
                    , Cmd.batch
                        [ -- Selective rendering requires we remove and add again.
                          addTrackToMap updatedModel
                        , after 100 (ClearMapClickDebounce |> msgWrapper)
                        , after 100 (RepaintMap |> msgWrapper)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

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
        --
        --( Ok "sketch", _ ) ->
        --    case ( longitudes, latitudes, elevations ) of
        --        ( Ok mapLongitudes, Ok mapLatitudes, Ok mapElevations ) ->
        --            let
        --                newTrack =
        --                    List.map3
        --                        (\x y z -> ( x, y, z ))
        --                        mapLongitudes
        --                        mapLatitudes
        --                        mapElevations
        --                        |> Track.trackFromMap
        --            in
        --            case newTrack of
        --                Just track ->
        --                    applyTrack (Model model) track
        --
        --                Nothing ->
        --                    ( Model model
        --                    , Cmd.none
        --                    )
        --
        --        _ ->
        --            ( Model model, Cmd.none )
        --
        --( Ok "no node", _ ) ->
        --    ( Model model
        --    , Cmd.none
        --    )
        --
        _ ->
            ( model, Cmd.none )
