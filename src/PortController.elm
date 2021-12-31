port module PortController exposing (..)

import Angle
import Axis3d
import BoundingBox3d exposing (BoundingBox3d)
import Delay exposing (after)
import Direction3d
import DomainModel exposing (PeteTree, gpxFromVector, leafFromIndex, startVector)
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Length
import LocalCoords exposing (LocalCoords)
import MapboxKey exposing (mapboxKey)
import Msg exposing (Msg(..))
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
                    leafFromIndex model.currentPosition tree
                        |> startVector
                        |> gpxFromVector
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


centreMapOnCurrent :
    { m
        | trackTree : Maybe PeteTree
        , renderDepth : Int
        , currentPosition : Int
    }
    -> Cmd msg
centreMapOnCurrent model =
    case model.trackTree of
        Just tree ->
            let
                { longitude, latitude, altitude } =
                    leafFromIndex model.currentPosition tree
                        |> startVector
                        |> gpxFromVector
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
                    leafFromIndex model.currentPosition tree
                        |> startVector
                        |> gpxFromVector
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


processPortMessage :
    { m
        | trackTree : Maybe PeteTree
        , lastMapClick : ( Float, Float )
        , mapClickDebounce : Bool
        , currentPosition : Int
        , renderDepth : Int
    }
    -> E.Value
    ->
        ( { m
            | trackTree : Maybe PeteTree
            , lastMapClick : ( Float, Float )
            , mapClickDebounce : Bool
            , currentPosition : Int
            , renderDepth : Int
          }
        , Cmd Msg
        )
processPortMessage model json =
    -- So we don't need to keep going to the PortController.
    -- These will be Model-domain messages.
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
                --( False, Ok lat1, Ok lon1 ) ->
                --    let
                --        gpxPoint =
                --            { longitude = Angle.degrees lon1
                --            , latitude = Angle.degrees lat1
                --            , altitude = Length.meters 0.0
                --            }
                --
                --        updatedModel =
                --            { model
                --                | lastMapClick = ( lon1, lat1 )
                --                , mapClickDebounce = True
                --                , currentPosition = index
                --            }
                --    in
                --    ( updatedModel
                --    , Cmd.batch
                --        [ -- Selective rendering requires we remove and add again.
                --          addTrackToMap updatedModel
                --        , after 100 ClearMapClickDebounce
                --        , after 100 RepaintMap
                --        ]
                --    )

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
        --( Ok "storage.got", _ ) ->
        --    let
        --        key =
        --            D.decodeValue (D.field "key" D.string) json
        --
        --        value =
        --            D.decodeValue (D.field "value" D.value) json
        --    in
        --    case ( key, value ) of
        --        ( Ok "accordion", Ok saved ) ->
        --            let
        --                ( restoreAccordionState, restoreAccordion ) =
        --                    Accordion.recoverStoredState
        --                        saved
        --                        model.toolsAccordion
        --            in
        --            ( Model
        --                { model
        --                    | accordionState = restoreAccordionState
        --                    , toolsAccordion = restoreAccordion
        --                }
        --            , Cmd.none
        --            )
        --
        --        ( Ok "splitter", Ok splitter ) ->
        --            let
        --                p =
        --                    D.decodeValue D.int splitter
        --            in
        --            case p of
        --                Ok pixels ->
        --                    ( Model
        --                        { model
        --                            | splitInPixels = pixels
        --                            , viewPanes = ViewPane.mapOverPanes (setViewPaneSize pixels) model.viewPanes
        --                        }
        --                    , Cmd.none
        --                    )
        --
        --                _ ->
        --                    ( Model model, Cmd.none )
        --
        --        ( Ok "panes", Ok saved ) ->
        --            let
        --                newPanes =
        --                    ViewPane.restorePaneState saved model.viewPanes
        --
        --                newModel =
        --                    { model
        --                        | viewPanes =
        --                            ViewPane.mapOverPanes
        --                                (setViewPaneSize model.splitInPixels)
        --                                newPanes
        --                    }
        --            in
        --            processPostUpdateAction newModel ActionRerender
        --
        --        ( Ok "display", Ok saved ) ->
        --            ( Model { model | displayOptions = DisplayOptions.decodeOptions saved }
        --            , Cmd.none
        --            )
        --
        --        _ ->
        --            ( Model model, Cmd.none )
        --
        --( Ok "storage.keys", _ ) ->
        --    ( Model model
        --    , Cmd.none
        --    )
        _ ->
            ( model, Cmd.none )
