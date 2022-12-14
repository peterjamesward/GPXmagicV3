port module LocalStorage exposing (processStoragePortMessage, storageGetItem, storageGetMemoryUsage, storageResponses, storageSetItem)

import Actions exposing (ToolAction(..))
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Tools.MemoryUsage


port storageCommands : E.Value -> Cmd msg


port storageResponses : (E.Value -> msg) -> Sub msg


storageGetMemoryUsage : Cmd msg
storageGetMemoryUsage =
    -- I know it's not storage but it doesn't merit another port pair.
    storageCommands <|
        E.object
            [ ( "Cmd", E.string "memory" ) ]


storageSetItem : String -> E.Value -> Cmd msg
storageSetItem key value =
    storageCommands <|
        E.object
            [ ( "Cmd", E.string "storage.set" )
            , ( "key", E.string key )
            , ( "value", value )
            ]


storageGetItem : String -> Cmd msg
storageGetItem key =
    storageCommands <|
        E.object
            [ ( "Cmd", E.string "storage.get" )
            , ( "key", E.string key )
            ]


msgDecoder : Decoder String
msgDecoder =
    field "msg" string


memoryDecoder : Decoder Tools.MemoryUsage.HeapStatus
memoryDecoder =
    D.map3 Tools.MemoryUsage.HeapStatus
        (D.field "jsHeapSizeLimit" D.int)
        (D.field "totalJSHeapSize" D.int)
        (D.field "usedJSHeapSize" D.int)


processStoragePortMessage :
    E.Value
    -> model
    -> List (ToolAction msg)
processStoragePortMessage json model =
    let
        jsonMsg =
            D.decodeValue msgDecoder json
    in
    case jsonMsg of
        Ok "storage.got" ->
            let
                key =
                    D.decodeValue (D.field "key" D.string) json

                value =
                    D.decodeValue (D.field "value" D.value) json
            in
            case ( key, value ) of
                ( Ok someKey, Ok somevalue ) ->
                    [ StoredValueRetrieved someKey somevalue ]

                _ ->
                    []

        Ok "memory" ->
            let
                memory =
                    D.decodeValue (D.field "memory" memoryDecoder) json
            in
            case memory of
                Ok gotMemory ->
                    [ HeapStatusUpdate gotMemory ]

                _ ->
                    []

        _ ->
            []
