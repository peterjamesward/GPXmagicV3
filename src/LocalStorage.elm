port module LocalStorage exposing (..)

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


storageListKeys : Cmd msg
storageListKeys =
    storageCommands <|
        E.object
            [ ( "Cmd", E.string "storage.list" )
            ]


storageClear : Cmd msg
storageClear =
    storageCommands <|
        E.object
            [ ( "Cmd", E.string "storage.clear" )
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
            case ( memory ) of
                ( Ok gotMemory ) ->
                    [ HeapStatusUpdate gotMemory ]

                _ ->
                    []

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
        --    (  model
        --    , Cmd.none
        --    )
        _ ->
            []
