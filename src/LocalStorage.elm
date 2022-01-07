module LocalStorage exposing (..)

import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E


port storageCommands : E.Value -> Cmd msg


port storageResponses : (E.Value -> msg) -> Sub msg


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


processStoragePortMessage :
    model
    -> E.Value
    -> ( model, Cmd msg )
processStoragePortMessage model json =
    let
        jsonMsg =
            D.decodeValue msgDecoder json
    in
    case jsonMsg of
        --( Ok "storage.got" ) ->
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
