module Tools.I18N exposing (Msg(..), availableI18N, defaultLocation, defaultOptions, editor, fromCountryCode, localisedString, requestDictionary, text, update)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FlatColors.ChinesePalette
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Locations.UK
import Task
import Tools.I18NOptions exposing (Location, Options, TwoLevelDict)


type Msg
    = ContentChange String
    | ChooseOuter String
    | ChooseInner String
    | Update
    | Download
    | Upload
    | FileChosen File
    | FileLoaded String
    | Dictionary (Result Http.Error TwoLevelDict)


defaultLocation : Location
defaultLocation =
    Locations.UK.location


defaultOptions : Options
defaultOptions =
    { editorOuter = Nothing
    , editorInner = Nothing
    , editorValue = Nothing
    }


availableI18N : List Location
availableI18N =
    [ Locations.UK.location

    --, frLocation
    ]


fromCountryCode code =
    case List.Extra.find (\loc -> loc.country.code == code) availableI18N of
        Just location ->
            location

        Nothing ->
            defaultLocation


localisedString : Location -> String -> String -> String
localisedString location tool tag =
    let
        fromActiveLocation =
            case Dict.get tool location.textDictionary of
                Just innerDict ->
                    Dict.get tag innerDict

                Nothing ->
                    Nothing

        fromDefault =
            case Dict.get tool defaultLocation.textDictionary of
                Just innerDict ->
                    Dict.get tag innerDict

                Nothing ->
                    Nothing
    in
    case fromActiveLocation of
        Just gotText ->
            gotText

        Nothing ->
            case fromDefault of
                Just gotText ->
                    gotText

                Nothing ->
                    tool ++ ":" ++ tag ++ "?"


text : Location -> String -> String -> Element msg
text location tool tag =
    Element.text <| localisedString location tool tag


editor : (Msg -> msg) -> Location -> Options -> Element msg
editor wrapper location options =
    let
        outerDictionaryList =
            Input.radio
                [ padding 4
                , spacing 3
                ]
                { onChange = wrapper << ChooseOuter
                , selected = options.editorOuter
                , label = Input.labelHidden "outer"
                , options = List.map option (Dict.keys location.textDictionary)
                }

        innerDictionaryList inner =
            Input.radio
                [ padding 4
                , spacing 3
                ]
                { onChange = wrapper << ChooseInner
                , selected = options.editorInner
                , label = Input.labelHidden "inner"
                , options = List.map option (Dict.keys inner)
                }

        option key =
            Input.optionWith key (button key)

        button key state =
            Element.el
                [ if state == Input.Selected then
                    Font.bold

                  else
                    Font.italic
                , alignTop
                ]
            <|
                Element.text key

        valueEditor =
            column [ width <| px 800 ]
                [ case options.editorValue of
                    Just value ->
                        Input.multiline
                            [ Border.rounded 6
                            , Border.width 2
                            , Border.color <| rgb255 0x72 0x9F 0xCF
                            , width fill
                            ]
                            { onChange = wrapper << ContentChange
                            , text = value
                            , placeholder = Just <| Input.placeholder [] <| Element.text "Appears to be empty"
                            , label = Input.labelHidden "text"
                            , spellcheck = False
                            }

                    Nothing ->
                        none
                , row [ padding 40, spacing 20 ]
                    [ updateButton, saveButton, loadButton ]
                ]

        updateButton =
            Input.button
                [ Border.width 3
                , Border.rounded 6
                , Border.color <| rgb255 0x72 0x9F 0xCF
                ]
                { onPress = Just <| wrapper Update
                , label = Element.text "UPDATE"
                }

        saveButton =
            Input.button
                [ Border.width 3
                , Border.rounded 6
                , Border.color <| rgb255 0x72 0x9F 0xCF
                ]
                { onPress = Just <| wrapper Download
                , label = Element.text "SAVE TO DOWNLOADS"
                }

        loadButton =
            Input.button
                [ Border.width 3
                , Border.rounded 6
                , Border.color <| rgb255 0x72 0x9F 0xCF
                ]
                { onPress = Just <| wrapper Upload
                , label = Element.text "LOAD LANGUAGE FILE"
                }
    in
    column
        [ Background.color FlatColors.ChinesePalette.antiFlashWhite
        , padding 10
        , spacing 10
        , Border.color FlatColors.ChinesePalette.saturatedSky
        , Border.width 4
        , Border.rounded 10
        , alignBottom
        , alignLeft
        , moveUp 50
        , moveRight 50
        ]
        [ el [ Font.bold ] <| Element.text location.country.name
        , row [ spacing 10, alignTop ]
            [ outerDictionaryList
            , case options.editorOuter of
                Nothing ->
                    none

                Just key ->
                    case Dict.get key location.textDictionary of
                        Just innerDict ->
                            row [ alignTop, spacing 10 ]
                                [ innerDictionaryList innerDict
                                , valueEditor
                                ]

                        Nothing ->
                            none
            ]
        ]


update : Msg -> (Msg -> msg) -> ( Location, Options ) -> ( Location, Options, Cmd msg )
update msg wrapper ( location, options ) =
    case msg of
        ChooseOuter key ->
            ( location
            , { options | editorOuter = Just key }
            , Cmd.none
            )

        ChooseInner key ->
            ( location
            , { options
                | editorInner = Just key
                , editorValue =
                    case options.editorOuter of
                        Just outer ->
                            case Dict.get outer location.textDictionary of
                                Just innerDict ->
                                    Dict.get key innerDict

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        ContentChange content ->
            ( location
            , { options | editorValue = Just content }
            , Cmd.none
            )

        Update ->
            let
                innerDict =
                    case options.editorOuter of
                        Just outer ->
                            Dict.get outer location.textDictionary

                        Nothing ->
                            Nothing

                newInnerDict =
                    case ( options.editorInner, innerDict, options.editorValue ) of
                        ( Just innerKey, Just isInnerDict, Just value ) ->
                            Just <| Dict.insert innerKey value isInnerDict

                        _ ->
                            Nothing

                newOuterDict =
                    case ( options.editorOuter, newInnerDict ) of
                        ( Just outerKey, Just newInner ) ->
                            Dict.insert outerKey newInner location.textDictionary

                        _ ->
                            location.textDictionary
            in
            ( { location | textDictionary = newOuterDict }
            , options
            , Cmd.none
            )

        Download ->
            ( location
            , options
            , Download.string "LOCATION.JSON" "text/json" <|
                E.encode 4 <|
                    locationToJson location
            )

        Upload ->
            ( location
            , options
            , Select.file [ "text/json" ] (wrapper << FileChosen)
            )

        FileChosen file ->
            ( location
            , options
            , Task.perform (wrapper << FileLoaded) (File.toString file)
            )

        FileLoaded content ->
            case D.decodeString locationDecoder content of
                Ok newDictionary ->
                    ( { location | textDictionary = newDictionary }
                    , options
                    , Cmd.none
                    )

                Err _ ->
                    ( location, options, Cmd.none )

        Dictionary result ->
            case result of
                Ok remoteDict ->
                    ( { location | textDictionary = remoteDict }, options, Cmd.none )

                Err _ ->
                    ( location, options, Cmd.none )


locationToJson : Location -> E.Value
locationToJson location =
    E.dict identity (E.dict identity E.string) location.textDictionary


locationDecoder : D.Decoder TwoLevelDict
locationDecoder =
    D.dict (D.dict D.string)


requestDictionary : (Msg -> msg) -> String -> Cmd msg
requestDictionary wrapper countryCode =
    Http.get
        { url = "languages/" ++ countryCode ++ ".JSON"
        , expect = Http.expectJson (wrapper << Dictionary) locationDecoder
        }
