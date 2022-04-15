module Tools.I18N exposing (..)

import Color
import Countries exposing (Country)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.ChinesePalette
import FormatNumber.Locales exposing (frenchLocale)
import List.Extra
import Locations.UK
import Tools.I18NOptions exposing (Location, Options)


type Msg
    = ContentChange String
    | ChooseOuter String
    | ChooseInner String


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
    , frLocation
    ]


frLocation =
    { country = Country "France" "FR" "🇫🇷"
    , locale = frenchLocale
    , textDictionary = Dict.empty
    }


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
            Element.text "UPDATE"

        saveButton =
            Element.text "SAVE TO DOWNLOADS"

        loadButton =
            Element.text "LOAD LANGUAGE FILE"
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


update : Msg -> ( Location, Options ) -> ( Location, Options )
update msg ( location, options ) =
    case msg of
        ChooseOuter key ->
            ( location
            , { options | editorOuter = Just key }
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
            )

        ContentChange content ->
            ( location, { options | editorValue = Just content } )
