module Tools.I18N exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FlatColors.ChinesePalette
import FormatNumber.Locales exposing (frenchLocale)
import List.Extra
import Locations.UK
import Tools.I18NOptions exposing (Location, Options)


type Msg
    = ContentChange String


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
            column [ spacing 2 ] <| List.map Element.text (Dict.keys location.textDictionary)
    in
    Element.el [ alignBottom, alignLeft, moveUp 50, moveRight 50 ] <|
        column
            [ Background.color FlatColors.ChinesePalette.antiFlashWhite
            , padding 10
            , spacing 10
            , centerY
            , centerX
            , Border.color FlatColors.ChinesePalette.saturatedSky
            , Border.width 4
            , Border.rounded 10
            ]
            [ Element.text location.country.name
            , row [ spacing 10 ]
                [ outerDictionaryList
                , Input.text
                    [ padding 5

                    --, onEnter WriteGpxFile
                    , width <| minimum 200 <| fill
                    ]
                    { text = "test"
                    , onChange = wrapper << ContentChange
                    , placeholder = Nothing
                    , label = Input.labelHidden "text"
                    }
                ]
            ]


update : Msg -> ( Location, Options ) -> ( Location, Options )
update msg ( location, options ) =
    case msg of
        ContentChange string ->
            ( location, options )
