module Tools.I18N exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import FlatColors.ChinesePalette
import FormatNumber.Locales exposing (frenchLocale)
import List.Extra
import Locations.UK
import Tools.I18NOptions exposing (Options)


defaultLocation : Options
defaultLocation =
    Locations.UK.options


availableI18N : List Options
availableI18N =
    [ Locations.UK.options
    , frOptions
    ]


frOptions =
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


localisedString : Options -> String -> String -> String
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


text : Options -> String -> String -> Element msg
text location tool tag =
    Element.text <| localisedString location tool tag


editor : Options -> Element msg
editor options =
    Element.el [ alignBottom, alignLeft, moveUp 50, moveRight 50 ] <|
        column
            [ Background.color FlatColors.ChinesePalette.antiFlashWhite
            , padding 10
            , centerY
            , centerX
            , width <| Element.px 400
            , Border.color FlatColors.ChinesePalette.saturatedSky
            , Border.width 4
            , Border.rounded 10
            ]
            [ Element.text options.country.name
            ]
