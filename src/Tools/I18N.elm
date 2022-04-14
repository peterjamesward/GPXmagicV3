module Tools.I18N exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import Element exposing (Element)
import FormatNumber.Locales exposing (frenchLocale)
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
