module Tools.I18N exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import Element exposing (Element)
import FormatNumber.Locales exposing (frenchLocale)
import Locations.Uk
import Tools.I18NOptions exposing (Options)


defaultOptions : Options
defaultOptions =
    Locations.Uk.ukOptions


availableI18N : List Options
availableI18N =
    [ Locations.Uk.ukOptions
    , frOptions
    ]


frOptions =
    { country = Country "France" "FR" "🇫🇷"
    , locale = frenchLocale
    , textDictionary = Dict.empty
    }


localisedString : Options -> String -> String -> String
localisedString location tool tag =
    case Dict.get tool location.textDictionary of
        Just innerDict ->
            case Dict.get tag innerDict of
                Just gotText ->
                    gotText

                Nothing ->
                    "Tag " ++ tag ++ " not found"

        Nothing ->
            "Tool " ++ tool ++ " not found"


text : Options -> String -> String -> Element msg
text location tool tag =
    Element.text <| localisedString location tool tag
