module Tools.I18NOptions exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import FormatNumber.Locales exposing (Locale)


type alias Options =
    { country : Country
    , locale : Locale
    , textDictionary : Dict String (Dict String String) -- Building on ToolsController namespaces.
    }
