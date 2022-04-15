module Tools.I18NOptions exposing (..)

import Countries exposing (Country)
import Dict exposing (Dict)
import FormatNumber.Locales exposing (Locale)


type alias Location =
    { country : Country
    , locale : Locale
    , textDictionary : Dict String (Dict String String) -- Building on ToolsController namespaces.
    }


type alias Options =
    { editorOuter : Maybe String
    , editorInner : Maybe String
    , editorValue : Maybe String
    }
