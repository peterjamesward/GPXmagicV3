module Tools.I18NOptions exposing (Location, Options, TwoLevelDict)

import Countries exposing (Country)
import Dict exposing (Dict)
import FormatNumber.Locales exposing (Locale)


type alias TwoLevelDict =
    Dict String (Dict String String)


type alias Location =
    { country : Country
    , locale : Locale
    , textDictionary : TwoLevelDict -- Building on ToolsController namespaces.
    }


type alias Options =
    { editorOuter : Maybe String
    , editorInner : Maybe String
    , editorValue : Maybe String
    }
