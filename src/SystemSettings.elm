module SystemSettings exposing (..)

import Tools.I18N as I18NOptions
import Tools.I18NOptions as I18NOptions


type ColourTheme
    = LightTheme
    | DarkTheme


type alias SystemSettings =
    { imperial : Bool
    , colourTheme : ColourTheme
    , location : I18NOptions.Location
    , v2Skin : Bool
    }


default =
    { imperial = False
    , colourTheme = LightTheme
    , location = I18NOptions.defaultLocation
    , v2Skin = True
    }
