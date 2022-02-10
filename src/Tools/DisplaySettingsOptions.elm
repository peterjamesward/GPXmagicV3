module Tools.DisplaySettingsOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

type alias Options =
    { roadSurface : Bool
    , curtainStyle : CurtainStyle
    , centreLine : Bool
    , groundPlane : Bool
    }

type CurtainStyle
    = NoCurtain
    | PlainCurtain
    | PastelCurtain
