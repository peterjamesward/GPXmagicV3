module ColourPalette exposing (gradientColourPastel, scrollbarBackground, stravaOrange, warningColor)

import Color
import Element exposing (rgb255)
import FlatColors.FlatUIPalette exposing (..)


gradientColourPastel : Float -> Color.Color
gradientColourPastel slope =
    Color.hsl (gradientHue slope) 0.6 0.7


gradientHue : Float -> Float
gradientHue slope =
    let
        x =
            (clamp -20.0 20.0 slope + 20.0) / 40.0

        steepestAscentHue =
            (Color.toHsla <| Color.rgb255 255 30 30).hue

        steepestDescentHue =
            (Color.toHsla <| Color.rgb255 143 87 255).hue
    in
    x * steepestAscentHue + (1.0 - x) * steepestDescentHue


stravaOrange =
    rgb255 0xFC 0x4C 0x02


warningColor =
    sunFlower


scrollbarBackground =
    asbestos
