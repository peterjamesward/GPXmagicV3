module ColourPalette exposing (gradientColourPastel, scrollbarBackground, stravaOrange, warningColor)

import Color
import Element exposing (rgb255)
import FlatColors.FlatUIPalette exposing (..)


gradientColourPastel : Float -> Color.Color
gradientColourPastel slope =
    Color.hsl (gradientHue slope) 0.8 0.6


gradientColourVivid : Float -> Color.Color
gradientColourVivid slope =
    Color.hsl (gradientHue slope) 1.0 0.4


gradientHueOld : Float -> Float
gradientHueOld slope =
    let
        x =
            (clamp -20.0 20.0 slope + 20.0) / 40.0

        steepestAscentHue =
            (Color.toHsla <| Color.rgb255 255 30 30).hue

        steepestDescentHue =
            (Color.toHsla <| Color.rgb255 143 87 255).hue
    in
    x * steepestAscentHue + (1.0 - x) * steepestDescentHue


gradientHue : Float -> Float
gradientHue slope =
    let
        hueOf col =
            let
                { hue, saturation, lightness, alpha } =
                    Color.toHsla col
            in
            hue
    in
    -- Closer to "standard" colouring.
    if slope < 0 then
        gradientHue slope

    else if slope <= 6.0 then
        interpolate (slope / 6.0) (hueOf Color.lightGreen) (hueOf Color.yellow)

    else if slope <= 9.0 then
        interpolate ((slope - 6.0) / 3.0) (hueOf Color.yellow) (hueOf Color.orange)

    else if slope <= 12.0 then
        interpolate ((slope - 9.0) / 3.0) (hueOf Color.orange) (hueOf Color.red)

    else
        interpolate ((clamp 12 30 slope - 12.0) / 18.0) (hueOf Color.red) (hueOf Color.black)


interpolate x a b =
    x * a + (1.0 - x) * b


stravaOrange =
    rgb255 0xFC 0x4C 0x02


warningColor =
    sunFlower


scrollbarBackground =
    asbestos
