module ColourPalette exposing (..)

import Color
import Element exposing (rgb255)
import FlatColors.FlatUIPalette exposing (..)


gradientHue : Float -> Float
gradientHue slope =
    let
        x =
            (clamp -20.0 20.0 slope + 20.0) / 40.0

        steepestAscentHue =
            (Color.toHsla Color.red).hue

        steepestDescentHue =
            (Color.toHsla Color.purple).hue
    in
    x * steepestAscentHue + (1.0 - x) * steepestDescentHue


interpolate x a b =
    x * a + (1.0 - x) * b


gradientHue2 : Float -> Float
gradientHue2 slope =
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


stravaOrange =
    rgb255 0xFC 0x4C 0x02


white =
    rgb255 0xFF 0xFF 0xFF


warningColor =
    sunFlower


buttonBackground =
    belizeHole


buttonText =
    white


buttonShadow =
    peterRiver


buttonGroupBackground =
    silver


expandedTabBorder =
    clouds


collapsedTabBorder =
    silver


expandedTabShadow =
    clouds


expandedTabBackground =
    rgb255 0 148 50


pinnedTabBackground =
    sunFlower


accordionContentBackground =
    clouds


collapsedTabBackground =
    rgb255 0 98 102


radioButtonShadow =
    silver


radioButtonSelected =
    nephritis


radioButtonDefault =
    rgb255 0 98 102


radioButtonText =
    white


scrollbarBackground =
    asbestos
