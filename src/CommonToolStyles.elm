module CommonToolStyles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FlatColors.ChinesePalette
import SystemSettings exposing (SystemSettings)


toolContentBoxStyle : SystemSettings -> List (Attribute msg)
toolContentBoxStyle settings =
    case settings.colourTheme of
        SystemSettings.LightTheme ->
            [ padding 5
            , spacing 5
            , width fill --<| px 300
            , centerX
            , Background.color FlatColors.ChinesePalette.antiFlashWhite
            , Font.color FlatColors.ChinesePalette.prestigeBlue
            ]

        SystemSettings.DarkTheme ->
            [ padding 5
            , spacing 5
            , width fill --<| px 300
            , centerX
            , Font.color FlatColors.ChinesePalette.antiFlashWhite
            , Background.color FlatColors.ChinesePalette.prestigeBlue
            ]
