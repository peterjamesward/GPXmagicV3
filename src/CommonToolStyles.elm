module CommonToolStyles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import SystemSettings exposing (ColourTheme(..), SystemSettings)
import Tools.I18N as I18N


noTrackMessage : SystemSettings -> Element msg
noTrackMessage settings =
    paragraph
        (toolContentBoxStyle settings)
        [ I18N.text settings.location "tools" "notrack" ]


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


themeBackground : ColourTheme -> Element.Color
themeBackground theme =
    case theme of
        LightTheme ->
            FlatColors.ChinesePalette.antiFlashWhite

        DarkTheme ->
            FlatColors.FlatUIPalette.midnightBlue


themeForeground : ColourTheme -> Element.Color
themeForeground theme =
    case theme of
        LightTheme ->
            FlatColors.FlatUIPalette.midnightBlue

        DarkTheme ->
            FlatColors.ChinesePalette.antiFlashWhite
