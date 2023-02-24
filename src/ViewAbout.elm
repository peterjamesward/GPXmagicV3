module ViewAbout exposing (view)

import About
import CommonToolStyles
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FlatColors.FlatUIPalette
import Markdown
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import SystemSettings exposing (SystemSettings)


view :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> SystemSettings
    -> Element msg
view ( contentWidth, contentHeight ) settings =
    el
        [ width (px <| Pixels.inPixels contentWidth)
        , height (px <| Pixels.inPixels contentHeight)
        , pointer
        , padding 20
        , Border.width 4
        , Border.color FlatColors.FlatUIPalette.concrete
        , scrollbarY
        , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
        ]
    <|
        paragraph
            [ width fill ]
        <|
            [ html <| Markdown.toHtml [] About.aboutText ]
