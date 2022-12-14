module ViewAbout exposing (view)

import About
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import FlatColors.FlatUIPalette
import Markdown
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


view :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Element msg
view ( contentWidth, contentHeight ) =
    el
        [ width (px <| Pixels.inPixels contentWidth)
        , height (px <| Pixels.inPixels contentHeight)
        , pointer
        , padding 20
        , Border.width 8
        , Border.color FlatColors.FlatUIPalette.asbestos
        , Background.color FlatColors.FlatUIPalette.clouds
        , scrollbarY
        ]
    <|
        paragraph
            [ width fill ]
        <|
            [ html <| Markdown.toHtml [] About.aboutText ]
