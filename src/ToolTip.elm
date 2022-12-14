module ToolTip exposing (buttonStylesWithTooltip, localisedTooltip, myTooltip, tooltip)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import ViewPureStyles exposing (neatToolsBorder, rgtPurple)


localisedTooltip : I18NOptions.Location -> String -> String -> Element msg
localisedTooltip location tool tag =
    myTooltip <| I18N.localisedString location tool tag


myTooltip : String -> Element msg
myTooltip str =
    el
        [ Background.color rgtPurple
        , Font.color (rgb 1 1 1)
        , padding 4
        , Border.rounded 5
        , Font.size 14
        , width <| px 100
        ]
        (paragraph [] [ text str ])


tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip usher tooltip_ =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << Element.map never) <|
                el
                    [ width fill
                    , htmlAttribute (Html.Attributes.style "pointerEvents" "none")
                    ]
                    tooltip_
            ]
            none


buttonStylesWithTooltip usher tooltip_ =
    tooltip usher (myTooltip tooltip_)
        :: neatToolsBorder
