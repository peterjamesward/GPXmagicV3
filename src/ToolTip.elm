module ToolTip exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import ViewPureStyles exposing (neatToolsBorder, rgtPurple)


example : Html msg
example =
    layout [ width fill, height fill ] <|
        column
            [ centerX, centerY ]
            [ el [ tooltip above (myTooltip "foo") ] (text "foo")
            , el [ tooltip below (myTooltip "bar") ] (text "bar")
            ]


localisedTooltip : I18NOptions.Options -> String -> String -> Element msg
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
