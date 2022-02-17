module ToolTip exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import ViewPureStyles exposing (neatToolsBorder)


example : Html msg
example =
    layout [ width fill, height fill ] <|
        column
            [ centerX, centerY ]
            [ el [ tooltip above (myTooltip "foo") ] (text "foo")
            , el [ tooltip below (myTooltip "bar") ] (text "bar")
            ]


myTooltip : String -> Element msg
myTooltip str =
    el
        [ Background.color (rgb 0 0 0)
        , Font.color (rgb 1 1 1)
        , padding 4
        , Border.rounded 5
        , Font.size 14
        , Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = rgba 0 0 0 0.32 }
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
                el [ width fill
                , htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltip_
            ]
            none


buttonStylesWithTooltip usher tooltip_ =
    tooltip usher (myTooltip tooltip_)
        :: neatToolsBorder
