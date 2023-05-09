module ViewPureStyles exposing
    ( commonLayoutStyles
    , commonShortHorizontalSliderStyles
    , commonShortVerticalSliderStyles
    , compactRadioButton
    , conditionallyVisible
    , contrastingColour
    , displayName
    , edges
    , infoButton
    , neatToolsBorder
    , onEnter
    , prettyButtonStyles
    , radioButton
    , rgtDark
    , rgtPurple
    , shortSliderStyles
    , showModalMessage
    , sliderThumb
    , stopProp
    , subtleToolStyles
    , ukraineBlue
    , ukraineYellow
    , useIcon
    , useIconWithSize
    , v2ButtonStyles
    , wideSliderStylesWithWidth
    )

import Color exposing (blue)
import ColourPalette exposing (scrollbarBackground)
import CommonToolStyles
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Thumb, thumb)
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import FlatColors.FlatUIPalette
import Html.Attributes exposing (style)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Markdown
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions


rgtPurple =
    Element.rgb255 63 0 73


rgtDark =
    Element.rgb255 12 0 22


ukraineBlue =
    rgb255 0 87 183


ukraineYellow =
    rgb255 255 221 0


edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


commonLayoutStyles =
    [ padding 2
    , spacing 5
    , width fill
    , height fill
    , Font.size 14
    , Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]
    ]


v2ButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 10
    , Border.color FlatColors.FlatUIPalette.belizeHole
    , Background.color FlatColors.FlatUIPalette.belizeHole
    , Font.color white
    , Font.size 14
    ]


prettyButtonStyles =
    [ Border.width 2
    , Border.rounded 4
    , padding 2
    , Border.color FlatColors.ChinesePalette.bayWharf
    , Background.color FlatColors.ChinesePalette.frenchSkyBlue
    , Font.color FlatColors.ChinesePalette.antiFlashWhite
    , Font.size 16

    --, mouseOver
    --    [ Background.color buttonText, Font.color buttonBackground ]
    --, focused
    --    [ Border.shadow { offset = ( 4, 0 ), size = 3, blur = 5, color = buttonShadow } ]
    ]


conditionallyVisible : Bool -> Element msg -> Element msg
conditionallyVisible test element =
    -- This turns out to be the secret sauce for easier map integration.
    -- It means we can pre-load a Mapbox map element.
    if test then
        el [] element

    else
        el [ htmlAttribute (style "display" "none") ] element


commonShortHorizontalSliderStyles =
    [ height <| px 20
    , width <| px 150
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 150
            , height <| px 2
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


commonShortVerticalSliderStyles =
    [ height <| px 150
    , width <| px 20
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 2
            , height <| px 150
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


contrastingColour col =
    let
        { red, green, blue } =
            toRgb col

        grey =
            0.299 * red + 0.587 * green + 0.114 * blue
    in
    if grey > 0.5 then
        FlatColors.AussiePalette.deepKoamaru

    else
        FlatColors.AussiePalette.coastalBreeze


radioButton label state =
    el
        [ spacing 1
        , padding 6
        , Border.color FlatColors.ChinesePalette.clearChill
        , Border.widthEach <|
            if state == Input.Selected then
                { left = 2, right = 2, top = 2, bottom = 0 }

            else
                { left = 1, right = 1, top = 1, bottom = 0 }

        --, Border.roundEach { topLeft = 6, bottomLeft = 0, topRight = 6, bottomRight = 0 }
        , Background.color <|
            if state == Input.Selected then
                FlatColors.ChinesePalette.antiFlashWhite

            else
                FlatColors.ChinesePalette.peace
        , Font.color <|
            if state == Input.Selected then
                FlatColors.ChinesePalette.prestigeBlue

            else
                FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        el [ centerX, centerY ] <|
            text label


compactRadioButton label state =
    el
        [ spacing 1
        , padding 1
        , Border.color FlatColors.ChinesePalette.clearChill
        , Border.width <|
            if state == Input.Selected then
                1

            else
                0

        --, Border.roundEach { topLeft = 6, bottomLeft = 0, topRight = 6, bottomRight = 0 }
        , Background.color <|
            if state == Input.Selected then
                FlatColors.ChinesePalette.antiFlashWhite

            else
                FlatColors.ChinesePalette.peace
        , Font.color <|
            if state == Input.Selected then
                FlatColors.ChinesePalette.prestigeBlue

            else
                FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        el [ centerX, centerY ] <|
            text label


displayName n =
    case n of
        Just s ->
            el [ Font.size 20 ]
                (text s)

        _ ->
            none


stopProp =
    { stopPropagation = True, preventDefault = False }


infoButton onPress =
    Input.button
        [ htmlAttribute <| Mouse.onWithOptions "click" stopProp (always onPress)
        ]
        { onPress = Just onPress
        , label = useIconWithSize 12 FeatherIcons.info
        }



--none


wideSliderStylesWithWidth : Quantity Int Pixels -> List (Attribute msg)
wideSliderStylesWithWidth w =
    let
        usedWidth =
            w |> Pixels.toInt |> toFloat |> (*) 0.8 |> round
    in
    [ height <| px 24
    , width <| px usedWidth
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px usedWidth
            , height <| px 2
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


neatToolsBorder =
    [ Background.color FlatColors.ChinesePalette.saturatedSky
    , Border.color FlatColors.ChinesePalette.frenchSkyBlue
    , Border.rounded 4
    , Border.width 2
    , Font.color white
    , padding 4
    ]


subtleToolStyles =
    [ Background.color FlatColors.ChinesePalette.antiFlashWhite
    , Border.color FlatColors.ChinesePalette.ufoGreen
    , Border.rounded 4
    , Border.width 2
    , padding 4
    , Font.color FlatColors.ChinesePalette.prestigeBlue
    ]


shortSliderStyles =
    [ height <| px 24
    , width <| px 150
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 150
            , height <| px 2
            , centerY
            , centerX
            , Background.color scrollbarBackground
            , Border.rounded 6
            ]
            Element.none
    ]


sliderThumb : Thumb
sliderThumb =
    thumb
        [ Element.width (Element.px 16)
        , Element.height (Element.px 16)
        , Border.rounded 4
        , Border.width 1
        , Border.color (Element.rgb 0.5 0.5 0.5)
        , Background.color (Element.rgb 1 1 1)
        ]


useIcon =
    html << FeatherIcons.toHtml [] << FeatherIcons.withSize 20


useIconWithSize size =
    html << FeatherIcons.toHtml [] << FeatherIcons.withSize size


showModalMessage : SystemSettings -> Int -> String -> msg -> Element msg
showModalMessage settings areaWidth content msg =
    let
        location =
            settings.location
    in
    column
        [ centerY
        , centerX
        , width <| px areaWidth
        , Border.width 6
        , Border.color FlatColors.AussiePalette.quinceJelly
        , Border.rounded 8
        , Background.color (CommonToolStyles.themeBackground settings.colourTheme)
        , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
        ]
        [ row
            [ width fill
            , spacing 8
            , padding 4
            , Background.color (CommonToolStyles.themeBackground settings.colourTheme)
            , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
            ]
            [ I18N.text location "main" "message"
            ]
        , column
            [ Background.color (CommonToolStyles.themeBackground settings.colourTheme)
            , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
            , height fill
            , width fill
            , spacing 20
            , padding 20
            ]
            [ paragraph
                [ width fill ]
              <|
                [ html <| Markdown.toHtml [] content ]
            , Input.button neatToolsBorder
                { onPress = Just msg
                , label = I18N.text location "main" "dismiss"
                }
            ]
        ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed msg

                        else
                            D.fail "Not the enter key"
                    )
            )
        )
