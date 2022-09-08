module ViewPureStyles exposing (..)

import Color exposing (black, blue)
import ColourPalette exposing (buttonBackground, buttonShadow, buttonText, collapsedTabBorder, radioButtonDefault, radioButtonSelected, radioButtonText, scrollbarBackground)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Thumb, thumb)
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import Html.Attributes exposing (style)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Markdown
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions


defaultRowLayout =
    []


toolRowLayout =
    [ spacing 10
    , paddingXY 20 10
    , width fill
    ]


rgtPurple =
    Element.rgb255 63 0 73


rgtDark =
    Element.rgb255 12 0 22


ukraineBlue =
    rgb255 0 87 183


ukraineYellow =
    rgb255 255 221 0


black =
    rgb255 0 0 0


edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


defaultColumnLayout =
    [ spacing 5, padding 5, alignTop, width fill ]


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


disabledButtonStyles =
    [ padding 10
    , Border.width 2
    , Border.rounded 4
    , Border.color FlatColors.BritishPalette.riseNShine
    , Background.color FlatColors.BritishPalette.riseNShine
    , Font.color FlatColors.ChinesePalette.antiFlashWhite
    , Font.size 16
    , width fill
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


checkboxIcon : Bool -> Element msg
checkboxIcon isChecked =
    el
        [ width <| px 32
        , height <| px 32
        , centerY
        , padding 4
        , Border.rounded 4
        , Border.width 2
        , Border.color FlatColors.ChinesePalette.frenchSkyBlue
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 4
            , Background.color <|
                if isChecked then
                    FlatColors.ChinesePalette.bruschettaTomato

                else
                    FlatColors.ChinesePalette.twinkleBlue
            ]
        <|
            none


contrastingColour col =
    let
        { red, green, blue, alpha } =
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


wideSliderStyles =
    wideSliderStylesWithWidth (Pixels.int 400)


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


showModalMessage : I18NOptions.Location -> Int -> String -> msg -> Element msg
showModalMessage location areaWidth content msg =
    column
        [ centerY
        , centerX
        , width <| px areaWidth
        , Border.width 6
        , Border.color FlatColors.AussiePalette.quinceJelly
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , spacing 8
            , padding 4
            , Background.color FlatColors.AussiePalette.quinceJelly
            , Font.color FlatColors.AussiePalette.coastalBreeze
            ]
            [ I18N.text location "main" "message"
            ]
        , column
            [ Background.color FlatColors.AussiePalette.coastalBreeze
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


noTrackMessage location =
    paragraph
        [ padding 20
        , spacing 5
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , centerX
        , centerY
        , height fill
        ]
        [ I18N.text location "tools" "notrack" ]
