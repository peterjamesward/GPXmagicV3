module RoadBook exposing (..)

import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import Markdown



--TODO: Link to a special URL.
--TODO: Display wrapper row of entries, with mouseover revealing text description.
--TODO: Click to show map, profile and description all on one view.
--TODO: Button to download the whole book (markdown and PNGs).


type alias Entry =
    { title : String
    , content : String
    , gpx : String
    }


content : List Entry
content =
    [ Entry "Windsor"
        """This popular shortish route skims the southern edge of the Chilterns
 before taking to flatter and faster roads including the famour "Museeuw Lane" before
 rolling into Windsor for some signature Cinnamon buns at the reknowned Cinnamon Cafe."""
        "Windsor.gpx"
    ]


homeScreen model =
    wrappedRow
        [ padding 20
        , spacing 20
        , alignLeft
        , alignTop
        ]
    <|
        List.map entryAsHeading model.entries


entryAsHeading : Entry -> Element Msg
entryAsHeading entry =
    tricolore 2 <|
        Input.button
            [ spacing 10
            , width (fill |> minimum 300 |> maximum 400)
            , padding 10
            ]
            { onPress = Just <| SelectEntry (Just entry)
            , label =
                paragraph
                    [ Font.size 20
                    , Font.bold
                    , Font.color FlatColors.BritishPalette.electromagnetic
                    ]
                    [ text entry.title ]
            }


withBorder colour width =
    el
        [ Border.color colour
        , Border.width width
        , Border.rounded (width * 4)
        , Background.color white
        ]


tricolore width =
    identity
        << withBorder FlatColors.BritishPalette.downloadProgress width
        << withBorder FlatColors.BritishPalette.lynxWhite width
        << withBorder FlatColors.BritishPalette.nasturcianFlower width


entryDetail : Entry -> Element Msg
entryDetail entry =
    let
        closeButton =
            Input.button
                [ Font.color FlatColors.BritishPalette.chainGangGrey
                , alignRight
                ]
                { onPress = Just <| SelectEntry Nothing
                , label =
                    html <|
                        FeatherIcons.toHtml [] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.x
                }
    in
    tricolore 5 <|
        column
            [ spacing 10, width (fill |> maximum 500), padding 10 ]
            [ row [ width fill ]
                [ paragraph
                    [ Font.size 24
                    , Font.color FlatColors.BritishPalette.electromagnetic
                    , Font.bold
                    , padding 10
                    ]
                    [ text entry.title ]
                , closeButton
                ]
            , paragraph
                [ Font.size 20
                , padding 10
                , Font.color FlatColors.BritishPalette.blueNights
                ]
                [ html <| Markdown.toHtml [] entry.content ]
            ]
