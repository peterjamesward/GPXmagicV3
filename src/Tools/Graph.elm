module Tools.Graph exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Input as I
import FlatColors.ChinesePalette
import Length exposing (Length, Meters, inMeters)
import Quantity exposing (Quantity, zero)
import Tools.GraphOptions exposing (..)
import UtilsForViews exposing (showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder)


defaultOptions : Options
defaultOptions =
    { graph = Nothing
    , pointTolerance = Length.meter
    , minimumEdgeLength = Length.meters 100
    }


emptyGraph : Graph
emptyGraph =
    { nodes = Dict.empty
    , edges = Dict.empty
    , userRoute = []
    , canonicalRoute = []
    , centreLineOffset = Length.meters 0.0
    , trackPointToCanonical = Dict.empty
    , selectedTraversal = Nothing
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset Float
    | ConvertFromGraph
    | HighlightTraversal Traversal
    | RemoveLastTraversal
    | AddTraversalFromCurrent
    | AddFirstTraversal


infoText =
    """
Route maker finds repeated sections of a route. You can then pick and choose which 
sectons to ride, making your own route based on the original. This will ensure that
each time you use a section, the altitudes will match and render well in RGT.
"""


view : (Msg -> msg) -> Options -> Element msg
view wrapper options =
    let
        offset =
            Maybe.map .centreLineOffset options.graph
                |> Maybe.map inMeters
                |> Maybe.withDefault 0.0

        analyseButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper GraphAnalyse)
                , label = text "Find route sections"
                }

        finishButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper ConvertFromGraph)
                , label = text "Convert from Graph"
                }

        offsetSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << CentreLineOffset
                , label =
                    I.labelBelow [] <|
                        text <|
                            "Offset = "
                                ++ (showDecimal2 <| abs offset)
                                ++ "m "
                                ++ (if offset < 0.0 then
                                        "left"

                                    else if offset > 0.0 then
                                        "right"

                                    else
                                        ""
                                   )
                , min = -5.0
                , max = 5.0
                , step = Just 0.25
                , value = offset
                , thumb = I.defaultThumb
                }

        removeButton =
            --TODO: Put a tarshcan icon on the last line.
            I.button neatToolsBorder
                { onPress = Just (wrapper RemoveLastTraversal)
                , label = text "Remove traversal\nlast in list"
                }

        addButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper AddTraversalFromCurrent)
                , label = text "Add traversal\nat Orange marker"
                }

        addFirstButton =
            --TODO: Replace with select start node, then arcs.
            I.button neatToolsBorder
                { onPress = Just (wrapper AddFirstTraversal)
                , label = text "Add traversal using\nPurple and Orange markers"
                }
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case options.graph of
            Nothing ->
                column [ width fill, padding 20, spacing 20 ]
                    [ paragraph [] [ text infoText ]
                    , analyseButton
                    ]

            Just g ->
                column [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                    [ row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ]
                        [ offsetSlider
                        , finishButton
                        ]

                    --, showTheRoute g wrapper
                    , row [ width fill, spaceEvenly, paddingXY 20 10, spacingXY 20 10 ] <|
                        if List.length g.userRoute > 0 then
                            [ removeButton
                            , addButton
                            ]

                        else
                            [ addFirstButton ]
                    ]


update : Msg -> Options -> (Msg -> msg) -> ( Options, List (Actions.ToolAction msg) )
update msg options wrapper =
    ( options, [] )



-- END
