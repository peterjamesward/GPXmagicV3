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
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showShortMeasure)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder)


defaultOptions : Options
defaultOptions =
    { graph = Nothing
    , pointTolerance = Length.meter
    , minimumEdgeLength = Length.meters 100
    , centreLineOffset = Length.meters 0.0
    }


emptyGraph : Graph
emptyGraph =
    { nodes = Dict.empty
    , edges = Dict.empty
    , userRoute = []
    , canonicalRoute = []
    , trackPointToCanonical = Dict.empty
    , selectedTraversal = Nothing
    }


type Msg
    = GraphAnalyse
    | CentreLineOffset (Quantity Float Meters)
    | ConvertFromGraph
    | HighlightTraversal Traversal
    | RemoveLastTraversal
    | AddTraversalFromCurrent
    | SelectStartNode
    | SetPointTolerance (Quantity Float Meters)
    | SetMinimumEdge (Quantity Float Meters)


infoText =
    """
Here we find repeated sections of a route. You can then pick and choose which
sectons to ride, making your own route based on the original. This will ensure that
each time you use a section, the altitudes will match and render well in RGT.
"""


view : (Msg -> msg) -> Options -> Element msg
view wrapper options =
    let
        offset =
            Length.inMeters options.centreLineOffset

        analyseButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper GraphAnalyse)
                , label = text "Find route sections"
                }

        finishButton =
            I.button neatToolsBorder
                { onPress = Just (wrapper ConvertFromGraph)
                , label = text "Convert back into route"
                }

        offsetSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << CentreLineOffset << Length.meters
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

        pointToleranceSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPointTolerance << Length.meters
                , label =
                    I.labelBelow [] <|
                        text <|
                            "Consider points equal if within "
                                ++ showShortMeasure False options.pointTolerance
                , min = 0.1
                , max = 10.0
                , step = Just 0.1
                , value = Length.inMeters options.pointTolerance
                , thumb = I.defaultThumb
                }

        minEdgeSlider =
            I.slider
                commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMinimumEdge << Length.meters
                , label =
                    I.labelBelow [] <|
                        text <|
                            "Ignore sections shorter than "
                                ++ showShortMeasure False options.minimumEdgeLength
                , min = 10.0
                , max = 100.0
                , step = Just 5.0
                , value = Length.inMeters options.minimumEdgeLength
                , thumb = I.defaultThumb
                }

        removeButton =
            --TODO: Put a trashcan icon on the last line.
            I.button neatToolsBorder
                { onPress = Just (wrapper RemoveLastTraversal)
                , label = text "Remove traversal\nlast in list"
                }
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case options.graph of
            Nothing ->
                column [ width fill, padding 20, spacing 20 ]
                    [ paragraph [] [ text infoText ]
                    , pointToleranceSlider
                    , minEdgeSlider
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
                            ]

                        else
                            [ none ]
                    ]


update : Msg -> Options -> TrackLoaded msg -> (Msg -> msg) -> ( Options, List (Actions.ToolAction msg) )
update msg options track wrapper =
    case msg of
        SetPointTolerance quantity ->
            ( { options | pointTolerance = quantity }, [] )

        SetMinimumEdge quantity ->
            ( { options | minimumEdgeLength = quantity }, [] )

        GraphAnalyse ->
            ( options, [] )

        HighlightTraversal traversal ->
            ( options, [] )

        SelectStartNode ->
            ( options, [] )

        AddTraversalFromCurrent ->
            ( options, [] )

        RemoveLastTraversal ->
            ( options, [] )

        CentreLineOffset float ->
            ( { options | centreLineOffset = float }, [] )

        ConvertFromGraph ->
            ( options, [] )



-- END
