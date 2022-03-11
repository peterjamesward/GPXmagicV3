module Tools.Intersections exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (labelHidden)
import FeatherIcons
import FlatColors.ChinesePalette
import List.Extra
import PreviewData exposing (PreviewShape(..))
import RoadIndex exposing (Intersection)
import ToolTip exposing (buttonStylesWithTooltip)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (neatToolsBorder, noTrackMessage, useIcon)


type alias Options =
    { features : List Intersection
    , current : Int
    , resultMode : ResultMode
    }


type ResultMode
    = ResultList
    | ResultNavigation


defaultOptions : Options
defaultOptions =
    { features = []
    , current = 0
    , resultMode = ResultNavigation
    }


type Msg
    = ViewNext
    | ViewPrevious
    | SetCurrentPosition Int
    | SetResultMode ResultMode
    | DisplayInfo String String


toolID : String
toolID =
    "intersections"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Intersections" )
        , ( "info", infoText )
        ]
    )


infoText =
    """This helps to find places where one road section crosses another, or where a piece
of road is used more than once, either in the same or the opposite direction.

This is gateway to thinking of a route as something that can be navigated differently,
but that's where _Route builder_ comes to play.
"""


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            -- Make sure we have up to date breaches and preview is shown.
            let
                newOptions =
                    { options
                        | features = RoadIndex.findFeatures theTrack.trackTree
                    }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "features"
                    , shape = PreviewCircle
                    , colour = colour
                    , points =
                        TrackLoaded.buildPreview
                            (List.map .thisSegment newOptions.features)
                            theTrack.trackTree
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "features" ] )


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> ( Options, List (ToolAction msg) )
update msg options wrap =
    case msg of
        SetResultMode mode ->
            let
                newOptions =
                    { options | resultMode = mode }
            in
            ( newOptions, [] )

        ViewNext ->
            let
                index =
                    min (List.length options.features - 1) (options.current + 1)

                newOptions =
                    { options | current = index }

                position =
                    options.features
                        |> List.Extra.getAt index
                        |> Maybe.map .thisSegment
                        |> Maybe.withDefault 0
            in
            ( newOptions, [ SetCurrent position ] )

        ViewPrevious ->
            let
                index =
                    max 0 (options.current + 1)

                newOptions =
                    { options | current = index }

                position =
                    options.features
                        |> List.Extra.getAt index
                        |> Maybe.map .thisSegment
                        |> Maybe.withDefault 0
            in
            ( newOptions, [ SetCurrent position ] )

        SetCurrentPosition position ->
            ( options, [ SetCurrent position ] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


view : Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view imperial msgWrapper options track =
    let
        resultModeSelection =
            Input.radioRow [ centerX, spacing 5 ]
                { onChange = msgWrapper << SetResultMode
                , options =
                    [ Input.option ResultNavigation (text "Summary")
                    , Input.option ResultList (text "List")
                    ]
                , selected = Just options.resultMode
                , label = Input.labelHidden "Results mode"
                }

        resultsNavigation =
            case options.features of
                [] ->
                    el [ centerX, centerY ] <| text "None found"

                a :: b ->
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.fromInt (options.current + 1)
                                    ++ " of "
                                    ++ (String.fromInt <| List.length options.features)
                        , row [ centerX, spacing 10 ]
                            [ Input.button
                                (buttonStylesWithTooltip below "Move to previous")
                                { label = useIcon FeatherIcons.chevronLeft
                                , onPress = Just <| msgWrapper <| ViewPrevious
                                }
                            , Input.button
                                (buttonStylesWithTooltip below "Centre view on this issue")
                                { label = useIcon FeatherIcons.mousePointer
                                , onPress = Just <| msgWrapper <| SetCurrentPosition a.thisSegment
                                }
                            , Input.button
                                (buttonStylesWithTooltip below "Move to next")
                                { label = useIcon FeatherIcons.chevronRight
                                , onPress = Just <| msgWrapper <| ViewNext
                                }
                            ]
                        ]

        linkButton : Intersection -> Element msg
        linkButton { thisSegment, otherSegment, category } =
            let
                distamceText =
                    " at "
                        ++ (showLongMeasure imperial <|
                                DomainModel.distanceFromIndex thisSegment track.trackTree
                           )

                categoryText =
                    case category of
                        RoadIndex.Crossing pointXY ->
                            " crosses "

                        RoadIndex.SameDirection ->
                            " loops "

                        RoadIndex.ContraDirection ->
                            " reverses "

                thisText =
                    String.fromInt thisSegment

                otherText =
                    String.fromInt otherSegment
            in
            Input.button neatToolsBorder
                { onPress = Just (msgWrapper <| SetCurrentPosition thisSegment)
                , label = text <| thisText ++ categoryText ++ otherText ++ distamceText
                }
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        column [ centerX, padding 5, spacing 8 ]
            [ resultModeSelection
            , case options.resultMode of
                ResultNavigation ->
                    resultsNavigation

                ResultList ->
                    column [ height <| px 150, scrollbarY ] <|
                        List.map linkButton options.features
            ]
