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
import String.Interpolate
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, noTrackMessage, useIcon)


toolId =
    "intersections"


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


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view location imperial msgWrapper options track =
    let
        i18n =
            I18N.text location toolId

        resultModeSelection =
            Input.radioRow [ centerX, spacing 5 ]
                { onChange = msgWrapper << SetResultMode
                , options =
                    [ Input.option ResultNavigation (i18n "Summary")
                    , Input.option ResultList (i18n "List")
                    ]
                , selected = Just options.resultMode
                , label = Input.labelHidden "Results mode"
                }

        resultsNavigation =
            case options.features of
                [] ->
                    el [ centerX, centerY ] <| i18n "none"

                a :: b ->
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.fromInt (options.current + 1)
                                    ++ " of "
                                    ++ (String.fromInt <| List.length options.features)
                        , row [ centerX, spacing 10 ]
                            [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                            , Input.button
                                (buttonStylesWithTooltip below <| I18N.localisedString location toolId "prev")
                                { label = useIcon FeatherIcons.chevronLeft
                                , onPress = Just <| msgWrapper <| ViewPrevious
                                }
                            , Input.button
                                (buttonStylesWithTooltip below <| I18N.localisedString location toolId "this")
                                { label = useIcon FeatherIcons.mousePointer
                                , onPress = Just <| msgWrapper <| SetCurrentPosition a.thisSegment
                                }
                            , Input.button
                                (buttonStylesWithTooltip below <| I18N.localisedString location toolId "next")
                                { label = useIcon FeatherIcons.chevronRight
                                , onPress = Just <| msgWrapper <| ViewNext
                                }
                            ]
                        ]

        linkButton : Intersection -> Element msg
        linkButton { thisSegment, otherSegment, category } =
            let
                distanceText =
                    showLongMeasure imperial <| DomainModel.distanceFromIndex thisSegment track.trackTree

                categoryText =
                    case category of
                        RoadIndex.Crossing pointXY ->
                            "crosses"

                        RoadIndex.SameDirection ->
                            "loops"

                        RoadIndex.ContraDirection ->
                            "reverses"

                thisText =
                    String.fromInt thisSegment

                otherText =
                    String.fromInt otherSegment
            in
            Input.button neatToolsBorder
                { onPress = Just (msgWrapper <| SetCurrentPosition thisSegment)
                , label =
                    text <|
                        String.Interpolate.interpolate
                            (I18N.localisedString location toolId "detail")
                            [ thisText, categoryText, otherText, distanceText ]
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
