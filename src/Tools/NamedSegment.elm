module Tools.NamedSegment exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the track points, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions exposing (ToolAction)
import DomainModel exposing (RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.ChinesePalette
import Length exposing (Meters)
import List.Extra
import Quantity exposing (Quantity)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.NamedSegmentOptions exposing (NamedSegment, Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (rgtDark, rgtPurple)


toolId =
    "segments"


defaultOptions : Options
defaultOptions =
    { selectedSegment = Nothing
    , namedSegments = []
    }


type Msg
    = NoOp
    | SelectSegment Int


initialise : List NamedSegment -> Options
initialise segments =
    { namedSegments = segments
    , selectedSegment = Nothing
    }


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, [] )

        _ ->
            -- Hide preview
            ( options, [] )


view : I18NOptions.Location -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view location wrapper options track =
    let
        i18n =
            I18N.text location toolId

        dataStyles selected =
            if selected then
                [ Font.color FlatColors.ChinesePalette.antiFlashWhite
                , Font.bold
                , Background.color rgtPurple
                , padding 2
                ]

            else
                [ Font.color rgtDark, padding 2 ]

        segmentsTable : Element msg
        segmentsTable =
            let
                headerAttrs =
                    [ Font.bold
                    , Font.color rgtDark
                    , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                    , Border.color rgtPurple
                    ]
            in
            column
                [ width <| maximum 500 fill
                , height <| px 150
                , spacing 10
                , padding 5
                , Border.width 2
                , Border.rounded 6
                , Border.color rgtDark
                ]
                [ row [ width fill ]
                    [ el ((width <| fillPortion 2) :: headerAttrs) <| i18n "name"
                    , el ((width <| fillPortion 1) :: headerAttrs) <| i18n "start"
                    , el ((width <| fillPortion 1) :: headerAttrs) <| i18n "end"
                    , el ((width <| fillPortion 1) :: headerAttrs) <| text "  "
                    ]

                -- workaround for a bug: it's necessary to wrap `table` in an `el`
                -- to get table height attribute to apply
                , el [ width fill ] <|
                    indexedTable
                        [ width fill
                        , height <| px 120
                        , scrollbarY
                        , spacing 4
                        ]
                        { data = options.namedSegments
                        , columns =
                            [ { header = none
                              , width = fillPortion 2
                              , view =
                                    \i t ->
                                        Input.button (dataStyles (Just i == options.selectedSegment))
                                            { label = text t.name
                                            , onPress = Just <| wrapper <| SelectSegment i
                                            }
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        el (dataStyles (Just i == options.selectedSegment)) <|
                                            text <|
                                                showLongMeasure False t.startDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        el (dataStyles (Just i == options.selectedSegment)) <|
                                            text <|
                                                showLongMeasure False t.endDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        el (dataStyles (Just i == options.selectedSegment)) <|
                                            text "actions"
                              }
                            ]
                        }
                ]

        labels =
            [ "distance"
            , "ascent"
            , "descent"
            ]

        selectedSegmentDetail =
            case options.selectedSegment of
                Just selected ->
                    -- Should be in list but of course we check
                    case List.Extra.getAt selected options.namedSegments of
                        Nothing ->
                            paragraph [] [ i18n "select" ]

                        Just segment ->
                            let
                                ( startIndex, endIndex ) =
                                    ( DomainModel.indexFromDistance segment.startDistance track.trackTree
                                    , DomainModel.indexFromDistance segment.endDistance track.trackTree
                                    )

                                ( ascent, descent ) =
                                    DomainModel.traverseTreeBetweenLimitsToDepth
                                        startIndex
                                        endIndex
                                        (always Nothing)
                                        0
                                        track.trackTree
                                        upsAndDowns
                                        ( Quantity.zero, Quantity.zero )

                                distance =
                                    segment.endDistance |> Quantity.minus segment.startDistance

                                upsAndDowns :
                                    RoadSection
                                    -> ( Quantity Float Meters, Quantity Float Meters )
                                    -> ( Quantity Float Meters, Quantity Float Meters )
                                upsAndDowns road ( up, down ) =
                                    ( Quantity.plus up road.altitudeGained
                                    , Quantity.plus down road.altitudeLost
                                    )
                            in
                            row
                                [ padding 10
                                , spacing 5
                                ]
                                [ column [ spacing 5 ] <| List.map (I18N.text location "info") labels
                                , column [ spacing 5 ]
                                    [ text <| showLongMeasure False distance
                                    , text <| showShortMeasure False ascent
                                    , text <| showShortMeasure False descent
                                    ]
                                ]

                Nothing ->
                    paragraph [] [ i18n "select" ]
    in
    el
        [ width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , padding 4
        ]
    <|
        column [ width fill, padding 4, spacing 10 ]
            [ segmentsTable
            , selectedSegmentDetail
            ]


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track wrapper =
    case msg of
        NoOp ->
            ( { options | selectedSegment = Nothing }, [] )

        SelectSegment seg ->
            ( { options | selectedSegment = Just seg }, [] )



-- END
