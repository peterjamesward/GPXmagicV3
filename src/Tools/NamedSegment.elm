module Tools.NamedSegment exposing (Msg(..), addSegment, defaultOptions, initialise, toolId, toolStateChange, update, view)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the track points, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions exposing (ToolAction)
import CommonToolStyles
import Dict
import DomainModel exposing (EarthPoint, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import Length exposing (Meters)
import List.Extra
import Point3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import Quantity.Interval as Interval
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.NamedSegmentOptions exposing (CreateMode(..), NamedSegment, Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, rgtDark, rgtPurple, sliderThumb, useIcon, useIconWithSize)


toolId =
    "segments"


defaultOptions : Options
defaultOptions =
    { selectedSegment = Nothing
    , landUseProximity = Nothing
    , landUsePreferCloser = False
    }


type Msg
    = SelectSegment Int
    | UpdateSegment
    | ChangeName Int String
    | DeleteSegment
    | CreateSegment
    | DisplayInfo String String


initialise : Options
initialise =
    { selectedSegment = Nothing
    , landUseProximity = Nothing
    , landUsePreferCloser = False
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
            ( options
            , [ exclusionZones theTrack
              , makePreview colour theTrack
              ]
            )

        _ ->
            -- Hide preview
            ( options
            , [ Actions.HidePreview "segments"
              , Actions.HidePreview "deadzones"
              , Actions.HidePreview "segmentprofile"
              ]
            )


makePreview colour track =
    let
        getStartIndex segment =
            DomainModel.indexFromDistanceRoundedUp segment.startDistance track.trackTree

        getEndIndex segment =
            DomainModel.indexFromDistanceRoundedDown segment.endDistance track.trackTree

        segmentIndices segment =
            List.range (getStartIndex segment) (getEndIndex segment)

        previewPoints =
            TrackLoaded.buildPreview
                (List.concatMap segmentIndices track.namedSegments)
                track.trackTree
    in
    Actions.ShowPreview
        { tag = "segments"
        , shape = PreviewCircle
        , colour = colour
        , points = previewPoints
        }


exclusionZones track =
    let
        fromStart =
            List.range 0
                (DomainModel.indexFromDistanceRoundedDown
                    (Length.meters 110)
                    track.trackTree
                )

        trackLength =
            DomainModel.trueLength track.trackTree

        fromEnd =
            List.range
                (DomainModel.indexFromDistanceRoundedUp
                    (trackLength |> Quantity.minus (Length.meters 190))
                    track.trackTree
                )
                (DomainModel.skipCount track.trackTree)
    in
    Actions.ShowPreview
        { tag = "deadzones"
        , shape = PreviewCircle
        , colour = FlatColors.FlatUIPalette.clouds
        , points =
            TrackLoaded.buildPreview
                (fromStart ++ fromEnd)
                track.trackTree
        }


view : SystemSettings -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view settings wrapper options track =
    let
        validated =
            checkForRuleBreaches track options

        i18n =
            I18N.text settings.location toolId

        dataStyles selected =
            if selected then
                Font.bold :: CommonToolStyles.toolContentBoxStyle settings

            else
                CommonToolStyles.toolContentBoxStyle settings

        highlightErrors isOk =
            if isOk then
                [ padding 2 ]

            else
                [ Font.bold
                , Background.color FlatColors.AmericanPalette.brightYarrow
                , padding 2
                ]

        segmentsTable : Element msg
        segmentsTable =
            let
                headerAttrs =
                    [ Font.bold
                    , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                    , Border.color rgtPurple
                    ]
            in
            column
                [ width <| minimum 200 <| maximum 500 fill

                --, height <| px 150
                , spacing 10
                , padding 5
                , Border.width 2
                , Border.rounded 6
                , Border.color FlatColors.FlatUIPalette.concrete
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
                        { data = validated
                        , columns =
                            [ { header = none
                              , width = fillPortion 3
                              , view =
                                    \i t ->
                                        Input.text (dataStyles (Just i == options.selectedSegment))
                                            { onChange = wrapper << ChangeName i
                                            , text = t.name
                                            , placeholder = Nothing
                                            , label = Input.labelHidden "name"
                                            }
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \_ t ->
                                        el (highlightErrors t.startOk) <|
                                            text <|
                                                showLongMeasure settings.imperial t.startDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \_ t ->
                                        el (highlightErrors t.endOk) <|
                                            text <|
                                                showLongMeasure settings.imperial t.endDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i _ ->
                                        row [ spaceEvenly ] <|
                                            Input.button
                                                [ tooltip onLeft (localisedTooltip settings.location toolId "show") ]
                                                { label = useIcon FeatherIcons.eye
                                                , onPress = Just <| wrapper (SelectSegment i)
                                                }
                                                :: (if Just i == options.selectedSegment then
                                                        [ Input.button
                                                            [ tooltip onLeft (localisedTooltip settings.location toolId "update")
                                                            ]
                                                            { label = useIcon FeatherIcons.checkCircle
                                                            , onPress = Just <| wrapper UpdateSegment
                                                            }
                                                        , Input.button
                                                            [ tooltip onLeft (localisedTooltip settings.location toolId "delete")
                                                            ]
                                                            { label = useIcon FeatherIcons.trash2
                                                            , onPress = Just <| wrapper DeleteSegment
                                                            }
                                                        ]

                                                    else
                                                        []
                                                   )
                              }
                            ]
                        }
                ]

        selectedSegmentDetail =
            case options.selectedSegment of
                Just selected ->
                    -- Should be in list but of course we check
                    case List.Extra.getAt selected track.namedSegments of
                        Nothing ->
                            paragraph [] [ i18n "select" ]

                        Just segment ->
                            let
                                labels =
                                    [ "distance"
                                    , "ascent"
                                    , "descent"
                                    , "steepest"
                                    ]

                                ( startIndex, endIndex ) =
                                    ( DomainModel.indexFromDistance segment.startDistance track.trackTree
                                    , DomainModel.indexFromDistance segment.endDistance track.trackTree
                                    )

                                ( ascent, descent, maxGrade ) =
                                    DomainModel.traverseTreeBetweenLimitsToDepth
                                        startIndex
                                        endIndex
                                        (always Nothing)
                                        0
                                        track.trackTree
                                        upsAndDowns
                                        ( Quantity.zero, Quantity.zero, 0.0 )

                                distance =
                                    segment.endDistance |> Quantity.minus segment.startDistance

                                upsAndDowns :
                                    RoadSection
                                    -> ( Quantity Float Meters, Quantity Float Meters, Float )
                                    -> ( Quantity Float Meters, Quantity Float Meters, Float )
                                upsAndDowns road ( up, down, steepest ) =
                                    ( Quantity.plus up road.altitudeGained
                                    , Quantity.plus down road.altitudeLost
                                    , max steepest road.gradientAtStart
                                    )
                            in
                            row
                                [ padding 10
                                , spacing 5
                                ]
                                [ column [ spacing 5 ] <| List.map (I18N.text settings.location "info") labels
                                , column [ spacing 5 ]
                                    [ text <| showLongMeasure settings.imperial distance
                                    , text <| showShortMeasure settings.imperial ascent
                                    , text <| showShortMeasure settings.imperial descent
                                    , text <| showDecimal2 maxGrade
                                    ]
                                ]

                Nothing ->
                    paragraph [] [ i18n "select" ]

        newSegmentButton =
            el [ centerX ] <|
                Input.button neatToolsBorder
                    { onPress = Just <| wrapper CreateSegment
                    , label = i18n "create"
                    }

        goodSeparation segments =
            segments
                |> List.all
                    (\seg -> seg.startOk && seg.endOk)

        overlapWarning =
            if goodSeparation validated then
                none

            else
                row
                    [ width fill
                    , padding 5
                    , spacing 5
                    , Background.color FlatColors.AmericanPalette.brightYarrow
                    ]
                    [ useIconWithSize 48 FeatherIcons.alertTriangle
                    , paragraph [] [ text <| I18N.localisedString settings.location toolId "warning" ]
                    ]

        duplicateWarning =
            if List.Extra.allDifferentBy .name track.namedSegments then
                none

            else
                row
                    [ width fill
                    , padding 5
                    , spacing 5
                    , Background.color FlatColors.AmericanPalette.brightYarrow
                    ]
                    [ useIconWithSize 48 FeatherIcons.alertTriangle
                    , paragraph [] [ text <| I18N.localisedString settings.location toolId "duplicate" ]
                    ]
    in
    column (CommonToolStyles.toolContentBoxStyle settings)
        [ segmentsTable
        , selectedSegmentDetail
        , newSegmentButton
        , overlapWarning
        , duplicateWarning
        ]


addSegment : NamedSegment -> TrackLoaded msg -> List NamedSegment
addSegment segment track =
    List.sortBy
        (.startDistance >> Length.inMeters)
        (segment :: track.namedSegments)


checkForRuleBreaches : TrackLoaded msg -> Options -> List NamedSegment
checkForRuleBreaches track options =
    -- Note the list MUST be sorted beforehand!
    let
        dummyFirst =
            { startDistance = Quantity.zero
            , endDistance = Quantity.zero
            , name = "dummy"
            , createMode = AutoSegment
            , startOk = True
            , endOk = True
            }

        dummyLast =
            { startDistance = DomainModel.trueLength track.trackTree
            , endDistance = DomainModel.trueLength track.trackTree
            , name = "dummy"
            , createMode = AutoSegment
            , startOk = True
            , endOk = True
            }

        validate previous current next =
            { current
                | startOk =
                    (current.startDistance
                        |> Quantity.greaterThan
                            (previous.endDistance |> Quantity.plus (Length.meters 50))
                    )
                        && (current.startDistance |> Quantity.greaterThan (Length.meters 110))
                , endOk =
                    (current.endDistance
                        |> Quantity.plus (Length.meters 50)
                        |> Quantity.lessThan next.startDistance
                    )
                        && (current.endDistance
                                |> Quantity.plus (Length.meters 190)
                                |> Quantity.lessThan (DomainModel.trueLength track.trackTree)
                           )
            }

        validated =
            List.map3 validate
                (dummyFirst :: track.namedSegments)
                track.namedSegments
                (List.drop 1 track.namedSegments ++ [ dummyLast ])
    in
    validated


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> Element.Color
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track previewColour wrapper =
    case msg of
        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        SelectSegment seg ->
            case List.Extra.getAt seg track.namedSegments of
                Just segment ->
                    let
                        ( startIndex, endIndex ) =
                            ( DomainModel.indexFromDistance segment.startDistance track.trackTree
                            , DomainModel.indexFromDistance segment.endDistance track.trackTree
                            )
                    in
                    ( { options | selectedSegment = Just seg }
                    , [ Actions.SetCurrent startIndex
                      , Actions.SetMarker (Just endIndex)
                      , Actions.PointerChange
                      ]
                    )

                Nothing ->
                    ( { options | selectedSegment = Nothing }, [] )

        UpdateSegment ->
            case options.selectedSegment of
                Nothing ->
                    ( { options | selectedSegment = Nothing }, [] )

                Just index ->
                    case List.Extra.getAt index track.namedSegments of
                        Nothing ->
                            ( { options | selectedSegment = Nothing }, [] )

                        Just segment ->
                            let
                                ( fromStart, fromEnd ) =
                                    TrackLoaded.getRangeFromMarkers track

                                endIndex =
                                    DomainModel.skipCount track.trackTree - fromEnd

                                updated =
                                    { segment
                                        | startDistance = DomainModel.distanceFromIndex fromStart track.trackTree
                                        , endDistance = DomainModel.distanceFromIndex endIndex track.trackTree
                                        , createMode = ManualSegment
                                    }

                                newSegments =
                                    track.namedSegments
                                        |> List.Extra.updateAt index (always updated)
                                        |> List.sortBy (.startDistance >> Length.inMeters)
                            in
                            ( options
                            , [ exclusionZones track
                              , makePreview previewColour track
                              , Actions.UpdateNamedSegments newSegments
                              ]
                            )

        DeleteSegment ->
            case options.selectedSegment of
                Nothing ->
                    ( { options | selectedSegment = Nothing }, [] )

                Just index ->
                    case List.Extra.getAt index track.namedSegments of
                        Nothing ->
                            ( { options | selectedSegment = Nothing }, [] )

                        Just _ ->
                            let
                                newSegments =
                                    List.Extra.removeAt index track.namedSegments

                                newOptions =
                                    { options | selectedSegment = Nothing }
                            in
                            ( newOptions
                            , [ exclusionZones track
                              , makePreview previewColour track
                              , Actions.UpdateNamedSegments newSegments
                              ]
                            )

        ChangeName index newName ->
            case List.Extra.getAt index track.namedSegments of
                Nothing ->
                    ( { options | selectedSegment = Nothing }, [] )

                Just segment ->
                    let
                        updatedSegment =
                            { segment | name = newName }

                        newSegments =
                            List.Extra.updateAt index (always updatedSegment) track.namedSegments
                    in
                    ( options
                    , [ Actions.UpdateNamedSegments newSegments ]
                    )

        CreateSegment ->
            -- Note validation is in the view, so create without checking overlaps.
            let
                ( fromStart, fromEnd ) =
                    TrackLoaded.getRangeFromMarkers track

                newSegment =
                    { startDistance = DomainModel.distanceFromIndex fromStart track.trackTree
                    , endDistance =
                        DomainModel.distanceFromIndex
                            (DomainModel.skipCount track.trackTree - fromEnd)
                            track.trackTree
                    , name = "ENTER NAME HERE"
                    , createMode = ManualSegment
                    , startOk = True
                    , endOk = True
                    }

                newSegments =
                    addSegment newSegment track
            in
            ( options
            , [ exclusionZones track
              , makePreview previewColour track
              , Actions.UpdateNamedSegments newSegments
              ]
            )


type alias SegmentCandidate =
    { name : String
    , place : EarthPoint
    , nearestTrackPointIndex : Int
    , distanceAway : Length.Length
    }



-- END
