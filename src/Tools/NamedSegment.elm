module Tools.NamedSegment exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the track points, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions exposing (ToolAction)
import Color
import Dict
import DomainModel exposing (EarthPoint, RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.BritishPalette
import FlatColors.ChinesePalette
import Length exposing (Meters)
import List.Extra
import Point3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import Quantity.Interval as Interval
import String.Interpolate
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
    , namedSegments = []
    , landUseProximity = Nothing
    , landUsePreferCloser = False
    }


type Msg
    = NoOp
    | SelectSegment Int
    | UpdateSegment
    | ChangeName Int String
    | DeleteSegment
    | CreateSegment
    | EnableAutoSuggest Bool
    | TogglePreferCloser Bool
    | LandUseProximity Length.Length
    | DisplayInfo String String


initialise : List NamedSegment -> Options
initialise segments =
    { namedSegments = segments
    , selectedSegment = Nothing
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
            , [ makePreview colour options theTrack ]
            )

        _ ->
            -- Hide preview
            ( options, [ Actions.HidePreview "segments" ] )


makePreview colour options track =
    let
        getStartIndex segment =
            DomainModel.indexFromDistanceRoundedUp segment.startDistance track.trackTree

        getEndIndex segment =
            DomainModel.indexFromDistanceRoundedDown segment.endDistance track.trackTree

        segmentIndices segment =
            List.range (getStartIndex segment) (getEndIndex segment)
    in
    Actions.ShowPreview
        { tag = "segments"
        , shape = PreviewCircle
        , colour = colour
        , points =
            TrackLoaded.buildPreview
                (List.concatMap segmentIndices options.namedSegments)
                track.trackTree
        }


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view location imperial wrapper options track =
    let
        validated =
            checkForRuleBreaches track options

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

        highlightErrors isOk =
            if isOk then
                [ Font.color rgtDark, padding 2 ]

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
                    , Font.color rgtDark
                    , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                    , Border.color rgtPurple
                    ]
            in
            column
                [ width <| maximum 500 fill

                --, height <| px 150
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
                        { data = validated.namedSegments
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
                                    \i t ->
                                        el (highlightErrors t.startOk) <|
                                            text <|
                                                showLongMeasure imperial t.startDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        el (highlightErrors t.endOk) <|
                                            text <|
                                                showLongMeasure imperial t.endDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        row [ spaceEvenly ] <|
                                            Input.button
                                                [ tooltip onLeft (localisedTooltip location toolId "show") ]
                                                { label = useIcon FeatherIcons.eye
                                                , onPress = Just <| wrapper (SelectSegment i)
                                                }
                                                :: (if Just i == options.selectedSegment then
                                                        [ Input.button
                                                            [ tooltip onLeft (localisedTooltip location toolId "update")
                                                            ]
                                                            { label = useIcon FeatherIcons.checkCircle
                                                            , onPress = Just <| wrapper UpdateSegment
                                                            }
                                                        , Input.button
                                                            [ tooltip onLeft (localisedTooltip location toolId "delete")
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

        labels =
            [ "distance"
            , "ascent"
            , "descent"
            , "steepest"
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
                                [ column [ spacing 5 ] <| List.map (I18N.text location "info") labels
                                , column [ spacing 5 ]
                                    [ text <| showLongMeasure imperial distance
                                    , text <| showShortMeasure imperial ascent
                                    , text <| showShortMeasure imperial descent
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

        autoSuggestButton =
            -- If we have named places from Land Use, they may provide segment names.
            el [ centerX ] <|
                if Dict.isEmpty track.landUseData.places then
                    i18n "nolanduse"

                else
                    column []
                        [ row []
                            [ Input.checkbox
                                [ padding 5
                                , spacing 5
                                ]
                                { onChange = wrapper << EnableAutoSuggest
                                , checked = options.landUseProximity /= Nothing
                                , label = Input.labelRight [] <| i18n "landuse"
                                , icon = Input.defaultCheckbox
                                }
                            , infoButton (wrapper <| DisplayInfo toolId "landusetip")
                            ]
                        , if options.landUseProximity == Nothing then
                            none

                          else
                            Input.slider
                                ViewPureStyles.shortSliderStyles
                                { onChange = Length.meters >> LandUseProximity >> wrapper
                                , value =
                                    Maybe.map Length.inMeters options.landUseProximity
                                        |> Maybe.withDefault 0
                                , label =
                                    Input.labelBelow [] <|
                                        text <|
                                            String.Interpolate.interpolate
                                                (I18N.localisedString location toolId "proximity")
                                                [ showDecimal0 <|
                                                    Maybe.withDefault 0 <|
                                                        Maybe.map Length.inMeters options.landUseProximity
                                                ]
                                , min = 50
                                , max = 5000
                                , step = Just 50
                                , thumb = sliderThumb
                                }
                        , if options.landUseProximity == Nothing then
                            none

                          else
                            Input.checkbox
                                [ padding 5
                                , spacing 5
                                ]
                                { onChange = wrapper << TogglePreferCloser
                                , checked = options.landUsePreferCloser
                                , label = Input.labelRight [] <| i18n "closer"
                                , icon = Input.defaultCheckbox
                                }
                        ]

        goodSeparation segments =
            segments
                |> List.all
                    (\seg -> seg.startOk && seg.endOk)

        overlapWarning =
            if goodSeparation validated.namedSegments then
                none

            else
                row
                    [ width fill
                    , padding 5
                    , spacing 5
                    , Background.color FlatColors.AmericanPalette.brightYarrow
                    ]
                    [ useIconWithSize 48 FeatherIcons.alertTriangle
                    , paragraph [] [ text <| I18N.localisedString location toolId "warning" ]
                    ]

        duplicateWarning =
            if List.Extra.allDifferentBy .name options.namedSegments then
                none

            else
                row
                    [ width fill
                    , padding 5
                    , spacing 5
                    , Background.color FlatColors.AmericanPalette.brightYarrow
                    ]
                    [ useIconWithSize 48 FeatherIcons.alertTriangle
                    , paragraph [] [ text <| I18N.localisedString location toolId "duplicate" ]
                    ]
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
            , newSegmentButton
            , autoSuggestButton
            , overlapWarning
            , duplicateWarning
            ]


addSegment : NamedSegment -> Options -> Options
addSegment segment options =
    { options
        | namedSegments =
            List.sortBy
                (.startDistance >> Length.inMeters)
                (segment :: options.namedSegments)
    }


checkForRuleBreaches : TrackLoaded msg -> Options -> Options
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
                    (previous.endDistance
                        |> Quantity.plus (Length.meters 50)
                        |> Quantity.lessThan current.startDistance
                    )
                        && (current.startDistance |> Quantity.greaterThan (Length.meters 110))
                , endOk =
                    (current.endDistance
                        |> Quantity.plus (Length.meters 50)
                        |> Quantity.lessThan next.startDistance
                    )
                        && (current.endDistance
                                |> Quantity.plus (Length.meters 200)
                                |> Quantity.lessThan (DomainModel.trueLength track.trackTree)
                           )
            }

        validated =
            List.map3 validate
                (dummyFirst :: options.namedSegments)
                options.namedSegments
                (List.drop 1 options.namedSegments ++ [ dummyLast ])
    in
    { options
        | namedSegments = validated
    }


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> Element.Color
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track previewColour wrapper =
    let
        getStartIndex segment =
            DomainModel.indexFromDistanceRoundedDown segment.startDistance track.trackTree
    in
    case msg of
        NoOp ->
            ( { options | selectedSegment = Nothing }, [] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        SelectSegment seg ->
            case List.Extra.getAt seg options.namedSegments of
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
                    case List.Extra.getAt index options.namedSegments of
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

                                newOptions =
                                    { options
                                        | namedSegments =
                                            options.namedSegments
                                                |> List.Extra.updateAt index (always updated)
                                                |> List.sortBy (.startDistance >> Length.inMeters)
                                    }
                            in
                            ( newOptions
                            , [ makePreview previewColour newOptions track ]
                            )

        DeleteSegment ->
            case options.selectedSegment of
                Nothing ->
                    ( { options | selectedSegment = Nothing }, [] )

                Just index ->
                    case List.Extra.getAt index options.namedSegments of
                        Nothing ->
                            ( { options | selectedSegment = Nothing }, [] )

                        Just segment ->
                            let
                                newOptions =
                                    { options
                                        | namedSegments = List.Extra.removeAt index options.namedSegments
                                        , selectedSegment = Nothing
                                    }
                            in
                            ( newOptions
                            , [ makePreview previewColour newOptions track ]
                            )

        ChangeName index newName ->
            case List.Extra.getAt index options.namedSegments of
                Nothing ->
                    ( { options | selectedSegment = Nothing }, [] )

                Just segment ->
                    let
                        updated =
                            { segment | name = newName }
                    in
                    ( { options
                        | namedSegments = List.Extra.updateAt index (always updated) options.namedSegments
                      }
                    , []
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

                newOptions =
                    addSegment newSegment options
            in
            ( newOptions
            , [ makePreview previewColour newOptions track ]
            )

        LandUseProximity distance ->
            let
                newOptions =
                    { options | landUseProximity = Just distance }
                        |> segmentsFromPlaces track
            in
            ( newOptions
            , [ makePreview previewColour newOptions track ]
            )

        EnableAutoSuggest enabled ->
            -- Add segments based on nearby Land Use names.
            if enabled then
                let
                    newOptions =
                        { options
                            | landUseProximity =
                                case enabled of
                                    True ->
                                        Just <| Length.meters 50

                                    False ->
                                        Nothing
                        }
                            |> segmentsFromPlaces track
                in
                ( newOptions
                , [ makePreview previewColour newOptions track ]
                )

            else
                -- when disabling, do npt clear the iist
                let
                    newOptions =
                        { options | landUseProximity = Nothing }
                in
                ( newOptions
                , [ makePreview previewColour newOptions track ]
                )

        TogglePreferCloser bool ->
            let
                newOptions =
                    { options | landUsePreferCloser = bool }
                        |> segmentsFromPlaces track
            in
            ( newOptions
            , [ makePreview previewColour newOptions track ]
            )


type alias SegmentCandidate =
    { name : String
    , place : EarthPoint
    , nearestTrackPointIndex : Int
    , distanceAway : Length.Length
    }


segmentsFromPlaces : TrackLoaded msg -> Options -> Options
segmentsFromPlaces track options =
    {-
       1. Filter names places within threshold.
       2. Sort by increasing or decreasing distance.
       3. Folding the segment list across the sorted places:
           3.1 Find nearest track point
           3.2 Derive segment start and end (formula TBD)
           3.3 If not overlapping existing segment, make new segment (also RGT rule check)
       4. Return track with new segment list.
    -}
    let
        retainedSegments =
            -- Always scrap previously auto-found segments.
            options.namedSegments
                |> List.filter (\seg -> seg.createMode == ManualSegment)

        withinThreshold : Length.Length -> SegmentCandidate -> Bool
        withinThreshold threshold candidate =
            candidate.distanceAway |> Quantity.lessThanOrEqualTo threshold

        makeCandidate ( name, place ) =
            let
                nearestTrackPointIndex =
                    DomainModel.nearestToEarthPoint place track.currentPosition track.trackTree track.leafIndex

                nearestTrackPoint =
                    DomainModel.earthPointFromIndex nearestTrackPointIndex track.trackTree
            in
            { name = name
            , place = place
            , nearestTrackPointIndex = nearestTrackPointIndex
            , distanceAway = Point3d.distanceFrom place.space nearestTrackPoint.space
            }

        sortMethod =
            if options.landUsePreferCloser then
                .distanceAway >> Length.inMeters

            else
                .distanceAway >> Quantity.negate >> Length.inMeters

        orderedCandidates =
            case options.landUseProximity of
                Just threshold ->
                    track.landUseData.places
                        |> Dict.toList
                        |> List.map makeCandidate
                        |> List.filter (withinThreshold threshold)
                        |> List.sortBy sortMethod

                Nothing ->
                    []

        padInterval padding interval =
            Interval.from
                (Interval.minValue interval |> Quantity.minus padding)
                (Interval.maxValue interval |> Quantity.plus padding)

        addSegmentIfNoConflict : SegmentCandidate -> List NamedSegment -> List NamedSegment
        addSegmentIfNoConflict candidate outputs =
            let
                newSegmentCentre =
                    DomainModel.distanceFromIndex candidate.nearestTrackPointIndex track.trackTree

                newSegmentHalfLength =
                    Quantity.half candidate.distanceAway
                        |> Quantity.max (Length.meters 50)

                startBuffer =
                    Interval.fromEndpoints ( Length.meters 0, Length.meters 120 )

                endBuffer =
                    Interval.from
                        (DomainModel.trueLength track.trackTree |> Quantity.minus (Length.meters 200))
                        (DomainModel.trueLength track.trackTree)

                ( idealStart, idealEnd ) =
                    ( newSegmentCentre |> Quantity.minus newSegmentHalfLength
                    , newSegmentCentre |> Quantity.plus newSegmentHalfLength
                    )

                actualStart =
                    DomainModel.distanceFromIndex
                        (DomainModel.indexFromDistanceRoundedDown idealStart track.trackTree)
                        track.trackTree

                actualEnd =
                    DomainModel.distanceFromIndex
                        (DomainModel.indexFromDistanceRoundedUp idealEnd track.trackTree)
                        track.trackTree

                newSegmentInterval =
                    Interval.fromEndpoints ( actualStart, actualEnd )

                intervalFrom segment =
                    -- Include RGT separation requirement here.
                    Interval.from segment.startDistance segment.endDistance
                        |> padInterval (Length.meters 50)

                hasIntersectionWith existing =
                    Interval.intersects
                        (intervalFrom existing)
                        newSegmentInterval
            in
            if
                List.any hasIntersectionWith outputs
                    || Interval.intersects newSegmentInterval startBuffer
                    || Interval.intersects newSegmentInterval endBuffer
            then
                outputs

            else
                { name = candidate.name
                , startDistance = actualStart
                , endDistance = actualEnd
                , createMode = AutoSegment
                , startOk = True
                , endOk = True
                }
                    :: outputs
    in
    { options
        | namedSegments =
            orderedCandidates
                |> List.foldl addSegmentIfNoConflict retainedSegments
                |> List.sortBy (.startDistance >> Length.inMeters)
    }



-- END
