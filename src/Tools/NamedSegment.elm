module Tools.NamedSegment exposing (..)

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the track points, so we can traverse sections
-- of track points multiple times and in each direction.

import Actions exposing (ToolAction)
import Dict
import DomainModel exposing (RoadSection)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.ChinesePalette
import Length exposing (Meters)
import List.Extra
import Quantity exposing (Quantity)
import String.Interpolate
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.NamedSegmentOptions exposing (NamedSegment, Options)
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
    }


type Msg
    = NoOp
    | SelectSegment Int
    | UpdateSegment
    | ChangeName Int String
    | DeleteSegment
    | CreateSegment
    | EnableAutoSuggest Bool
    | LandUseProximity Length.Length
    | DisplayInfo String String


initialise : List NamedSegment -> Options
initialise segments =
    { namedSegments = segments
    , selectedSegment = Nothing
    , landUseProximity = Nothing
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


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view location imperial wrapper options track =
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
                                        el (dataStyles (Just i == options.selectedSegment)) <|
                                            text <|
                                                showLongMeasure imperial t.startDistance
                              }
                            , { header = none
                              , width = fillPortion 1
                              , view =
                                    \i t ->
                                        el (dataStyles (Just i == options.selectedSegment)) <|
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
                    none

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
                                , min = 10
                                , max = 1000
                                , step = Just 10
                                , thumb = sliderThumb
                                }
                        ]

        goodSeparation : List NamedSegment -> Bool
        goodSeparation segs =
            -- Note the list should be sorted beforehand!
            case segs of
                seg1 :: seg2 :: moreSegs ->
                    (seg1.startDistance |> Quantity.greaterThan (Length.meters 110))
                        && (seg1.endDistance
                                |> Quantity.plus (Length.meters 50)
                                |> Quantity.lessThan seg2.startDistance
                           )
                        && goodSeparation (seg2 :: moreSegs)

                [ lastSeg ] ->
                    (lastSeg.startDistance |> Quantity.greaterThan (Length.meters 110))
                        && (lastSeg.endDistance
                                |> Quantity.plus (Length.meters 200)
                                |> Quantity.lessThan (DomainModel.trueLength track.trackTree)
                           )

                [] ->
                    True

        overlapWarning =
            if goodSeparation options.namedSegments then
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
                                    }
                            in
                            ( { options
                                | namedSegments =
                                    options.namedSegments
                                        |> List.Extra.updateAt index (always updated)
                                        |> List.sortBy (.startDistance >> Length.inMeters)
                              }
                            , []
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
                            ( { options
                                | namedSegments = List.Extra.removeAt index options.namedSegments
                                , selectedSegment = Nothing
                              }
                            , []
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
                    }
            in
            ( addSegment newSegment options
            , []
            )

        LandUseProximity distance ->
            ( { options | landUseProximity = Just distance }
            , []
            )

        EnableAutoSuggest enabled ->
            -- Add segments based on nearby Land Use names.
            ( { options
                | landUseProximity =
                    case enabled of
                        True ->
                            Just <| Length.meters 50

                        False ->
                            Nothing
              }
            , []
            )



-- END
