module Tools.GradientProblems exposing (GradientProblem(..), Msg(..), Options, ResultMode(..), defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import CommonToolStyles exposing (noTrackMessage)
import DomainModel exposing (PeteTree, RoadSection, skipCount)
import Element exposing (..)
import Element.Input as Input
import FeatherIcons
import Length
import List.Extra
import PreviewData exposing (PreviewShape(..))
import Quantity
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showLongMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, sliderThumb, useIcon)


toolId =
    "gradients"


type GradientProblem
    = AbruptChange
    | SteepClimb
    | SteepDescent
    | Flats


type alias Options =
    { threshold : Float
    , breaches : List ( Int, Float )
    , currentBreach : Int
    , mode : GradientProblem
    , resultMode : ResultMode
    }


type ResultMode
    = ResultList
    | ResultNavigation


defaultOptions =
    { threshold = 10.0
    , breaches = []
    , currentBreach = 0
    , mode = AbruptChange
    , resultMode = ResultNavigation
    }


type Msg
    = ViewNext
    | ViewPrevious
    | PositionMarkerAtBreach Int Int
    | SetThreshold Float
    | SetMode GradientProblem
    | SetResultMode ResultMode
    | Autofix
    | DisplayInfo String String


findAbruptGradientChanges : Options -> PeteTree -> Options
findAbruptGradientChanges options tree =
    -- This function called when track changes, or we call it when threshold is changed.
    -- We search the tree. At worst, fold over the whole darn tree. Optimize if needed.
    let
        foldFn :
            RoadSection
            -> ( Int, Maybe RoadSection, List ( Int, Float ) )
            -> ( Int, Maybe RoadSection, List ( Int, Float ) )
        foldFn road ( index, previousIfAny, outputs ) =
            case previousIfAny of
                Nothing ->
                    ( index + 1, Just road, outputs )

                Just previousRoad ->
                    let
                        change =
                            abs (road.gradientAtStart - previousRoad.gradientAtStart)
                    in
                    if change > options.threshold then
                        ( index + 1, Just road, ( index, change ) :: outputs )

                    else
                        ( index + 1, Just road, outputs )

        ( _, _, breaches ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount tree)
                (always Nothing)
                0
                tree
                foldFn
                ( 0, Nothing, [] )
    in
    { options
        | breaches = List.reverse breaches
        , currentBreach = 0
    }


findSteepClimbs : Options -> PeteTree -> Options
findSteepClimbs options tree =
    -- This function called when track changes, or we call it when threshold is changed.
    -- We search the tree. At worst, fold over the whole darn tree. Optimize if needed.
    let
        foldFn :
            RoadSection
            -> ( Int, List ( Int, Float ) )
            -> ( Int, List ( Int, Float ) )
        foldFn road ( index, outputs ) =
            if road.gradientAtStart > options.threshold then
                ( index + 1, ( index, road.gradientAtStart ) :: outputs )

            else
                ( index + 1, outputs )

        ( _, breaches ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount tree)
                (always Nothing)
                0
                tree
                foldFn
                ( 0, [] )
    in
    { options
        | breaches = List.reverse breaches
        , currentBreach = 0
    }


findSteepDescents : Options -> PeteTree -> Options
findSteepDescents options tree =
    let
        foldFn :
            RoadSection
            -> ( Int, List ( Int, Float ) )
            -> ( Int, List ( Int, Float ) )
        foldFn road ( index, outputs ) =
            if -road.gradientAtStart > options.threshold then
                ( index + 1, ( index, road.gradientAtStart ) :: outputs )

            else
                ( index + 1, outputs )

        ( _, breaches ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount tree)
                (always Nothing)
                0
                tree
                foldFn
                ( 0, [] )
    in
    { options
        | breaches = List.reverse breaches
        , currentBreach = 0
    }


type alias FlatFinderFoldState =
    -- Too much state for a tuple.
    { lastPointIndex : Int
    , captureStart : Maybe Int
    , previousAltitude : Length.Length
    , outputs : List ( Int, Float )
    }


findFlats : Options -> PeteTree -> Options
findFlats options tree =
    -- Request from Falk Levien, to identify contiguous points with no change of altitude.
    -- These seem to arise from use of LIDAR data.
    -- Will (naughtily) use the Float in the tuple to hold the index of the final point in the run.
    let
        initialFoldState : FlatFinderFoldState
        initialFoldState =
            { lastPointIndex = 0
            , captureStart = Nothing
            , previousAltitude = DomainModel.gpxPointFromIndex 0 tree |> .altitude
            , outputs = []
            }

        foldFn :
            RoadSection
            -> FlatFinderFoldState
            -> FlatFinderFoldState
        foldFn road foldState =
            let
                thisPointAltitude =
                    road.sourceData |> Tuple.second |> .altitude

                isFlat =
                    Quantity.equalWithin Length.micron
                        thisPointAltitude
                        foldState.previousAltitude
            in
            case ( foldState.captureStart, isFlat ) of
                ( Just start, True ) ->
                    -- We are capturing, continue to capture.
                    { foldState | lastPointIndex = foldState.lastPointIndex + 1 }

                ( Just start, False ) ->
                    -- We are capturing, but have now found the end of the run
                    { foldState
                        | lastPointIndex = foldState.lastPointIndex + 1
                        , captureStart = Nothing
                        , outputs = ( start, toFloat foldState.lastPointIndex ) :: foldState.outputs
                        , previousAltitude = thisPointAltitude
                    }

                ( Nothing, True ) ->
                    -- We must start capture from previous point
                    { foldState
                        | lastPointIndex = foldState.lastPointIndex + 1
                        , captureStart = Just <| foldState.lastPointIndex
                    }

                ( Nothing, False ) ->
                    -- Not capturing, no need to start
                    { foldState
                        | lastPointIndex = foldState.lastPointIndex + 1
                        , previousAltitude = thisPointAltitude
                    }

        finalFoldState =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount tree)
                (always Nothing)
                0
                tree
                foldFn
                initialFoldState
    in
    { options
        | breaches = List.reverse finalFoldState.outputs
        , currentBreach = 0
        , threshold = 0
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
            -- Make sure we have up to date breaches and preview is shown.
            let
                populatedOptions =
                    case options.mode of
                        AbruptChange ->
                            findAbruptGradientChanges options theTrack.trackTree

                        SteepClimb ->
                            findSteepClimbs options theTrack.trackTree

                        SteepDescent ->
                            findSteepDescents options theTrack.trackTree

                        Flats ->
                            findFlats options theTrack.trackTree
            in
            ( populatedOptions
            , [ ShowPreview
                    { tag = "ridge"
                    , shape = PreviewCircle
                    , colour = colour
                    , points =
                        TrackLoaded.buildPreview
                            (List.map Tuple.first populatedOptions.breaches)
                            theTrack.trackTree
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( { options | breaches = [] }
            , [ HidePreview "ridge" ]
            )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    let
        actions opts track =
            [ ShowPreview
                { tag = "ridge"
                , shape = PreviewCircle
                , colour = previewColour
                , points =
                    TrackLoaded.buildPreview
                        (List.map Tuple.first opts.breaches)
                        track.trackTree
                }
            ]

        populateOptions opts track =
            case opts.mode of
                AbruptChange ->
                    findAbruptGradientChanges opts track.trackTree

                SteepClimb ->
                    findSteepClimbs opts track.trackTree

                SteepDescent ->
                    findSteepDescents opts track.trackTree

                Flats ->
                    findFlats opts track.trackTree

        moveMarkers orange purple =
            case options.mode of
                Flats ->
                    -- Place purple at end
                    [ SetCurrent orange
                    , SetMarker <| Just purple
                    , PointerChange
                    ]

                _ ->
                    [ SetCurrent orange ]
    in
    case msg of
        ViewNext ->
            let
                breachIndex =
                    min (List.length options.breaches - 1) (options.currentBreach + 1)

                newOptions =
                    { options | currentBreach = breachIndex }

                ( orange, purple ) =
                    Maybe.withDefault ( 0, 0 ) <|
                        List.Extra.getAt breachIndex newOptions.breaches
            in
            ( newOptions, moveMarkers orange (truncate purple) )

        ViewPrevious ->
            let
                breachIndex =
                    max 0 (options.currentBreach - 1)

                newOptions =
                    { options | currentBreach = breachIndex }

                ( orange, purple ) =
                    Maybe.withDefault ( 0, 0 ) <|
                        List.Extra.getAt breachIndex newOptions.breaches
            in
            case options.mode of
                Flats ->
                    -- Place purple at end
                    ( options
                    , moveMarkers orange (truncate purple)
                    )

                _ ->
                    ( newOptions, [ SetCurrent orange ] )

        PositionMarkerAtBreach orange purple ->
            ( options, moveMarkers orange purple )

        SetThreshold value ->
            let
                newOptions =
                    { options | threshold = value, breaches = [] }
            in
            case hasTrack of
                Just track ->
                    let
                        populatedOptions =
                            populateOptions newOptions track
                    in
                    ( populatedOptions
                    , actions populatedOptions track
                    )

                Nothing ->
                    ( newOptions, [] )

        SetResultMode mode ->
            ( { options | resultMode = mode }, [] )

        SetMode mode ->
            let
                newOptions =
                    { options | mode = mode, breaches = [] }
            in
            case hasTrack of
                Just track ->
                    let
                        populatedOptions =
                            populateOptions newOptions track
                    in
                    ( populatedOptions
                    , actions populatedOptions track
                    )

                Nothing ->
                    ( newOptions, [] )

        Autofix ->
            case hasTrack of
                Just track ->
                    ( options
                    , [ Actions.WithUndo (Actions.Autofix <| List.map Tuple.first options.breaches)
                      , Actions.Autofix <| List.map Tuple.first options.breaches
                      , TrackHasChanged
                      ]
                    )

                Nothing ->
                    ( options, [] )

        DisplayInfo id tag ->
            ( options, [ Actions.DisplayInfo id tag ] )


view : SystemSettings -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view settings msgWrapper options isTrack =
    let
        i18n =
            I18N.text settings.location toolId

        resultsNavigation =
            case options.breaches of
                [] ->
                    el [ centerX, centerY ] <| i18n "none"

                _ :: _ ->
                    let
                        ( position, turn ) =
                            Maybe.withDefault ( 0, 0 ) <|
                                List.Extra.getAt options.currentBreach options.breaches
                    in
                    column (CommonToolStyles.toolContentBoxStyle settings)
                        [ el [ centerX ] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString settings.location toolId ".of.")
                                    [ String.fromInt (options.currentBreach + 1)
                                    , String.fromInt <| List.length options.breaches
                                    , showDecimal2 turn
                                    ]
                        , row [ centerX, spacing 10 ]
                            [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                            , Input.button
                                (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "prev")
                                { label = useIcon FeatherIcons.chevronLeft
                                , onPress = Just <| msgWrapper <| ViewPrevious
                                }
                            , Input.button
                                (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "this")
                                { label = useIcon FeatherIcons.mousePointer
                                , onPress = Just <| msgWrapper <| PositionMarkerAtBreach position (truncate turn)
                                }
                            , Input.button
                                (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "next")
                                { label = useIcon FeatherIcons.chevronRight
                                , onPress = Just <| msgWrapper <| ViewNext
                                }
                            ]
                        ]

        linkButton track ( orange, purple ) =
            Input.button (alignTop :: neatToolsBorder)
                { onPress = Just (msgWrapper <| PositionMarkerAtBreach orange (truncate purple))
                , label =
                    text <|
                        showLongMeasure settings.imperial <|
                            DomainModel.distanceFromIndex orange track
                }
    in
    case isTrack of
        Just track ->
            let
                modeSelection =
                    Input.radio [ centerX, spacing 5 ]
                        { onChange = msgWrapper << SetMode
                        , options =
                            [ Input.option AbruptChange (i18n "usepoint")
                            , Input.option SteepClimb (i18n "climbs")
                            , Input.option SteepDescent (i18n "descents")
                            , Input.option Flats (i18n "flats")
                            ]
                        , selected = Just options.mode
                        , label = Input.labelHidden "Mode"
                        }

                resultModeSelection =
                    Input.radioRow [ centerX, spacing 5 ]
                        { onChange = msgWrapper << SetResultMode
                        , options =
                            [ Input.option ResultNavigation (i18n "summary")
                            , Input.option ResultList (i18n "list")
                            ]
                        , selected = Just options.resultMode
                        , label = Input.labelHidden "Results mode"
                        }

                thresholdSlider =
                    Input.slider
                        ViewPureStyles.shortSliderStyles
                        { onChange = SetThreshold >> msgWrapper
                        , value = options.threshold
                        , label = Input.labelHidden "Threshold"
                        , min = 1
                        , max = 20
                        , step = Just 1
                        , thumb = sliderThumb
                        }

                autofixButton =
                    if options.breaches == [] then
                        none

                    else
                        row [ spacing 4 ]
                            [ none
                            , infoButton (msgWrapper <| DisplayInfo "gradients" "autofix")
                            , Input.button
                                (alignTop :: neatToolsBorder)
                                { onPress = Just (msgWrapper Autofix)
                                , label = i18n "smooth"
                                }
                            ]

                unlessFlatMode x =
                    if options.mode == Flats then
                        el [ paddingXY 0 2 ] none

                    else
                        x
            in
            el (CommonToolStyles.toolContentBoxStyle settings) <|
                column [ centerX, padding 4, spacing 6 ]
                    [ el [ centerX ] modeSelection
                    , unlessFlatMode <| el [ centerX ] thresholdSlider
                    , unlessFlatMode <|
                        el [ centerX ] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString settings.location toolId "threshold")
                                    [ showDecimal2 options.threshold ]
                    , unlessFlatMode <| el [ centerX ] autofixButton
                    , el [ centerX ] resultModeSelection
                    , case options.resultMode of
                        ResultNavigation ->
                            resultsNavigation

                        ResultList ->
                            wrappedRow
                                [ scrollbarY
                                , height <|
                                    px <|
                                        clamp 32 300 <|
                                            (List.length options.breaches // 3 * 24)
                                , spacingXY 6 6
                                , padding 6
                                , alignTop
                                ]
                            <|
                                List.map
                                    (linkButton track.trackTree)
                                    options.breaches
                    ]

        Nothing ->
            noTrackMessage settings
