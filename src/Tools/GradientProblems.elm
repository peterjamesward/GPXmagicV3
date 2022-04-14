module Tools.GradientProblems exposing (..)

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
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showDecimal2, showLongMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, noTrackMessage, sliderThumb, useIcon)


toolId =
    "gradients"


type GradientProblem
    = AbruptChange
    | SteepClimb
    | SteepDescent


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
    | SetCurrentPosition Int
    | SetThreshold Float
    | SetMode GradientProblem
    | SetResultMode ResultMode
    | Autofix
    | DisplayInfo String String


findAbruptDirectionChanges : Options -> PeteTree -> Options
findAbruptDirectionChanges options tree =
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
            if (0.0 - road.gradientAtStart) > options.threshold then
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
                    findAbruptDirectionChanges options theTrack.trackTree
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
                    findAbruptDirectionChanges opts track.trackTree

                SteepClimb ->
                    findSteepClimbs opts track.trackTree

                SteepDescent ->
                    findSteepDescents opts track.trackTree
    in
    case msg of
        ViewNext ->
            let
                breachIndex =
                    min (List.length options.breaches - 1) (options.currentBreach + 1)

                newOptions =
                    { options | currentBreach = breachIndex }

                ( position, _ ) =
                    Maybe.withDefault ( 0, 0 ) <|
                        List.Extra.getAt breachIndex newOptions.breaches
            in
            ( newOptions, [ SetCurrent position ] )

        ViewPrevious ->
            let
                breachIndex =
                    max 0 (options.currentBreach - 1)

                newOptions =
                    { options | currentBreach = breachIndex }

                ( position, _ ) =
                    Maybe.withDefault ( 0, 0 ) <|
                        List.Extra.getAt breachIndex newOptions.breaches
            in
            ( newOptions, [ SetCurrent position ] )

        SetCurrentPosition position ->
            ( options, [ SetCurrent position ] )

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
            ( options
            , [ Actions.Autofix <| List.map Tuple.first options.breaches
              , TrackHasChanged
              ]
            )

        DisplayInfo id tag ->
            ( options, [ Actions.DisplayInfo id tag ] )


view : I18NOptions.Options -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial msgWrapper options isTrack =
    let
        modeSelection =
            Input.radio [ centerX, spacing 5 ]
                { onChange = msgWrapper << SetMode
                , options =
                    [ Input.option AbruptChange (text "Abrupt changes")
                    , Input.option SteepClimb (text "Steep climbs")
                    , Input.option SteepDescent (text "Steep descents")
                    ]
                , selected = Just options.mode
                , label = Input.labelHidden "Mode"
                }

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

        thresholdSlider =
            Input.slider
                ViewPureStyles.shortSliderStyles
                { onChange = SetThreshold >> msgWrapper
                , value = options.threshold
                , label = Input.labelHidden "Threshold"
                , min = 3
                , max = 20
                , step = Just 1
                , thumb = sliderThumb
                }

        resultsNavigation =
            case options.breaches of
                [] ->
                    el [ centerX, centerY ] <| text "None found"

                a :: b ->
                    let
                        ( position, turn ) =
                            Maybe.withDefault ( 0, 0 ) <|
                                List.Extra.getAt options.currentBreach options.breaches
                    in
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.fromInt (options.currentBreach + 1)
                                    ++ " of "
                                    ++ (String.fromInt <| List.length options.breaches)
                                    ++ " is "
                                    ++ showDecimal2 turn
                                    ++ "º"
                        , row [ centerX, spacing 10 ]
                            [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                            , Input.button
                                (buttonStylesWithTooltip below "Move to previous")
                                { label = useIcon FeatherIcons.chevronLeft
                                , onPress = Just <| msgWrapper <| ViewPrevious
                                }
                            , Input.button
                                (buttonStylesWithTooltip below "Centre view on this issue")
                                { label = useIcon FeatherIcons.mousePointer
                                , onPress = Just <| msgWrapper <| SetCurrentPosition position
                                }
                            , Input.button
                                (buttonStylesWithTooltip below "Move to next")
                                { label = useIcon FeatherIcons.chevronRight
                                , onPress = Just <| msgWrapper <| ViewNext
                                }
                            ]
                        ]

        linkButton track point =
            Input.button (alignTop :: neatToolsBorder)
                { onPress = Just (msgWrapper <| SetCurrentPosition point)
                , label =
                    text <|
                        showLongMeasure imperial <|
                            DomainModel.distanceFromIndex point track
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
                        , label = text "Smooth these points"
                        }
                    ]
    in
    case isTrack of
        Just track ->
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column [ centerX, padding 4, spacing 6 ]
                    [ el [ centerX ] modeSelection
                    , el [ centerX ] thresholdSlider
                    , el [ centerX ] <|
                        text <|
                            "Threshold "
                                ++ showDecimal2 options.threshold
                                ++ "%"
                    , el [ centerX ] autofixButton
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
                                    (Tuple.first >> linkButton track.trackTree)
                                    options.breaches
                    ]

        Nothing ->
            noTrackMessage location
