module Tools.DirectionChanges exposing (DirectionChangeMode(..), Msg(..), Options, ResultMode(..), defaultOptions, toolId, toolStateChange, update, view, widenBend)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (GPXSource, PeteTree(..), RoadSection, asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import Length exposing (Meters)
import List.Extra
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import String.Interpolate
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Nudge
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, noTrackMessage, sliderThumb, useIcon)


toolId =
    "bends"


type alias Options =
    { threshold : Angle
    , singlePointBreaches : List ( Int, Angle )
    , bendBreaches : List ( List Int, Quantity Float Meters )
    , currentPointBreach : Int
    , currentBendBreach : Int
    , mode : DirectionChangeMode
    , radius : Length.Length
    , resultMode : ResultMode
    }


type ResultMode
    = ResultList
    | ResultNavigation


type DirectionChangeMode
    = DirectionChangeAbrupt
    | DirectionChangeWithRadius


defaultOptions : Options
defaultOptions =
    { threshold = Angle.degrees 120
    , singlePointBreaches = []
    , bendBreaches = []
    , currentPointBreach = 0
    , currentBendBreach = 0
    , mode = DirectionChangeAbrupt
    , radius = Length.meters 10.0
    , resultMode = ResultNavigation
    }


type Msg
    = ViewNext
    | ViewPrevious
    | SetCurrentPosition Int
    | SetThreshold Angle
    | SetRadius Length.Length
    | SetMode DirectionChangeMode
    | SetResultMode ResultMode
    | Autofix
    | NudgeOne
    | DisplayInfo String String


findDirectionChanges : Options -> PeteTree -> Options
findDirectionChanges options tree =
    -- This function called when track changes, or we call it when threshold is changed.
    -- We search the tree.
    let
        helper : Int -> PeteTree -> List ( Int, Angle ) -> List ( Int, Angle )
        helper skip treeNode accum =
            case treeNode of
                Leaf _ ->
                    -- No, this doesn't apply at leaves.
                    accum

                Node node ->
                    let
                        thisNodeAngle =
                            Quantity.abs <|
                                Direction2d.angleFrom
                                    (node.left |> asRecord |> .directionAtEnd)
                                    (node.right |> asRecord |> .directionAtStart)

                        withThisNodeIfNeeded acc =
                            -- Is it at this node, or one of its children?
                            if thisNodeAngle |> Quantity.greaterThanOrEqualTo options.threshold then
                                ( skip + skipCount node.left, thisNodeAngle ) :: acc

                            else
                                acc
                    in
                    if
                        node.nodeContent.directionChangeMaximumAbs
                            |> Quantity.greaterThanOrEqualTo options.threshold
                    then
                        accum
                            |> helper (skip + skipCount node.left) node.right
                            |> helper skip node.left
                            |> withThisNodeIfNeeded

                    else
                        accum
    in
    case options.mode of
        DirectionChangeAbrupt ->
            { options
                | singlePointBreaches = helper 0 tree []
                , currentPointBreach = 0
            }

        DirectionChangeWithRadius ->
            { options
                | bendBreaches = findBendsWithRadius tree options
                , currentBendBreach = 0
            }


findBendsWithRadius : PeteTree -> Options -> List ( List Int, Quantity Float Meters )
findBendsWithRadius tree options =
    {-
       The UI says direction change and radius but we look for the given direction
       change over a track length of radius * direction change in radians.
    -}
    let
        windowLength =
            options.radius |> Quantity.multiplyBy (Angle.inRadians options.threshold)

        consumeLength :
            ( Length.Length, List ( Int, RoadSection ) )
            -> List ( Int, RoadSection )
            -> List ( Int, RoadSection )
        consumeLength ( runningLength, retainedRoads ) roads =
            if runningLength |> Quantity.greaterThanOrEqualTo windowLength then
                -- Got enough, but the accumulator is reversed bu consing.
                List.reverse retainedRoads

            else
                -- Not enough, keep this one and recurse if possible
                case roads of
                    [] ->
                        -- OK, no roads.
                        List.reverse retainedRoads

                    ( n, aRoad ) :: moreRoads ->
                        consumeLength
                            ( Quantity.plus runningLength aRoad.trueLength
                            , ( n, aRoad ) :: retainedRoads
                            )
                            moreRoads

        runningDirectionChange :
            RoadSection
            -> ( Int, List ( Int, RoadSection ), List ( List Int, Quantity Float Meters ) )
            -> ( Int, List ( Int, RoadSection ), List ( List Int, Quantity Float Meters ) )
        runningDirectionChange road ( newIndex, window, outputs ) =
            {-
               Think of this as a variable window function.
               This list contains recent road segments adding up the length given
               by angular change sought in radians times the radius.
               For each leaf we add to the window, remove excess length from window,
               assess turn across the window and if exceeds threshold, add to outputs.
            -}
            let
                newWindow =
                    consumeLength
                        ( Quantity.zero, [] )
                        (( newIndex, road ) :: window)

                turnDuringWindow =
                    radiansTurned newWindow

                radiansTurned : List ( Int, RoadSection ) -> Float
                radiansTurned sections =
                    case sections of
                        [] ->
                            0

                        [ _ ] ->
                            0

                        ( _, first ) :: ( n, second ) :: more ->
                            second.directionAtStart
                                |> Direction2d.angleFrom first.directionAtStart
                                |> Angle.inRadians
                                |> (+) (radiansTurned (( n, second ) :: more))
            in
            if abs turnDuringWindow >= Angle.inRadians options.threshold then
                -- NOTE: Adding 1 to the points, as these look like the ones that need a nudge.
                ( newIndex + 1
                , []
                , ( newIndex + 1 :: newIndex :: List.map Tuple.first window
                  , windowLength |> Quantity.divideBy turnDuringWindow
                  )
                    :: outputs
                )

            else
                ( newIndex + 1, newWindow, outputs )

        ( _, _, bendStarts ) =
            DomainModel.foldOverRoute
                runningDirectionChange
                tree
                ( 0, [], [] )
    in
    List.reverse bendStarts


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
                    findDirectionChanges options theTrack.trackTree
            in
            ( populatedOptions
            , actions populatedOptions colour theTrack
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "kinks" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    let
        undoInfo action =
            TrackLoaded.undoInfo action track
    in
    case msg of
        SetMode mode ->
            let
                newOptions =
                    { options
                        | mode = mode
                        , singlePointBreaches = []
                        , bendBreaches = []
                    }

                populatedOptions =
                    findDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions, actions populatedOptions previewColour track )

        SetResultMode mode ->
            let
                newOptions =
                    { options | resultMode = mode }
            in
            ( newOptions, [] )

        ViewNext ->
            case options.mode of
                DirectionChangeAbrupt ->
                    let
                        breachIndex =
                            min (List.length options.singlePointBreaches - 1) (options.currentPointBreach + 1)

                        newOptions =
                            { options | currentPointBreach = breachIndex }

                        ( position, _ ) =
                            Maybe.withDefault ( 0, Angle.degrees 0 ) <|
                                List.Extra.getAt breachIndex newOptions.singlePointBreaches
                    in
                    ( newOptions, [ SetCurrent position, MapCenterOnCurrent ] )

                DirectionChangeWithRadius ->
                    let
                        breachIndex =
                            min (List.length options.bendBreaches - 1) (options.currentBendBreach + 1)

                        newOptions =
                            { options | currentBendBreach = breachIndex }

                        ( points, _ ) =
                            Maybe.withDefault ( [ 0 ], Quantity.zero ) <|
                                List.Extra.getAt breachIndex newOptions.bendBreaches
                    in
                    case points of
                        [] ->
                            ( newOptions, [] )

                        position :: _ ->
                            ( newOptions, [ SetCurrent position, MapCenterOnCurrent ] )

        ViewPrevious ->
            case options.mode of
                DirectionChangeAbrupt ->
                    let
                        breachIndex =
                            max 0 (options.currentPointBreach - 1)

                        newOptions =
                            { options | currentPointBreach = breachIndex }

                        ( position, _ ) =
                            Maybe.withDefault ( 0, Angle.degrees 0 ) <|
                                List.Extra.getAt breachIndex newOptions.singlePointBreaches
                    in
                    ( newOptions, [ SetCurrent position, MapCenterOnCurrent ] )

                DirectionChangeWithRadius ->
                    let
                        breachIndex =
                            max 0 (options.currentBendBreach - 1)

                        newOptions =
                            { options | currentBendBreach = breachIndex }

                        ( points, _ ) =
                            Maybe.withDefault ( [ 0 ], Quantity.zero ) <|
                                List.Extra.getAt breachIndex newOptions.bendBreaches
                    in
                    case points of
                        [] ->
                            ( newOptions, [] )

                        position :: _ ->
                            ( newOptions, [ SetCurrent position, MapCenterOnCurrent ] )

        SetCurrentPosition position ->
            case options.mode of
                DirectionChangeAbrupt ->
                    ( options, [ SetCurrent position, MapCenterOnCurrent ] )

                DirectionChangeWithRadius ->
                    ( options, [ SetCurrent position, MapCenterOnCurrent ] )

        SetThreshold angle ->
            let
                newOptions =
                    { options | threshold = angle }

                populatedOptions =
                    findDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions
            , actions options previewColour track
            )

        SetRadius radius ->
            let
                newOptions =
                    { options | radius = radius }

                populatedOptions =
                    findDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions
            , actions options previewColour track
            )

        Autofix ->
            ( options
            , [ Actions.WithUndo (Actions.Autofix <| List.map Tuple.first options.singlePointBreaches)
              , Actions.Autofix <| List.map Tuple.first options.singlePointBreaches
              , TrackHasChanged
              ]
            )

        NudgeOne ->
            let
                ( points, estimatedRadius ) =
                    Maybe.withDefault ( [], Quantity.zero ) <|
                        List.Extra.getAt options.currentBendBreach options.bendBreaches

                desired =
                    if estimatedRadius |> Quantity.greaterThanOrEqualToZero then
                        options.radius

                    else
                        Quantity.negate options.radius
            in
            ( options
            , [ Actions.WithUndo <| Actions.WidenBend points (Quantity.minus desired estimatedRadius)
              , Actions.WidenBend points (Quantity.minus desired estimatedRadius)
              , TrackHasChanged
              ]
            )

        DisplayInfo id tag ->
            ( options, [ Actions.DisplayInfo id tag ] )


actions options previewColour track =
    [ ShowPreview
        { tag = "kinks"
        , shape = PreviewCircle
        , colour = previewColour
        , points =
            case options.mode of
                DirectionChangeAbrupt ->
                    TrackLoaded.buildPreview
                        (List.map Tuple.first options.singlePointBreaches)
                        track.trackTree

                DirectionChangeWithRadius ->
                    TrackLoaded.buildPreview
                        (List.concatMap Tuple.first options.bendBreaches)
                        track.trackTree
        }
    ]


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial msgWrapper options isTrack =
    let
        i18n =
            I18N.text location toolId

        commonButtons current =
            row [ centerX, spacing 10 ]
                [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                , Input.button
                    (buttonStylesWithTooltip below <| I18N.localisedString location toolId "prev")
                    { label = useIcon FeatherIcons.chevronLeft
                    , onPress = Just <| msgWrapper <| ViewPrevious
                    }
                , Input.button
                    (buttonStylesWithTooltip below <| I18N.localisedString location toolId "this")
                    { label = useIcon FeatherIcons.mousePointer
                    , onPress = Just <| msgWrapper <| SetCurrentPosition current
                    }
                , Input.button
                    (buttonStylesWithTooltip below <| I18N.localisedString location toolId "next")
                    { label = useIcon FeatherIcons.chevronRight
                    , onPress = Just <| msgWrapper <| ViewNext
                    }
                ]

        singlePointResultsNavigation breaches =
            case breaches of
                [] ->
                    el [ centerX, centerY ] <| i18n "none"

                _ :: _ ->
                    let
                        ( position, turn ) =
                            Maybe.withDefault ( 0, Angle.degrees 0 ) <|
                                List.Extra.getAt options.currentPointBreach options.singlePointBreaches
                    in
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId ".of.")
                                    [ String.fromInt (options.currentPointBreach + 1)
                                    , String.fromInt <| List.length options.singlePointBreaches
                                    , showAngle <| turn
                                    ]
                        , commonButtons position
                        ]

        bendResultsNavigation breaches =
            case breaches of
                [] ->
                    el [ centerX, centerY ] <| i18n "none"

                _ :: _ ->
                    let
                        ( window, radius ) =
                            Maybe.withDefault ( [ 0 ], Quantity.zero ) <|
                                List.Extra.getAt options.currentBendBreach options.bendBreaches

                        at =
                            Maybe.withDefault 0 <| List.head window
                    in
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId ".radius.")
                                    [ String.fromInt (options.currentBendBreach + 1)
                                    , String.fromInt <| List.length options.bendBreaches
                                    , showShortMeasure imperial (Quantity.abs radius)
                                    ]
                        , commonButtons at
                        ]

        singlePointLinkButton track point =
            Input.button (alignTop :: neatToolsBorder)
                { onPress = Just (msgWrapper <| SetCurrentPosition point)
                , label =
                    text <|
                        showLongMeasure imperial <|
                            DomainModel.distanceFromIndex point track
                }

        bendLinkButton track ( window, _ ) =
            case List.head window of
                Just point ->
                    singlePointLinkButton track point

                Nothing ->
                    none

        wrappedRowStyle breaches =
            -- Pain getting this wrapped row to look OK.
            [ scrollbarY
            , height <|
                px <|
                    clamp 32 300 <|
                        (List.length breaches // 3 * 24)
            , spacingXY 6 6
            , alignTop
            , padding 6
            ]
    in
    case isTrack of
        Just track ->
            let
                modeSelection =
                    Input.radioRow [ centerX, spacing 5 ]
                        { onChange = msgWrapper << SetMode
                        , options =
                            [ Input.option DirectionChangeAbrupt (i18n "usepoint")
                            , Input.option DirectionChangeWithRadius (i18n "useradius")
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

                angleSelection =
                    Input.slider
                        ViewPureStyles.shortSliderStyles
                        { onChange = Angle.degrees >> SetThreshold >> msgWrapper
                        , value = Angle.inDegrees options.threshold
                        , label =
                            Input.labelBelow [] <|
                                text <|
                                    String.Interpolate.interpolate
                                        (I18N.localisedString location toolId "change")
                                        [ String.fromInt <| round <| Angle.inDegrees options.threshold ]
                        , min = 15
                        , max = 170
                        , step = Just 1
                        , thumb = sliderThumb
                        }

                autofixButton =
                    if options.singlePointBreaches == [] || options.mode == DirectionChangeWithRadius then
                        none

                    else
                        row [ spacing 4 ]
                            [ none
                            , infoButton (msgWrapper <| DisplayInfo "bends" "autofix")
                            , Input.button
                                (alignTop :: neatToolsBorder)
                                { onPress = Just (msgWrapper Autofix)
                                , label = i18n "smooth"
                                }
                            ]

                bendButtonFix =
                    if options.bendBreaches == [] || options.mode == DirectionChangeAbrupt then
                        none

                    else
                        wrappedRow [ spacing 4 ]
                            [ none
                            , infoButton (msgWrapper <| DisplayInfo "bends" "widen")
                            , Input.button
                                (alignTop :: neatToolsBorder)
                                { onPress = Just (msgWrapper NudgeOne)
                                , label = i18n "adjust"
                                }

                            --    }
                            ]
            in
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column [ padding 4, spacing 6, width fill ]
                    [ modeSelection
                    , angleSelection
                    , if options.mode == DirectionChangeWithRadius then
                        Input.slider
                            ViewPureStyles.shortSliderStyles
                            { onChange = Length.meters >> SetRadius >> msgWrapper
                            , value = Length.inMeters options.radius
                            , label =
                                Input.labelBelow [] <|
                                    text <|
                                        String.Interpolate.interpolate
                                            (I18N.localisedString location toolId "radius")
                                            [ showShortMeasure imperial options.radius ]
                            , min = 4.0
                            , max = 100.0
                            , step = Just 1
                            , thumb = sliderThumb
                            }

                      else
                        none
                    , el [ centerX ] autofixButton
                    , el [ centerX ] bendButtonFix
                    , el [ centerX ] resultModeSelection
                    , case ( options.mode, options.resultMode ) of
                        ( DirectionChangeAbrupt, ResultNavigation ) ->
                            singlePointResultsNavigation options.singlePointBreaches

                        ( DirectionChangeAbrupt, ResultList ) ->
                            wrappedRow (wrappedRowStyle options.singlePointBreaches) <|
                                List.map
                                    (Tuple.first >> singlePointLinkButton track.trackTree)
                                    options.singlePointBreaches

                        ( DirectionChangeWithRadius, ResultNavigation ) ->
                            bendResultsNavigation options.bendBreaches

                        ( DirectionChangeWithRadius, ResultList ) ->
                            wrappedRow (wrappedRowStyle options.bendBreaches) <|
                                List.map
                                    (bendLinkButton track.trackTree)
                                    options.bendBreaches
                    ]

        Nothing ->
            noTrackMessage location


widenBend :
    List Int
    -> Quantity Float Meters
    -> TrackLoaded msg
    -> Maybe PeteTree
widenBend points adjustment track =
    Tools.Nudge.widenBendHelper points adjustment track
