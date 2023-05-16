module Tools.DirectionChanges exposing (DirectionChangeMode(..), Msg(..), Options, ResultMode(..), defaultOptions, toolId, toolStateChange, update, view, widenBend)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Circle2d
import CommonToolStyles exposing (noTrackMessage)
import Direction2d exposing (Direction2d)
import DomainModel exposing (GPXSource, PeteTree(..), RoadSection, asRecord, skipCount)
import Element exposing (..)
import Element.Input as Input
import FeatherIcons
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import Set exposing (Set)
import SketchPlane3d
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18N as I18N
import Tools.Nudge
import TrackLoaded exposing (TrackLoaded)
import Triangle2d
import UtilsForViews exposing (showAngle, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, sliderThumb, useIcon)


toolId =
    "bends"


type alias Options =
    { threshold : Angle
    , singlePointBreaches : List ( Int, Angle )
    , bendBreaches : List (Set Int)
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


type BendWindow
    = NoRoads
    | OneRoad RoadSection
    | TwoRoads RoadSection RoadSection
    | ThreeRoads RoadSection RoadSection RoadSection


findBendsWithRadius : PeteTree -> Options -> List (Set Int)
findBendsWithRadius tree options =
    -- looking for two adjacent points where the direction change is in the same direction
    -- and the total change divided into the length is less than the radius specified.
    -- We can quite neatly do this with a fold over the track that repeatedly examines
    -- three neighbouring road sections.
    -- Where we find a "run" of such points we put them in a set, as part of the same "bend".
    -- It is a list of these "bend points" that we return.
    let
        collectBendPoints :
            RoadSection
            -> ( BendWindow, List (Set Int) )
            -> ( BendWindow, List (Set Int) )
        collectBendPoints road ( state, outputs ) =
            case state of
                NoRoads ->
                    ( OneRoad road, outputs )

                OneRoad roadOne ->
                    ( TwoRoads roadOne road, outputs )

                TwoRoads roadOne roadTwo ->
                    ( ThreeRoads roadOne roadTwo road, outputs )

                ThreeRoads roadOne roadTwo roadThree ->
                    --TODO: Check same directions and total change (in radians), is curvature excessive?
                    --Note: Avoid division.
                    ( state, outputs )

        ( finalState, bendPoints ) =
            DomainModel.foldOverRoute
                collectBendPoints
                tree
                ( NoRoads, [] )

        --TODO: Mop up final state.
    in
    List.reverse bendPoints


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
                    in
                    case newOptions.bendBreaches of
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
                    in
                    case newOptions.bendBreaches of
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
                        options.bendBreaches
                        track.trackTree
        }
    ]


view : SystemSettings -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view settings msgWrapper options isTrack =
    let
        i18n =
            I18N.text settings.location toolId

        commonButtons current =
            row (CommonToolStyles.toolContentBoxStyle settings)
                [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                , Input.button
                    (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "prev")
                    { label = useIcon FeatherIcons.chevronLeft
                    , onPress = Just <| msgWrapper <| ViewPrevious
                    }
                , Input.button
                    (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "this")
                    { label = useIcon FeatherIcons.mousePointer
                    , onPress = Just <| msgWrapper <| SetCurrentPosition current
                    }
                , Input.button
                    (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "next")
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
                                    (I18N.localisedString settings.location toolId ".of.")
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
                    case List.Extra.getAt options.currentBendBreach options.bendBreaches of
                        Just at ->
                            column [ spacing 4, centerX ]
                                [ el [ centerX ] <|
                                    text <|
                                        String.Interpolate.interpolate
                                            (I18N.localisedString settings.location toolId ".radius.")
                                            [ String.fromInt (options.currentBendBreach + 1)
                                            , String.fromInt <| List.length options.bendBreaches
                                            ]
                                , commonButtons at
                                ]

                        Nothing ->
                            none

        singlePointLinkButton track point =
            Input.button (alignTop :: neatToolsBorder)
                { onPress = Just (msgWrapper <| SetCurrentPosition point)
                , label =
                    text <|
                        showLongMeasure settings.imperial <|
                            DomainModel.distanceFromIndex point track
                }

        bendLinkButton track point =
            singlePointLinkButton track point

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

                radiusSelection =
                    Input.slider
                        ViewPureStyles.shortSliderStyles
                        { onChange = Length.meters >> SetRadius >> msgWrapper
                        , value = Length.inMeters options.radius
                        , label =
                            Input.labelBelow [] <|
                                text <|
                                    String.Interpolate.interpolate
                                        (I18N.localisedString settings.location toolId "radius")
                                        [ showShortMeasure settings.imperial options.radius ]
                        , min = 4.0
                        , max = 100.0
                        , step = Just 1
                        , thumb = sliderThumb
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
                                        (I18N.localisedString settings.location toolId "change")
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
            in
            el (CommonToolStyles.toolContentBoxStyle settings) <|
                column [ padding 4, spacing 6, width fill ]
                    [ modeSelection
                    , if options.mode == DirectionChangeWithRadius then
                        radiusSelection

                      else
                        angleSelection
                    , el [ centerX ] autofixButton
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
            noTrackMessage settings


widenBend :
    List Int
    -> Quantity Float Meters
    -> TrackLoaded msg
    -> TrackLoaded msg
widenBend points adjustment track =
    case Tools.Nudge.widenBendHelper points adjustment track of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track
