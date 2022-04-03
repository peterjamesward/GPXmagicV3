module Tools.DirectionChanges exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (labelHidden)
import FeatherIcons
import FlatColors.ChinesePalette
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import ToolTip exposing (buttonStylesWithTooltip, myTooltip, tooltip)
import Tools.Nudge
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showDecimal0, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, noTrackMessage, sliderThumb, useIcon)


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
    | NudgeAll
    | DisplayInfo String String


toolID : String
toolID =
    "bends"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Bend problems" )
        , ( "info", """Find points where the road direction changes significantly, or find
sections of track that may be a bend with a small radius.

From here, you can jump directly to the sections of track and use other tools to fix the problems.""" )
        , ( "autofix", """Smooth each of these individually using the single point _Smooth with arcs_. Use that
tool to change the number of points that are added to smooth each point.

You should use this only for trivial fixes; there are better tools for smoothing
serious issues. This tool can even make things worse.""" )
        , ( "locate", """These buttons will move the Orange pointer through the list of issues.

**Note**: this will only centre the views which have the padlock closed.""" )
        , ( "widen", """Nudge the points on the bend(s) outwards to increase the radius.

You may get better results from using the _Smooth with Arcs_ or _Radiused Bends_ tools.""" )
        ]
    )


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
    --TODO: Add correction needed to the outputs.
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

                        [ onlyOne ] ->
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
                , ( newIndex :: List.map Tuple.first window |> List.map add1
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
                    ( newOptions, [ SetCurrent position ] )

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
                            ( newOptions, [ SetCurrent position ] )

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
                    ( newOptions, [ SetCurrent position ] )

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
                            ( newOptions, [ SetCurrent position ] )

        SetCurrentPosition position ->
            case options.mode of
                DirectionChangeAbrupt ->
                    ( options, [ SetCurrent position ] )

                DirectionChangeWithRadius ->
                    ( options, [ SetCurrent position ] )

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
            , [ Actions.Autofix <| List.map Tuple.first options.singlePointBreaches
              , TrackHasChanged
              ]
            )

        NudgeOne ->
            let
                ( points, estimatedRadius ) =
                    Maybe.withDefault ( [], Quantity.zero ) <|
                        List.Extra.getAt options.currentBendBreach options.bendBreaches
            in
            ( options
            , [ Actions.WidenBend points (Quantity.minus options.radius estimatedRadius)
              , TrackHasChanged
              ]
            )

        NudgeAll ->
            ( options
            , []
            )

        DisplayInfo id tag ->
            ( options, [ Actions.DisplayInfo id tag ] )


add1 x =
    x + 1


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


view : Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view imperial msgWrapper options isTrack =
    let
        modeSelection =
            Input.radioRow [ centerX, spacing 5 ]
                { onChange = msgWrapper << SetMode
                , options =
                    [ Input.option DirectionChangeAbrupt (text "At point")
                    , Input.option DirectionChangeWithRadius (text "With radius")
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

        angleSelection =
            Input.slider
                ViewPureStyles.shortSliderStyles
                { onChange = Angle.degrees >> SetThreshold >> msgWrapper
                , value = Angle.inDegrees options.threshold
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Direction change "
                                ++ (String.fromInt <| round <| Angle.inDegrees options.threshold)
                                ++ "º"
                , min = 30
                , max = 170
                , step = Just 1
                , thumb = sliderThumb
                }

        radiusSelection =
            Input.slider
                ViewPureStyles.shortSliderStyles
                { onChange = Length.meters >> SetRadius >> msgWrapper
                , value = Length.inMeters options.radius
                , label =
                    Input.labelBelow [] <|
                        text <|
                            "Radius "
                                ++ showShortMeasure imperial options.radius
                , min = 4.0
                , max = 50.0
                , step = Just 1
                , thumb = sliderThumb
                }

        commonButtons current =
            row [ centerX, spacing 10 ]
                [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                , Input.button
                    (buttonStylesWithTooltip below "Move to previous")
                    { label = useIcon FeatherIcons.chevronLeft
                    , onPress = Just <| msgWrapper <| ViewPrevious
                    }
                , Input.button
                    (buttonStylesWithTooltip below "Move pointer to this issue\n(Is the padlock on?)")
                    { label = useIcon FeatherIcons.mousePointer
                    , onPress = Just <| msgWrapper <| SetCurrentPosition current
                    }
                , Input.button
                    (buttonStylesWithTooltip below "Move to next")
                    { label = useIcon FeatherIcons.chevronRight
                    , onPress = Just <| msgWrapper <| ViewNext
                    }
                ]

        singlePointResultsNavigation breaches =
            case breaches of
                [] ->
                    el [ centerX, centerY ] <| text "None found"

                a :: b ->
                    let
                        ( position, turn ) =
                            Maybe.withDefault ( 0, Angle.degrees 0 ) <|
                                List.Extra.getAt options.currentPointBreach options.singlePointBreaches
                    in
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.fromInt (options.currentPointBreach + 1)
                                    ++ " of "
                                    ++ (String.fromInt <| List.length options.singlePointBreaches)
                                    ++ ", "
                                    ++ (showAngle <| turn)
                                    ++ "º"
                        , commonButtons position
                        ]

        bendResultsNavigation breaches =
            case breaches of
                [] ->
                    el [ centerX, centerY ] <| text "None found"

                a :: b ->
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
                                String.fromInt (options.currentBendBreach + 1)
                                    ++ " of "
                                    ++ (String.fromInt <| List.length options.bendBreaches)
                                    ++ ", radius "
                                    ++ showShortMeasure imperial (Quantity.abs radius)
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
                        , label = text "Smooth these points"
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
                        , label = text "Widen current bend"
                        }

                    --, Input.button
                    --    (alignTop :: neatToolsBorder)
                    --    { onPress = Just (msgWrapper NudgeAll)
                    --    , label = text "Widen these bends"
                    --    }
                    ]

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
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column [ padding 4, spacing 6, width fill ]
                    [ modeSelection
                    , angleSelection
                    , if options.mode == DirectionChangeWithRadius then
                        radiusSelection

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
            noTrackMessage


widenBend :
    List Int
    -> Quantity Float Meters
    -> TrackLoaded msg
    -> ( Maybe PeteTree, List GPXSource, ( Int, Int ) )
widenBend points adjustment track =
    Tools.Nudge.widenBendHelper points adjustment track
