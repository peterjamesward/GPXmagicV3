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
import Length
import List.Extra
import LocalCoords exposing (LocalCoords)
import PreviewData exposing (PreviewShape(..))
import Quantity
import ToolTip exposing (buttonStylesWithTooltip, myTooltip, tooltip)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showDecimal0, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (infoButton, neatToolsBorder, noTrackMessage, sliderThumb, useIcon)


type alias Options =
    { threshold : Angle
    , breaches : List ( Int, Angle )
    , currentBreach : Int
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
    , breaches = []
    , currentBreach = 0
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
        , ( "info", infoText )
        , ( "autofix", autofixText )
        , ( "locate", """These buttons will move the Orange pointer through the list of issues.

**Note**: this will only centre the views which have the padlock closed.""")
        ]
    )


infoText =
    """Find points where the road direction changes significantly, or find
sections of track that may be a bend with a small radius.

From here, you can jump directly to the sections of track and use other tools to fix the problems.
"""


autofixText =
    """Smooth each of these individually using the single point _Smooth with arcs_. Use that
tool to change the number of points that are added to smooth each point.

You should use this only for trivial fixes; there are better tools for smoothing
serious issues. This tool can even make things worse."""


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
                | breaches = helper 0 tree []
                , currentBreach = 0
            }

        DirectionChangeWithRadius ->
            { options
                | breaches = findBendsWithRadius tree options
                , currentBreach = 0
            }


findBendsWithRadius : PeteTree -> Options -> List ( Int, Angle )
findBendsWithRadius tree options =
    {-
       The UI says direction change and radius but what we look for is the
       given direction change over a track length equivalent to the radius.
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
            -> ( Int, List ( Int, RoadSection ), List ( Int, Angle ) )
            -> ( Int, List ( Int, RoadSection ), List ( Int, Angle ) )
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
                    degreesTurned newWindow

                degreesTurned : List ( Int, RoadSection ) -> Float
                degreesTurned sections =
                    case sections of
                        [] ->
                            0

                        [ onlyOne ] ->
                            0

                        ( _, first ) :: ( n, second ) :: more ->
                            second.directionAtStart
                                |> Direction2d.angleFrom first.directionAtStart
                                |> Angle.inDegrees
                                |> (+) (degreesTurned (( n, second ) :: more))
            in
            if abs turnDuringWindow >= Angle.inDegrees options.threshold then
                ( newIndex + 1
                , []
                , ( newIndex
                  , Angle.degrees <| abs turnDuringWindow
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
            , [ ShowPreview
                    { tag = "kinks"
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
                    { options | mode = mode, breaches = [] }

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
            let
                breachIndex =
                    min (List.length options.breaches - 1) (options.currentBreach + 1)

                newOptions =
                    { options | currentBreach = breachIndex }

                ( position, _ ) =
                    Maybe.withDefault ( 0, Angle.degrees 0 ) <|
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
                    Maybe.withDefault ( 0, Angle.degrees 0 ) <|
                        List.Extra.getAt breachIndex newOptions.breaches
            in
            ( newOptions, [ SetCurrent position ] )

        SetCurrentPosition position ->
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
            , [ Actions.Autofix <| List.map Tuple.first options.breaches
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
            TrackLoaded.buildPreview
                (List.map Tuple.first options.breaches)
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

        resultsNavigation =
            case options.breaches of
                [] ->
                    el [ centerX, centerY ] <| text "None found"

                a :: b ->
                    let
                        ( position, turn ) =
                            Maybe.withDefault ( 0, Angle.degrees 0 ) <|
                                List.Extra.getAt options.currentBreach options.breaches
                    in
                    column [ spacing 4, centerX ]
                        [ el [ centerX ] <|
                            text <|
                                String.fromInt (options.currentBreach + 1)
                                    ++ " of "
                                    ++ (String.fromInt <| List.length options.breaches)
                                    ++ " is "
                                    ++ (showAngle <| turn)
                                    ++ "º"
                        , row [ centerX, spacing 10 ]
                            [ infoButton <| msgWrapper <| DisplayInfo "bends" "locate"
                            , Input.button
                                (buttonStylesWithTooltip below "Move to previous")
                                { label = useIcon FeatherIcons.chevronLeft
                                , onPress = Just <| msgWrapper <| ViewPrevious
                                }
                            , Input.button
                                (buttonStylesWithTooltip below "Move pointer to this issue\n(Is the padlock on?)")
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
            if options.breaches == [] || options.mode == DirectionChangeWithRadius then
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
    in
    case isTrack of
        Just track ->
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column [ padding 4, spacing 6, width fill ]
                    [ el [ centerX ] modeSelection
                    , el [ centerX ] angleSelection
                    , if options.mode == DirectionChangeWithRadius then
                        el [ centerX ] radiusSelection

                      else
                        none
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
                                , alignTop
                                , padding 6
                                ]
                            <|
                                List.map
                                    (Tuple.first >> linkButton track.trackTree)
                                    options.breaches
                    ]

        Nothing ->
            noTrackMessage
