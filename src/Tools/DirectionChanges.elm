module Tools.DirectionChanges exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Angle exposing (Angle)
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
import Quantity
import ToolTip exposing (buttonStylesWithTooltip, myTooltip, tooltip)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showDecimal0, showShortMeasure)
import ViewPureStyles exposing (neatToolsBorder, noTrackMessage, sliderThumb, useIcon)


type alias Options =
    { threshold : Angle
    , breaches : List ( Int, Angle )
    , currentBreach : Int
    , mode : DirectionChangeMode
    , radius : Length.Length
    }


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
    }


type Msg
    = ViewNext
    | ViewPrevious
    | SetCurrentPosition Int
    | SetThreshold Angle
    | SetRadius Length.Length
    | SetMode DirectionChangeMode


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
                        DomainModel.buildPreview
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
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( msg, hasTrack ) of
        ( SetMode mode, Just track ) ->
            let
                newOptions =
                    { options | mode = mode, breaches = [] }

                populatedOptions =
                    findDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions, actions populatedOptions previewColour track )

        ( ViewNext, _ ) ->
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

        ( ViewPrevious, _ ) ->
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

        ( SetCurrentPosition position, _ ) ->
            ( options, [ SetCurrent position ] )

        ( SetThreshold angle, Just track ) ->
            let
                newOptions =
                    { options | threshold = angle }

                populatedOptions =
                    findDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions
            , actions options previewColour track
            )

        ( SetRadius radius, Just track ) ->
            let
                newOptions =
                    { options | radius = radius }

                populatedOptions =
                    findDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions
            , actions options previewColour track
            )

        _ ->
            ( options, [] )


actions options previewColour track =
    [ ShowPreview
        { tag = "kinks"
        , shape = PreviewCircle
        , colour = previewColour
        , points =
            DomainModel.buildPreview
                (List.map Tuple.first options.breaches)
                track.trackTree
        }
    ]


view : Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view imperial msgWrapper options isTrack =
    case isTrack of
        Just track ->
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column [ centerX, padding 4, spacing 4 ]
                    [ Input.radio [ centerX, spacing 5 ]
                        { onChange = msgWrapper << SetMode
                        , options =
                            [ Input.option DirectionChangeAbrupt (text "Abrupt changes")
                            , Input.option DirectionChangeWithRadius (text "Significant bends")
                            ]
                        , selected = Just options.mode
                        , label = Input.labelHidden "Mode"
                        }
                    , Input.slider
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
                    , if options.mode == DirectionChangeWithRadius then
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

                      else
                        none
                    , case options.breaches of
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
                                    [ Input.button
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
                    ]

        Nothing ->
            noTrackMessage
