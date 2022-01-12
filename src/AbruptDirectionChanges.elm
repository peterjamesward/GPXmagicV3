module AbruptDirectionChanges exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import List.Extra
import Quantity
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle)
import ViewPureStyles exposing (neatToolsBorder, sliderThumb, useIcon)


type alias Options =
    { threshold : Angle
    , breaches : List ( Int, Angle )
    , currentBreach : Int
    }


defaultOptions =
    { threshold = Angle.degrees 60
    , breaches = []
    , currentBreach = 0
    }


type Msg
    = ViewNext
    | ViewPrevious
    | SetCurrentPosition Int
    | SetThreshold Angle


findAbruptDirectionChanges : Options -> PeteTree -> Options
findAbruptDirectionChanges options tree =
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
                            Direction2d.angleFrom
                                (node.left |> asRecord |> .directionAtEnd)
                                (node.right |> asRecord |> .directionAtStart)
                                |> Quantity.abs

                        withThisNodeIfNeeded =
                            -- Is it at this node, or one of its children?
                            if thisNodeAngle |> Quantity.greaterThanOrEqualTo options.threshold then
                                ( skip + 1, thisNodeAngle ) :: accum

                            else
                                accum
                    in
                    if
                        node.nodeContent.directionChangeMaximumAbs
                            |> Quantity.greaterThanOrEqualTo options.threshold
                    then
                        withThisNodeIfNeeded
                            |> helper skip node.left
                            |> helper (skip + skipCount node.left) node.right

                    else
                        accum
    in
    { options
        | breaches = helper 0 tree []
        , currentBreach = 0
    }


update :
    Msg
    -> Options
    -> TrackLoaded
    -> ( Options, List (ToolAction msg) )
update msg options track =
    case msg of
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
                    { options | threshold = angle, breaches = [] }

                populatedOptions =
                    findAbruptDirectionChanges newOptions track.trackTree
            in
            ( populatedOptions, [] )


view : (Msg -> msg) -> Options -> Element msg
view msgWrapper options =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        column [ centerX, padding 4, spacing 4, height <| px 100 ]
            [ Input.slider
                ViewPureStyles.shortSliderStyles
                { onChange = Angle.degrees >> SetThreshold >> msgWrapper
                , value = Angle.inDegrees options.threshold
                , label = Input.labelHidden "Direction change threshold"
                , min = 30
                , max = 120
                , step = Just 1
                , thumb = sliderThumb
                }
            , el [ centerX ] <|
                text <|
                    "Threshold "
                        ++ (String.fromInt <| round <| Angle.inDegrees options.threshold)
                        ++ "º"
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
                            [ Input.button neatToolsBorder
                                { label = useIcon FeatherIcons.chevronLeft
                                , onPress = Just <| msgWrapper <| ViewPrevious
                                }
                            , Input.button neatToolsBorder
                                { label = useIcon FeatherIcons.mousePointer
                                , onPress = Just <| msgWrapper <| SetCurrentPosition position
                                }
                            , Input.button neatToolsBorder
                                { label = useIcon FeatherIcons.chevronRight
                                , onPress = Just <| msgWrapper <| ViewNext
                                }
                            ]
                        ]
            ]
