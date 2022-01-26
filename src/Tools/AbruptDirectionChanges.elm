module Tools.AbruptDirectionChanges exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, skipCount)
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
    { threshold = Angle.degrees 120
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
    { options
        | breaches = helper 0 tree []
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
            --TODO: May stop sending the list here and let action processor request it.
            -- (Not much in it.)
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
            in
            case hasTrack of
                Just track ->
                    let
                        populatedOptions =
                            findAbruptDirectionChanges newOptions track.trackTree
                    in
                    ( populatedOptions
                    , [ ShowPreview
                            { tag = "kinks"
                            , shape = PreviewCircle
                            , colour = previewColour
                            , points =
                                DomainModel.buildPreview
                                    (List.map Tuple.first options.breaches)
                                    track.trackTree
                            }
                      ]
                    )

                Nothing ->
                    ( newOptions, [] )


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
