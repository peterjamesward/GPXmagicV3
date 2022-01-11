module AbruptDirectionChanges exposing (..)

import Actions
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import List.Extra
import LocalCoords exposing (LocalCoords)
import Quantity
import Scene3d exposing (Entity)
import UtilsForViews exposing (showAngle)
import ViewPureStyles exposing (neatToolsBorder, sliderThumb, useIcon)
import ViewingMode exposing (ViewingMode)


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
    -> (Msg -> msg)
    ->
        { model
            | trackTree : Maybe PeteTree
            , directionChangeOptions : Options
            , viewMode : ViewingMode
            , scene : List (Entity LocalCoords)
            , currentPosition : Int
            , referenceLonLat : DomainModel.GPXSource
            , renderDepth : Int
        }
    ->
        ( { model
            | trackTree : Maybe PeteTree
            , directionChangeOptions : Options
            , viewMode : ViewingMode
            , scene : List (Entity LocalCoords)
            , currentPosition : Int
            , referenceLonLat : DomainModel.GPXSource
            , renderDepth : Int
          }
        , Cmd msg
        )
update msg msgWrapper model =
    let
        oldOptions =
            model.directionChangeOptions
    in
    case msg of
        ViewNext ->
            let
                newOptions =
                    { oldOptions
                        | currentBreach =
                            min
                                (List.length oldOptions.breaches - 1)
                                (1 + oldOptions.currentBreach)
                    }
            in
            ( { model | directionChangeOptions = newOptions }
            , Cmd.none
            )

        ViewPrevious ->
            let
                newOptions =
                    { oldOptions | currentBreach = max 0 (oldOptions.currentBreach - 1) }
            in
            ( { model | directionChangeOptions = newOptions }
            , Cmd.none
            )

        SetCurrentPosition position ->
            Actions.setCurrentPosition position model

        SetThreshold angle ->
            let
                newOptions =
                    { oldOptions | threshold = angle, breaches = [] }

                populatedOptions =
                    case model.trackTree of
                        Just aTree ->
                            findAbruptDirectionChanges newOptions aTree

                        Nothing ->
                            newOptions
            in
            ( { model | directionChangeOptions = populatedOptions }
            , Cmd.none
            )


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
                                { label = useIcon FeatherIcons.eye
                                , onPress = Just <| msgWrapper <| SetCurrentPosition position
                                }
                            , Input.button neatToolsBorder
                                { label = useIcon FeatherIcons.chevronRight
                                , onPress = Just <| msgWrapper <| ViewNext
                                }
                            ]
                        ]
            ]
