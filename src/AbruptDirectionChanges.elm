module AbruptDirectionChanges exposing (..)

import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (PeteTree(..), asRecord)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import Quantity
import ViewPureStyles exposing (sliderThumb)


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
    | SetCurrentTo
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
                                ( skip, thisNodeAngle ) :: accum

                            else
                                accum
                    in
                    if
                        node.nodeContent.directionChangeMaximumAbs
                            |> Quantity.greaterThanOrEqualTo options.threshold
                    then
                        withThisNodeIfNeeded
                            |> helper skip node.left
                            |> helper (skip + node.nodeContent.skipCount) node.right

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
        }
    ->
        ( { model
            | trackTree : Maybe PeteTree
            , directionChangeOptions : Options
          }
        , Cmd msg
        )
update msg msgWrapper model =
    case msg of
        ViewNext ->
            ( model, Cmd.none )

        ViewPrevious ->
            ( model, Cmd.none )

        SetCurrentTo ->
            ( model, Cmd.none )

        SetThreshold angle ->
            let
                oldOptions =
                    model.directionChangeOptions

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
        column []
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
            ]
