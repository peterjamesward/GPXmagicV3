module AbruptDirectionChanges exposing (..)

import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (PeteTree(..), asRecord)
import Element exposing (..)
import Element.Background as Background
import FlatColors.ChinesePalette
import Quantity


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


view : (Msg -> msg) -> Options -> Element msg
view msgWrapper options =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case options.breaches of
            pair :: morePairs ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ text <| "Found" ++ (String.fromInt <| List.length options.breaches)
                    ]

            [] ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ text "None found"
                    ]
