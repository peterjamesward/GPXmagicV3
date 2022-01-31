module SceneBuilderProfile exposing (..)

import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientHue, gradientHue2)
import DomainModel exposing (..)
import Length exposing (Meters)
import Quantity
import TrackLoaded exposing (TrackLoaded)


type alias ProfileDatum =
    -- Intended for use with the terezka charts, but agnostic.
    -- One required for each point
    { distance : Float -- metres or miles depending on units setting
    , minAltitude : Float -- metres or feet
    , maxAltitude : Float -- will be same as above for Leaf
    , startGradient : Float -- percent
    , endGradient : Float -- again, same for Leaf.
    , colour : Color -- use average gradient if not Leaf
    }


renderBoth : TrackLoaded msg -> List ProfileDatum
renderBoth track =
    let
        foldFn :
            RoadSection
            -> ( Length.Length, List ProfileDatum )
            -> ( Length.Length, List ProfileDatum )
        foldFn road ( distance, outputs ) =
            -- Ambitiously, do gradient in the same traversal.
            ( distance |> Quantity.plus road.trueLength
            , outputs
            )

        depthFn road =
            -- Try fading of detail in rendering.
            Just 10

        ( _, result ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                depthFn
                0
                track.trackTree
                foldFn
                ( Quantity.zero, [] )
    in
    result
