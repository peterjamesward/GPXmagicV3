module Tools.OneClickQuickFix exposing
    ( Msg
    , apply
    , oneClickQuickFixButton
    , update
    )

{-
   One-click quick-fix.
       - Simplify until mean density >= 25 meters, empirically.
       - Maximum slope 15% up & down. (NO)
       - Interpolate to max 10m spacing, say.
       - Centroid x 3
       - Write with same file name (OS will append -1)
       - Button goes in the top bar, not the accordion.
-}

import Actions exposing (ToolAction)
import DomainModel exposing (GPXSource, PeteTree)
import Element exposing (Element, centerY, height, px)
import Element.Input as Input
import Length
import Loop
import Quantity
import Tools.BezierSplines
import Tools.CentroidAverage
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Interpolate
import Tools.Simplify
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (prettyButtonStyles)


type Msg
    = Apply


apply : TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply originalTrack =
    let
        trackWithNoMarkers =
            { originalTrack | markerPosition = Nothing }

        simplifyTrack : TrackLoaded msg -> TrackLoaded msg
        simplifyTrack anyTrack =
            let
                meanSpacing =
                    DomainModel.trueLength anyTrack.trackTree
                        |> Quantity.divideBy (toFloat <| DomainModel.skipCount anyTrack.trackTree)

                treeWithOneRoundOfPointsRemoved =
                    Tools.Simplify.simplifyFor1CQF anyTrack

                mnumberOfPointsRemoved =
                    DomainModel.skipCount treeWithOneRoundOfPointsRemoved
                        - DomainModel.skipCount anyTrack.trackTree
            in
            if
                (meanSpacing |> Quantity.lessThanOrEqualTo (Length.meters 25))
                    && mnumberOfPointsRemoved
                    > 0
            then
                simplifyTrack { anyTrack | trackTree = treeWithOneRoundOfPointsRemoved }

            else
                { anyTrack | trackTree = treeWithOneRoundOfPointsRemoved }

        interpolateTrack : TrackLoaded msg -> TrackLoaded msg
        interpolateTrack track =
            { track | trackTree = Tools.Interpolate.interpolateFor1CQF track }

        smoothTrack : TrackLoaded msg -> TrackLoaded msg
        smoothTrack track =
            { track | trackTree = Tools.CentroidAverage.centroidAverageFor1CQF track }

        bezierApprox : TrackLoaded msg -> TrackLoaded msg
        bezierApprox track =
            { track | trackTree = Tools.BezierSplines.bezierApproximationFor1CQF track }

        finalTrack =
            trackWithNoMarkers
                |> simplifyTrack
                |> bezierApprox
                |> Loop.for 3 smoothTrack
    in
    ( Just finalTrack.trackTree
    , DomainModel.getAllGPXPointsInNaturalOrder originalTrack.trackTree
    )


oneClickQuickFixButton : I18NOptions.Location -> (Msg -> msg) -> Maybe (TrackLoaded msg) -> Element msg
oneClickQuickFixButton location wrapper track =
    case track of
        Just _ ->
            Input.button
                (height (px 30) :: centerY :: prettyButtonStyles)
                { onPress = Just (wrapper Apply)
                , label = I18N.text location "main" "1CQF"
                }

        Nothing ->
            Element.none


update :
    Msg
    -> Maybe (TrackLoaded msg)
    -> List (ToolAction msg)
update msg hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, Apply ) ->
            [ Actions.OneClickQuickFix, Actions.TrackHasChanged ]

        _ ->
            []
