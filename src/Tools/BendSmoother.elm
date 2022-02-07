module Tools.BendSmoother exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, endPoint, skipCount, startPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Tools.BendSmootherOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { bendTrackPointSpacing = 5.0
    , smoothedBend = Nothing
    , segments = 1
    }


type alias Point =
    Point2d Meters LocalCoords


type Msg
    = ApplyWithOptions
    | SetBendTrackPointSpacing Float


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        earthPoints =
            []

        previewPoints =
            earthPoints
                |> List.map
                    (\earth ->
                        ( earth
                        , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                        )
                    )
    in
    previewPoints


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                (List.map Tuple.second <| computeNewPoints options track)
                track.trackTree

        oldPoints =
            DomainModel.extractPointsInRange
                fromStart
                fromEnd
                track.trackTree
    in
    ( newTree
    , oldPoints |> List.map Tuple.second
    )


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, previewActions options colour theTrack )

        _ ->
            ( options, [ HidePreview "bend" ] )


previewActions newOptions colour track =
    -- Subverting this mechanism to show the discs and captured points on the views.
    [ ShowPreview
        { tag = "bend"
        , shape = PreviewCircle
        , colour = colour
        , points = computeNewPoints newOptions track
        }
    ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetBendTrackPointSpacing spacing ) ->
            let
                newOptions =
                    { options | bendTrackPointSpacing = spacing }

                --|> makeCurveIfPossible track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ Actions.BendSmootherApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view imperial wrapper options track =
    let
        fixBendButton smooth =
            button
                neatToolsBorder
                { onPress = Just <| wrapper ApplyWithOptions
                , label =
                    case smooth of
                        Just isSmooth ->
                            paragraph [] <|
                                [ text <|
                                    "Smooth between markers\nRadius "
                                        ++ showShortMeasure imperial (Length.meters isSmooth.radius)
                                ]

                        Nothing ->
                            text "No bend found"
                }
    in
    case track of
        Just isTrack ->
            column
                [ padding 10
                , spacing 5
                , width fill
                , centerX
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ el [centerX] <| bendSmoothnessSlider imperial options wrapper
                , el [centerX] <| fixBendButton options.smoothedBend
                ]

        Nothing ->
            noTrackMessage


bendSmoothnessSlider : Bool -> Options -> (Msg -> msg) -> Element msg
bendSmoothnessSlider imperial options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetBendTrackPointSpacing
        , label =
            Input.labelBelow [] <|
                text <|
                    "Spacing: "
                        ++ showShortMeasure imperial (Length.meters options.bendTrackPointSpacing)
        , min =
            Length.inMeters <|
                if imperial then
                    Length.feet 3.0

                else
                    Length.meters 1.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 30.0

                else
                    Length.meters 10.0
        , step = Nothing
        , value = options.bendTrackPointSpacing
        , thumb = Input.defaultThumb
        }
