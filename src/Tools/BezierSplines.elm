module Tools.BezierSplines exposing
    ( Msg(..)
    , applyUsingOptions
    , defaultOptions
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import BezierSplines
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import String.Interpolate
import Tools.BezierOptions as BezierOptions exposing (..)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder)


toolId =
    "splines"


defaultOptions : Options
defaultOptions =
    { bezierTension = 0.5
    , bezierTolerance = 5.0
    , bezierStyle = BezierOptions.Approximated
    , extent = ExtentIsRange
    }


type Msg
    = SetBezierTension Float
    | SetBezierTolerance Float
    | BezierApplyWithOptions
    | SetBezierStyle BezierStyle


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            if track.markerPosition /= Nothing then
                TrackLoaded.getRangeFromMarkers track

            else
                ( 0, 0 )

        distanceToPreview =
            distanceFromIndex fromStart track.trackTree

        splineFunction =
            case options.bezierStyle of
                ThroughExisting ->
                    BezierSplines.bezierSplinesThroughExistingPoints

                Approximated ->
                    BezierSplines.bezierSplineApproximation

        splineEarthPoints =
            splineFunction
                False
                options.bezierTension
                options.bezierTolerance
                fromStart
                (skipCount track.trackTree - fromEnd)
                track.trackTree
    in
    TrackLoaded.asPreviewPoints track distanceToPreview splineEarthPoints



--TODO: Refactor and whilst at it, stop redundant calculations.


applyUsingOptions :
    Options
    -> TrackLoaded msg
    -> TrackLoaded msg
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            if track.markerPosition /= Nothing then
                TrackLoaded.getRangeFromMarkers track

            else
                ( 0, 0 )

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                (List.map .gpx <| computeNewPoints options track)
                track.trackTree
    in
    case newTree of
        Just isTree ->
            let
                pointerReposition =
                    --Let's reposition by distance, not uncommon.
                    --TODO: Arguably, position from the relevant track end would be better.
                    DomainModel.preserveDistanceFromStart track.trackTree isTree

                ( newOrange, newPurple ) =
                    ( pointerReposition track.currentPosition
                    , Maybe.map pointerReposition track.markerPosition
                    )
            in
            { track
                | trackTree = isTree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options
            , actions options colour theTrack
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "bezier", HidePreview "bezierprofile" ] )


actions options previewColour track =
    let
        previewTree =
            -- What would the track become if applied?
            .trackTree <| applyUsingOptions options track

        profilePreview ptree =
            ShowPreview
                { tag = "bezierprofile"
                , shape = PreviewProfile ptree
                , colour = previewColour
                , points = []
                }
    in
    case options.extent of
        ExtentIsRange ->
            let
                normalPreview =
                    ShowPreview
                        { tag = "bezier"
                        , shape = PreviewCircle
                        , colour = previewColour
                        , points = computeNewPoints options track
                        }
            in
            [ normalPreview, profilePreview previewTree ]

        ExtentIsTrack ->
            [ profilePreview previewTree ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetBezierTension tension ) ->
            let
                newOptions =
                    { options | bezierTension = tension }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetBezierTolerance tolerance ) ->
            let
                newOptions =
                    { options | bezierTolerance = tolerance }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, BezierApplyWithOptions ) ->
            ( options
            , [ WithUndo (Actions.BezierApplyWithOptions options)
              , Actions.BezierApplyWithOptions options
              , TrackHasChanged
              ]
            )

        ( Just track, SetBezierStyle style ) ->
            let
                newOptions =
                    { options | bezierStyle = style }
            in
            ( newOptions, actions newOptions previewColour track )

        _ ->
            ( options, [] )


view : I18NOptions.Location -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view location wrap options track =
    let
        i18n =
            I18N.text location toolId

        sliders =
            column [ centerX, width fill, spacing 5 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrap << SetBezierTension
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "tension")
                                    [ showDecimal2 options.bezierTension ]
                    , min = 0.0
                    , max = 1.0
                    , step = Nothing
                    , value = options.bezierTension
                    , thumb = Input.defaultThumb
                    }
                , Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrap << SetBezierTolerance
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "tolerance")
                                    [ showDecimal2 options.bezierTolerance ]
                    , min = 1.0
                    , max = 10.0
                    , step = Just 0.5
                    , value = options.bezierTolerance
                    , thumb = Input.defaultThumb
                    }
                ]

        modeChoice =
            Input.radio
                [ padding 10
                , spacing 5
                ]
                { onChange = wrap << SetBezierStyle
                , selected = Just options.bezierStyle
                , label = Input.labelHidden "Style"
                , options =
                    [ Input.option ThroughExisting (i18n "through")
                    , Input.option Approximated (i18n "approx")
                    ]
                }

        extent =
            paragraph [] <|
                if track.markerPosition == Nothing then
                    [ i18n "whole" ]

                else
                    [ i18n "part" ]

        actionButton =
            el [ centerX, width fill, spacing 5 ] <|
                button (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap BezierApplyWithOptions
                    , label = paragraph [] [ i18n "apply" ]
                    }
    in
    column
        [ spacing 5
        , padding 5
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ el [ centerX ] sliders
        , el [ centerX ] modeChoice
        , el [ centerX ] extent
        , el [ centerX ] actionButton
        ]
