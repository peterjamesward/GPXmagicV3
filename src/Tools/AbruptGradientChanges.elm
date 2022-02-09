module Tools.AbruptGradientChanges exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import List.Extra
import Quantity
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle, showDecimal2)
import ViewPureStyles exposing (neatToolsBorder, noTrackMessage, sliderThumb, useIcon)


type alias Options =
    { threshold : Float
    , breaches : List ( Int, Float )
    , currentBreach : Int
    }


defaultOptions =
    { threshold = 10.0
    , breaches = []
    , currentBreach = 0
    }


type Msg
    = ViewNext
    | ViewPrevious
    | SetCurrentPosition Int
    | SetThreshold Float


findAbruptDirectionChanges : Options -> PeteTree -> Options
findAbruptDirectionChanges options tree =
    -- This function called when track changes, or we call it when threshold is changed.
    -- We search the tree. At worst, fold over the whole darn tree. Optimize if needed.
    let
        foldFn :
            RoadSection
            -> ( Int, Maybe RoadSection, List ( Int, Float ) )
            -> ( Int, Maybe RoadSection, List ( Int, Float ) )
        foldFn road ( index, previousIfAny, outputs ) =
            case previousIfAny of
                Nothing ->
                    ( index + 1, Just road, outputs )

                Just previousRoad ->
                    let
                        change =
                            abs (road.gradientAtStart - previousRoad.gradientAtStart)
                    in
                    if change > options.threshold then
                        ( index + 1, Just road, ( index, change ) :: outputs )

                    else
                        ( index + 1, Just road, outputs )

        ( _, _, breaches ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount tree)
                (always Nothing)
                0
                tree
                foldFn
                ( 0, Nothing, [] )
    in
    { options
        | breaches = breaches
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
                    { tag = "ridge"
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
            ( options, [ HidePreview "ridge" ] )


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
                    Maybe.withDefault ( 0, 0 ) <|
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
                    Maybe.withDefault ( 0, 0 ) <|
                        List.Extra.getAt breachIndex newOptions.breaches
            in
            ( newOptions, [ SetCurrent position ] )

        SetCurrentPosition position ->
            ( options, [ SetCurrent position ] )

        SetThreshold value ->
            let
                newOptions =
                    { options | threshold = value, breaches = [] }
            in
            case hasTrack of
                Just track ->
                    let
                        populatedOptions =
                            findAbruptDirectionChanges newOptions track.trackTree
                    in
                    ( populatedOptions
                    , [ ShowPreview
                            { tag = "ridge"
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


view : (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view msgWrapper options isTrack =
    case isTrack of
        Just track ->
            el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
                column [ centerX, padding 4, spacing 4, height <| px 100 ]
                    [ Input.slider
                        ViewPureStyles.shortSliderStyles
                        { onChange = SetThreshold >> msgWrapper
                        , value = options.threshold
                        , label = Input.labelHidden "Gradient change threshold"
                        , min = 3
                        , max = 20
                        , step = Just 1
                        , thumb = sliderThumb
                        }
                    , el [ centerX ] <|
                        text <|
                            "Threshold "
                                ++ showDecimal2 options.threshold
                                ++ "%"
                    , case options.breaches of
                        [] ->
                            el [ centerX, centerY ] <| text "None found"

                        a :: b ->
                            let
                                ( position, turn ) =
                                    Maybe.withDefault ( 0, 0 ) <|
                                        List.Extra.getAt options.currentBreach options.breaches
                            in
                            column [ spacing 4, centerX ]
                                [ el [ centerX ] <|
                                    text <|
                                        String.fromInt (options.currentBreach + 1)
                                            ++ " of "
                                            ++ (String.fromInt <| List.length options.breaches)
                                            ++ " is "
                                            ++ showDecimal2 turn
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

        Nothing ->
            noTrackMessage
