module Tools.CurveFormer exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import ColourPalette exposing (warningColor)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FeatherIcons
import FlatColors.ChinesePalette
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Quantity
import Svg
import Svg.Attributes as SA
import Tools.CurveFormerOptions exposing (GradientSmoothing(..), Options)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import UtilsForViews exposing (showDecimal2, showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (checkboxIcon, commonShortHorizontalSliderStyles, disabledToolsBorder, edges, neatToolsBorder, prettyButtonStyles, useIcon)


defaultOptions : Options
defaultOptions =
    { vector = Vector2d.zero
    , referencePoint = Nothing
    , dragging = Nothing
    , smoothGradient = Holistic
    , pushRadius = Length.meters 10.0
    , pullRadius = Length.meters 15.0
    , spacing = Length.meters 5.0
    , usePullRadius = False
    , pointsWithinCircle = []
    , pointsWithinDisc = []
    , circle = Nothing
    , pointsAreContiguous = False
    , newTrackPoints = []
    , fixedAttachmentPoints = Nothing
    , transitionRadius = Length.meters 20.0
    , lastVector = Vector2d.zero
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | DraggerModeToggle Bool
    | DraggerReset
    | SetPushRadius Float
    | SetPullRadius Float
    | SetTransitionRadius Float
    | SetSpacing Float
    | ToggleUsePullRadius Bool
    | ApplyWithOptions


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
            ( options
            , [ ShowPreview
                    { tag = "former"
                    , shape = PreviewCircle
                    , colour = colour
                    , points = computeNewPoints options theTrack
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "former" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetPushRadius radius ) ->
            let
                newOptions =
                    { options | pushRadius = Length.meters radius }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "former"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints newOptions track
                    }
              ]
            )

        ( Just track, SetPullRadius radius ) ->
            let
                newOptions =
                    { options | pullRadius = Length.meters radius }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "former"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints newOptions track
                    }
              ]
            )

        ( Just track, ToggleUsePullRadius _ ) ->
            let
                newOptions =
                    { options | usePullRadius = not options.usePullRadius }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "former"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints newOptions track
                    }
              ]
            )

        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ Actions.CurveFormerApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : Bool -> (Msg -> msg) -> Options -> Element msg
view imperial wrapper options =
    let
        squared x =
            x * x

        showPushRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPushRadius << squared
                , label =
                    -- Note, I want non-linear slider.
                    Input.labelBelow []
                        (text <| "Bend radius " ++ showShortMeasure imperial options.pushRadius)
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = options.pushRadius |> Length.inMeters |> sqrt
                , thumb = Input.defaultThumb
                }

        showTransitionRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetTransitionRadius << squared
                , label =
                    -- Note, I want non-linear slider.
                    Input.labelBelow []
                        (text <| "Joining radius " ++ showShortMeasure imperial options.transitionRadius)
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = options.transitionRadius |> Length.inMeters |> sqrt
                , thumb = Input.defaultThumb
                }

        showPullRadiusSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetPullRadius
                , label =
                    Input.labelBelow []
                        (text <| "Inclusion zone " ++ showShortMeasure imperial options.pullRadius)
                , min = 5.0
                , max = 40.0
                , step = Nothing
                , value = options.pullRadius |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showSpacingSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetSpacing
                , label =
                    Input.labelBelow []
                        (text <| "Spacing " ++ showShortMeasure imperial options.spacing)
                , min = 2.0
                , max = 10.0
                , step = Nothing
                , value = options.spacing |> Length.inMeters
                , thumb = Input.defaultThumb
                }

        showActionButtons =
            row [ padding 5, spacing 5, width fill ]
                [ Input.button neatToolsBorder
                    { label = text "Reset"
                    , onPress = Just <| wrapper DraggerReset
                    }
                , case ( List.length options.newTrackPoints >= 3, options.pointsAreContiguous ) of
                    ( _, True ) ->
                        Input.button
                            neatToolsBorder
                            { label = text "Apply"
                            , onPress = Just <| wrapper ApplyWithOptions
                            }

                    ( _, False ) ->
                        Input.button
                            (width fill :: disabledToolsBorder)
                            { label = paragraph [ width fill ] <| [ text "Need at least 3 contiguous points" ]
                            , onPress = Nothing
                            }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = Input.defaultCheckbox
                , checked = options.smoothGradient == Holistic
                , label = Input.labelRight [ centerY ] (text "Smooth gradient")
                }

        showPullSelection =
            Input.checkbox []
                { onChange = wrapper << ToggleUsePullRadius
                , icon = Input.defaultCheckbox
                , checked = options.usePullRadius
                , label = Input.labelRight [ centerY ] (text "Include outliers")
                }

        showHelpfulMessage =
            row [ padding 5, spacing 10, Background.color warningColor, width fill ]
                [ useIcon FeatherIcons.info
                , paragraph [] <| [ text "I don't know what to do without contiguous points." ]
                ]
    in
    -- Try with linear vector, switch to log or something else if needed.
    row
        [ paddingEach { edges | right = 10 }
        , spacing 5
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ twoWayDragControl options wrapper
        , column [ width fill, spacing 5 ]
            [ wrappedRow
                [ Element.alignLeft
                , Element.width Element.fill
                , spacing 5
                ]
                [ showPushRadiusSlider
                , showTransitionRadiusSlider
                , showSpacingSlider
                , showPullSelection
                , if options.usePullRadius then
                    showPullRadiusSlider

                  else
                    none
                ]
            , showModeSelection
            , showActionButtons
            ]
        ]


type alias Point =
    Point2d Meters LocalCoords


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


controlSvgRadius =
    100


twoWayDragControl : Options -> (Msg -> msg) -> Element msg
twoWayDragControl options wrapper =
    let
        clickableContainer =
            el
                [ htmlAttribute <| Pointer.onDown (.pointer >> .offsetPos >> point >> DraggerGrab >> wrapper)
                , htmlAttribute <| Pointer.onMove (.pointer >> .offsetPos >> point >> DraggerMove >> wrapper)
                , htmlAttribute <| Pointer.onUp (.pointer >> .offsetPos >> point >> DraggerRelease >> wrapper)
                , htmlAttribute <| Html.Attributes.style "touch-action" "none"
                , Element.pointer
                , Element.alignLeft
                ]
                << html
                << Svg.svg
                    [ SA.viewBox "-150 -150 300 300"
                    , SA.width "140px"
                    , SA.height "140px"
                    ]

        ( x, y ) =
            Vector2d.components options.vector

        ( xPoint, yPoint ) =
            ( String.fromFloat <| Length.inMeters x
            , String.fromFloat <| Length.inMeters y
            )
    in
    clickableContainer <|
        [ Svg.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| String.fromInt controlSvgRadius
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SA.fill "darkslategrey"
            ]
            []
        , Svg.line
            [ SA.x1 "0"
            , SA.y1 "0"
            , SA.x2 xPoint
            , SA.y2 yPoint
            , SA.stroke "orange"
            , SA.strokeWidth "10"
            , SA.strokeLinecap "round"
            ]
            []
        ]


type alias FoldState =
    { newPoints : List EarthPoint
    }


formCurveWithOptions : Bool -> Options -> Int -> Int -> PeteTree -> List EarthPoint
formCurveWithOptions isLoop options fromStart fromEnd treeNode =
    let
        foldFn : RoadSection -> FoldState -> FoldState
        foldFn road state =
            state

        foldOutput =
            DomainModel.traverseTreeBetweenLimitsToDepth
                fromStart
                (skipCount treeNode - fromEnd)
                (always Nothing)
                0
                treeNode
                foldFn
                (FoldState [])
    in
    foldOutput.newPoints |> List.reverse
