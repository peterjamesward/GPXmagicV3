module Tools.MoveAndStretch exposing (..)

import Actions exposing (PreviewShape(..), ToolAction(..))
import Axis3d
import DomainModel exposing (EarthPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Length exposing (meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import Quantity
import Svg
import Svg.Attributes as SA
import Tools.MoveAndStretchOptions exposing (Mode(..), Options, Point)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { vector = Vector2d.zero
    , lastVector = Vector2d.zero
    , dragging = Nothing
    , preview = []
    , mode = Translate
    , stretchPointer = Nothing
    , heightSliderSetting = 0.0 -- Note this is not the length.
    }


type Msg
    = DraggerGrab Point
    | DraggerMove Point
    | DraggerRelease Point
    | DraggerModeToggle Bool
    | DraggerReset
    | DraggerMarker Int
    | DraggerApply
    | StretchHeight Float


radius =
    100


heightOffset sliderValue =
    -- Using cube here gives large range, preserves sign, more control in the centre.
    let
        clamped =
            -- Just to be sure.
            clamp -1.0 1.0 sliderValue
    in
    Length.meters <| 100.0 * clamped * clamped * clamped


point : ( Float, Float ) -> Point
point ( x, y ) =
    Point2d.fromMeters { x = x, y = y }


settingNotZero : Options -> Bool
settingNotZero model =
    Vector2d.direction model.vector
        /= Nothing
        || model.heightSliderSetting
        /= 0.0


twoWayDragControl : Options -> (Msg -> msg) -> Element msg
twoWayDragControl model wrapper =
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
            Vector2d.components model.vector

        ( xPoint, yPoint ) =
            ( String.fromFloat <| Length.inMeters x
            , String.fromFloat <| Length.inMeters y
            )
    in
    clickableContainer <|
        [ Svg.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| String.fromInt radius
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
            ( options, [ HidePreview "stretch" ] )


previewActions newOptions colour track =
    [ ShowPreview
        { tag = "stretch"
        , shape = PreviewLine
        , colour = colour
        , points = []
        }
    ]


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Options, List (Actions.ToolAction msg) )
update message model wrapper track =
    case message of
        DraggerGrab offset ->
            ( { model | dragging = Just offset }
            , []
            )

        DraggerMove offset ->
            case model.dragging of
                Nothing ->
                    ( model
                    , []
                    )

                Just dragStart ->
                    let
                        newVector =
                            model.lastVector |> Vector2d.plus (Vector2d.from dragStart offset)
                    in
                    ( { model | vector = newVector }
                    , []
                    )

        DraggerRelease _ ->
            ( { model
                | dragging = Nothing
                , lastVector = model.vector
              }
            , []
            )

        DraggerModeToggle bool ->
            ( { model
                | mode =
                    case model.mode of
                        Translate ->
                            Stretch

                        Stretch ->
                            Translate
              }
            , []
            )

        DraggerReset ->
            ( { model
                | dragging = Nothing
                , vector = Vector2d.zero
                , heightSliderSetting = 0.0
                , preview = []
              }
            , []
            )

        DraggerMarker int ->
            ( { model | stretchPointer = Just int }
            , []
            )

        DraggerApply ->
            ( { model | preview = [] }
            , []
            )

        StretchHeight x ->
            ( { model | heightSliderSetting = x }
            , []
            )


minmax a b =
    ( toFloat <| min a b
    , toFloat <| max a b
    )


view : Bool -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view imperial options wrapper track =
    let
        ( nearEnd, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        farEnd =
            DomainModel.skipCount track.trackTree - fromEnd

        canApply =
            case options.mode of
                Translate ->
                    nearEnd < farEnd

                Stretch ->
                    let
                        drag =
                            options.stretchPointer |> Maybe.withDefault 0
                    in
                    nearEnd < drag && drag < farEnd

        heightSlider =
            Input.slider commonShortVerticalSliderStyles
                { onChange = wrapper << StretchHeight
                , label =
                    Input.labelBelow [  ]
                        (text <| showShortMeasure imperial (heightOffset options.heightSliderSetting))
                , min = -1.0
                , max = 1.0
                , step = Nothing
                , value = options.heightSliderSetting
                , thumb =
                    Input.defaultThumb
                }

        showSliderInStretchMode =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << DraggerMarker << round
                , label =
                    Input.labelBelow []
                        (text "Choose the point to drag")
                , min = toFloat <| nearEnd + 1
                , max = toFloat <| farEnd - 1
                , step = Just 1.0
                , value = options.stretchPointer |> Maybe.withDefault 0 |> toFloat
                , thumb = Input.defaultThumb
                }

        showActionButtons =
            row [ spacing 5 ]
                [ Input.button neatToolsBorder
                    { label = text "Zero", onPress = Just <| wrapper DraggerReset }
                , if canApply then
                    Input.button neatToolsBorder
                        { label = text "Apply"
                        , onPress = Just <| wrapper DraggerApply
                        }

                  else
                    Input.button neatToolsBorder
                        { label = text "Not valid"
                        , onPress = Nothing
                        }
                ]

        showModeSelection =
            Input.checkbox []
                { onChange = wrapper << DraggerModeToggle
                , icon = Input.defaultCheckbox
                , checked = options.mode == Stretch
                , label = Input.labelRight [ centerY ] (text "Stretch")
                }
    in
    -- Try with linear vector, switch to log or something else if needed.
    row
        [ padding 5
        , spacing 5
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ twoWayDragControl options wrapper
        , column
            [ Element.alignLeft
            , Element.width Element.fill
            , spacing 10
            ]
            [ el [ centerX ] showModeSelection
            , el [ centerX ] showSliderInStretchMode
            , el [ centerX ] showActionButtons
            ]
        , heightSlider
        ]


movePoints : Options -> List EarthPoint -> List EarthPoint
movePoints options region =
    -- This used by preview and action.
    let
        ( xShift, yShift ) =
            Vector2d.components options.vector

        zShift =
            heightOffset options.heightSliderSetting

        translation =
            -- Negate y because SVG coordinates go downards.
            Point3d.translateBy (Vector3d.xyz xShift (Quantity.negate yShift) zShift)

        ( notLast, last ) =
            region |> List.Extra.splitAt (List.length region - 1)

        ( first, regionExcludingEnds ) =
            notLast |> List.Extra.splitAt 1
    in
    []



{-

   stretchPoints : Options -> EarthPoint -> List EarthPoint -> List EarthPoint
   stretchPoints options stretcher region =
       -- This used by preview and action.
       -- Here we move points either side of the stretch marker.
       let
           ( xShift, yShift ) =
               Vector2d.components options.vector

           zShiftMax =
               Vector3d.xyz
                   Quantity.zero
                   Quantity.zero
                   (heightOffset options.heightSliderSetting)

           horizontalTranslation =
               -- Negate y because SVG coordinates go downards.
               Vector3d.xyz xShift (Quantity.negate yShift) (meters 0)

           ( startAnchor, endAnchor ) =
               ( region |> List.head |> Maybe.withDefault stretcher
               , region |> List.Extra.last |> Maybe.withDefault stretcher
               )

           ( firstPart, secondPart ) =
               region |> List.Extra.splitAt (stretcher.index - startAnchor.index)

           ( firstPartAxis, secondPartAxis ) =
               ( Axis3d.throughPoints startAnchor.xyz stretcher.xyz
               , Axis3d.throughPoints endAnchor.xyz stretcher.xyz
               )

           ( firstPartDistance, secondPartDistance ) =
               ( Point3d.distanceFrom startAnchor.xyz stretcher.xyz
               , Point3d.distanceFrom endAnchor.xyz stretcher.xyz
               )

           distanceAlong maybeAxis p =
               case maybeAxis of
                   Just axis ->
                       p |> Point3d.signedDistanceAlong axis

                   Nothing ->
                       Quantity.zero

           ( adjustedFirstPoints, adjustedSecondPoints ) =
               ( List.map
                   adjustRelativeToStart
                   firstPart
               , List.map
                   adjustRelativeToEnd
                   secondPart
               )

           adjustRelativeToStart pt =
               let
                   proportion =
                       Quantity.ratio
                           (pt.xyz |> distanceAlong firstPartAxis)
                           firstPartDistance
               in
               pt.xyz
                   |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                   |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportion)

           adjustRelativeToEnd pt =
               let
                   proportion =
                       Quantity.ratio
                           (pt.xyz |> distanceAlong secondPartAxis)
                           secondPartDistance
               in
               pt.xyz
                   |> Point3d.translateBy (horizontalTranslation |> Vector3d.scaleBy proportion)
                   |> Point3d.translateBy (zShiftMax |> Vector3d.scaleBy proportion)
       in
       adjustedFirstPoints ++ adjustedSecondPoints
-}
