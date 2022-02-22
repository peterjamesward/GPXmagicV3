module ViewMap exposing (..)

import Actions exposing (ToolAction(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons
import FlatColors.ChinesePalette
import Html.Attributes exposing (id)
import Pixels exposing (Pixels, inPixels)
import Quantity exposing (Quantity)
import ToolTip exposing (myTooltip, tooltip)
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (useIcon)


type alias Context =
    { mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , followOrange : Bool
    , draggable : Bool
    }


type Msg
    = ToggleFollowOrange
    | ToggleDraggable


initialiseContext : Maybe Context -> Context
initialiseContext currentContext =
    case currentContext of
        Just context ->
            { context
                | mapClickDebounce = False
                , lastMapClick = ( 0, 0 )
            }

        Nothing ->
            { mapClickDebounce = False
            , lastMapClick = ( 0, 0 )
            , followOrange = False
            , draggable = False
            }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track area context =
    case msg of
        ToggleFollowOrange ->
            ( { context | followOrange = not context.followOrange }
            , []
            )

        ToggleDraggable ->
            let
                newOptions =
                    { context | draggable = not context.draggable }
            in
            ( newOptions
            , [ MakeMapPointsDraggable newOptions.draggable ]
            )


view :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe Context
    -> (Msg -> msg)
    -> Element msg
view ( viewWidth, viewHeight ) mContext msgWrapper =
    let
        handyMapControls context =
            column
                [ alignTop
                , alignRight
                , moveDown 100
                , moveLeft 10
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                , Font.size 40
                , padding 6
                , spacing 8
                ]
                [ Input.button
                    [ tooltip onLeft <|
                        case context.followOrange of
                            True ->
                                myTooltip "Map locked to Orange"

                            False ->
                                myTooltip "Map is draggable"
                    ]
                    { onPress = Just <| msgWrapper ToggleFollowOrange
                    , label =
                        if context.followOrange then
                            useIcon FeatherIcons.lock

                        else
                            useIcon FeatherIcons.unlock
                    }
                , Input.button
                    [ tooltip onLeft <|
                        case context.draggable of
                            True ->
                                myTooltip "Click to disable point dragging"

                            False ->
                                myTooltip "Click to allow point dragging"
                    ]
                    { onPress = Just <| msgWrapper ToggleDraggable
                    , label =
                        if context.draggable then
                            useIcon FeatherIcons.move

                        else
                            useIcon FeatherIcons.x
                    }
                ]
    in
    case mContext of
        Just context ->
            row
                [ inFront <| handyMapControls context ]
                [ el
                    [ width <| px <| inPixels viewWidth
                    , height <| px <| inPixels viewHeight
                    , alignLeft
                    , alignTop
                    , Border.width 2
                    , Border.color FlatColors.ChinesePalette.peace
                    , htmlAttribute (id "map")
                    ]
                    none
                ]

        Nothing ->
            -- Keep the DOM hierarchy consistent.
            row [] [ el [ htmlAttribute (id "map") ] none ]
