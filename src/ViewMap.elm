module ViewMap exposing (..)

import Actions exposing (ToolAction)
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
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (useIcon)


type alias Context =
    { mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , followOrange : Bool
    }


type Msg
    = ToggleFollowOrange


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
                [ Input.button []
                    { onPress = Just <| msgWrapper ToggleFollowOrange
                    , label =
                        if context.followOrange then
                            useIcon FeatherIcons.lock

                        else
                            useIcon FeatherIcons.unlock
                    }
                ]
    in
    case mContext of
        Just context ->
            row
                [ spacing 0
                , padding 0
                , inFront <| handyMapControls context
                ]
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
