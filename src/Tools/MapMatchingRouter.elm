module Tools.MapMatchingRouter exposing
    ( Msg(..)
    , defaultOptions
    , initialise
    , toolId
    , toolStateChange
    , update
    , view
    )

-- Use mapbox's map matching API for use to route around <= 100 waypoints.

import Actions exposing (ToolAction)
import CommonToolStyles
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.FlatUIPalette
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.MapMatchingRouterOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (neatToolsBorder, rgtPurple)


toolId =
    "routing"


defaultOptions : Options
defaultOptions =
    { numPoints = 0
    , planning = False
    }


type Msg
    = DisplayInfo String String
    | EnablePlanning


initialise : Options
initialise =
    defaultOptions


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
            , []
            )

        _ ->
            -- Hide preview
            ( options, [] )


view : SystemSettings -> Maybe (TrackLoaded msg) -> (Msg -> msg) -> Options -> Element msg
view settings track wrapper options =
    let
        i18n =
            I18N.text settings.location toolId

        startButton =
            Input.button
                neatToolsBorder
                { onPress = Just <| wrapper EnablePlanning
                , label = text "Enable map planning"
                }
    in
    column (CommonToolStyles.toolContentBoxStyle settings) <|
        if track == Nothing then
            [ startButton ]

        else
            [ i18n "track" ]


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options wrapper =
    case msg of
        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        EnablePlanning ->
            ( { options
                | planning = True
                , numPoints = 0
              }
            , [ Actions.EnablePlanningOnMap ]
            )



-- END
