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
    { numPoints = 0 }


type Msg
    = DisplayInfo String String


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


view : SystemSettings -> (Msg -> msg) -> Options -> Element msg
view settings wrapper options =
    let
        i18n =
            I18N.text settings.location toolId

        dataStyles selected =
            if selected then
                Font.bold :: CommonToolStyles.toolContentBoxStyle settings

            else
                CommonToolStyles.toolContentBoxStyle settings
    in
    column (CommonToolStyles.toolContentBoxStyle settings)
        []


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg options wrapper =
    case msg of
        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )



-- END
