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
import Element.Input as Input
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.MapMatchingRouterOptions exposing (Options, RouteState(..))
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (neatToolsBorder, rgtPurple)


toolId =
    "routing"


api =
    --"https://api.mapbox.com/matching/v5/mapbox/{profile}/{coordinates}.json?access_token=YOUR_MAPBOX_ACCESS_TOKEN"
    --e.g. https://api.mapbox.com/matching/v5/mapbox/driving/-117.17282,32.71204;-117.17288,32.71225?
    -- steps=true&radiuses=25;25&
    -- access_token=pk.eyJ1IjoicGV0ZXJqYW1lc3dhcmQiLCJhIjoiY2tpcmpwem54MjdhbTJycWpvYjU2dmJpcSJ9.gysxozddlQQ0XaWnywEyJg
    "https://api.mapbox.com/matching/v5/mapbox/{profile}/{coordinates}.json?access_token=YOUR_MAPBOX_ACCESS_TOKEN"


defaultOptions : Options
defaultOptions =
    { numPoints = 0
    , routeState = RouteIdle
    }


type Msg
    = DisplayInfo String String
    | EnablePlanning
    | GetDrawnPoints


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
                , label = i18n "enable"
                }

        routeButton =
            Input.button
                neatToolsBorder
                { onPress = Just <| wrapper GetDrawnPoints
                , label = i18n "fetch"
                }
    in
    column (CommonToolStyles.toolContentBoxStyle settings) <|
        if track == Nothing then
            case options.routeState of
                RouteIdle ->
                    [ startButton ]

                RouteDrawing ->
                    [ routeButton ]

                RouteComputing ->
                    []

                RouteShown ->
                    []

                RouteAdopted ->
                    []

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
                | routeState = RouteDrawing
                , numPoints = 0
              }
            , [ Actions.EnablePlanningOnMap ]
            )

        GetDrawnPoints ->
            ( { options
                | routeState = RouteComputing
                , numPoints = 0
              }
            , [ Actions.GetPointsFromMap ]
            )



-- END
