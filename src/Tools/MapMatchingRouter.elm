module Tools.MapMatchingRouter exposing
    ( Msg(..)
    , defaultOptions
    , initialise
    , mapMatchingApi
    , toolId
    , toolStateChange
    , trackFromDrawnRoute
    , update
    , view
    )

-- Use mapbox's map matching API for use to route around <= 100 waypoints.

import Actions exposing (ToolAction)
import Angle
import CommonToolStyles
import Direction2d
import DomainModel
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.FlatUIPalette
import Http
import MapboxKey exposing (mapboxKey)
import Maybe.Extra
import Quantity
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.MapMatchingRouterOptions exposing (Intersection, Leg, Matching, Matchings, Options, RouteState(..), Step, matchingsDecoder)
import TrackLoaded exposing (TrackLoaded)
import Url.Builder as Builder
import ViewPureStyles exposing (neatToolsBorder, rgtPurple, useIconWithSize)


toolId =
    "routing"


apiRoot =
    --"https://api.mapbox.com/matching/v5/mapbox/{profile}/{coordinates}.json?access_token=YOUR_MAPBOX_ACCESS_TOKEN"
    --e.g. https://api.mapbox.com/matching/v5/mapbox/driving/-117.17282,32.71204;-117.17288,32.71225?
    -- steps=true&radiuses=25;25&
    -- access_token=pk.eyJ1IjoicGV0ZXJqYW1lc3dhcmQiLCJhIjoiY2tpcmpwem54MjdhbTJycWpvYjU2dmJpcSJ9.gysxozddlQQ0XaWnywEyJg
    "https://api.mapbox.com"



--matching/v5/mapbox/{profile}/{coordinates}.json?access_token=YOUR_MAPBOX_ACCESS_TOKEN"


formatCoordinatePairs : List (List Float) -> String
formatCoordinatePairs coords =
    let
        formatPair : List Float -> String
        formatPair lonLat =
            case lonLat of
                [ lon, lat ] ->
                    String.join ","
                        [ String.fromFloat lon
                        , String.fromFloat lat
                        ]

                _ ->
                    ""
    in
    coords |> List.map formatPair |> String.join ";"


radii : List (List Float) -> String
radii coords =
    -- Must specify tolerance for each coordinate.
    "25" |> List.repeat (List.length coords) |> String.join ";"


mapMatchingApi : (Result Http.Error Matchings -> msg) -> List (List Float) -> Cmd msg
mapMatchingApi msg coords =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            Builder.crossOrigin apiRoot
                [ "matching"
                , "v5"
                , "mapbox"
                , "driving"
                , formatCoordinatePairs coords
                ]
                [ Builder.string "access_token" mapboxKey
                , Builder.string "steps" "true"
                , Builder.string "radiuses" (radii coords)
                ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg matchingsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


trackFromDrawnRoute : Result Http.Error Matchings -> Options -> ( Options, Maybe (TrackLoaded msg) )
trackFromDrawnRoute result options =
    case result of
        Ok resultBody ->
            let
                asGPXpoints : List DomainModel.GPXSource
                asGPXpoints =
                    resultBody.matchings |> List.concatMap collectMatchings

                collectMatchings : Matching -> List DomainModel.GPXSource
                collectMatchings matching =
                    matching.legs |> List.concatMap collectLegs

                collectLegs : Leg -> List DomainModel.GPXSource
                collectLegs leg =
                    leg.steps |> List.concatMap collectSteps

                collectSteps : Step -> List DomainModel.GPXSource
                collectSteps step =
                    Maybe.Extra.toList (asGPX step.maneuver.location)
                        ++ List.concatMap collectIntersections step.intersections

                --++ (Maybe.Extra.toList <| asGPX step.maneuver.location)
                collectIntersections : Intersection -> List DomainModel.GPXSource
                collectIntersections intersection =
                    Maybe.Extra.toList <| asGPX intersection.location

                asGPX : List Float -> Maybe DomainModel.GPXSource
                asGPX location =
                    case location of
                        [ lon, lat ] ->
                            Just
                                { longitude = Direction2d.fromAngle <| Angle.degrees lon
                                , latitude = Angle.degrees lat
                                , altitude = Quantity.zero
                                , timestamp = Nothing
                                }

                        _ ->
                            Nothing
            in
            ( { options | routeState = RouteShown }
            , TrackLoaded.trackFromPoints "Drawn on map" asGPXpoints
            )

        Err _ ->
            ( { options | routeState = RouteFailed }, Nothing )


defaultOptions : Options
defaultOptions =
    { numPoints = 0
    , routeState = RouteIdle
    }


type Msg
    = DisplayInfo String String
    | EnablePlanning
    | GetDrawnPoints
    | UseMapElevations


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

        guidanceText tag =
            row
                [ Background.color FlatColors.FlatUIPalette.turquoise
                , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
                , Border.rounded 5
                ]
                [ useIconWithSize 20 FeatherIcons.info
                , paragraph [ padding 4 ] [ i18n tag ]
                ]

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

        elevationFetchButton =
            Input.button
                neatToolsBorder
                { onPress = Just <| wrapper UseMapElevations
                , label = i18n "elevations"
                }
    in
    column (CommonToolStyles.toolContentBoxStyle settings) <|
        if track == Nothing then
            case options.routeState of
                RouteIdle ->
                    [ guidanceText "info"
                    , startButton
                    ]

                RouteDrawing ->
                    [ guidanceText "guidance"
                    , routeButton
                    ]

                RouteComputing ->
                    [ guidanceText "waiting" ]

                RouteShown ->
                    [ guidanceText "done"
                    , elevationFetchButton
                    ]

                RouteFailed ->
                    [ guidanceText "fail" ]

        else
            [ guidanceText "done"
            , elevationFetchButton
            ]


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

        UseMapElevations ->
            ( options
            , [ Actions.FetchMapElevations ]
            )



-- END
