port module ProfilePort exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import PreviewData exposing (PreviewData)
import SceneBuilderProfile
import SystemSettings exposing (SystemSettings)
import TrackLoaded exposing (TrackLoaded)
import ViewProfileChartContext


type ProfileMsg
    = ProfileMessage E.Value


port profileCommands : E.Value -> Cmd msg


port profileMessages : (E.Value -> msg) -> Sub msg


paintCanvasProfileChart :
    ViewProfileChartContext.ProfileContext
    -> SystemSettings
    -> TrackLoaded msg
    -> Dict String PreviewData
    -> Cmd msg
paintCanvasProfileChart profileContext settings track previews =
    profileCommands <|
        E.object
            [ ( "Cmd", E.string "Profile" )
            , ( "container", E.string <| "altitude." ++ profileContext.contextSuffix )
            , ( "chart"
              , SceneBuilderProfile.profileChart
                    profileContext
                    settings
                    track
                    previews
              )
            ]


paintCanvasGradientChart : ViewProfileChartContext.ProfileContext -> SystemSettings -> TrackLoaded msg -> Cmd msg
paintCanvasGradientChart profileContext settings track =
    profileCommands <|
        E.object
            [ ( "Cmd", E.string "Gradient" )
            , ( "container", E.string <| "gradient." ++ profileContext.contextSuffix )
            , ( "chart", SceneBuilderProfile.gradientChart profileContext settings track )
            ]


processPortMessage :
    Maybe (TrackLoaded msg)
    -> ProfileMsg
    -> List (ToolAction msg)
processPortMessage mTrack (ProfileMessage json) =
    let
        jsonMsg =
            D.decodeValue (D.field "msg" D.string) json

        containerAndDistance =
            ( D.decodeValue (D.field "container" D.string) json
            , D.decodeValue (D.field "x" D.float) json
            )
    in
    case jsonMsg of
        Ok "profileClick" ->
            -- Not really a map thing but not worth another port.
            -- Need to kick it into Main, where we have imperial, and
            -- should then probably go to PaneLayoutManager.
            --{ 'msg' : 'profileClick'
            --, 'container' : name of the container for the canvas
            --, 'x' : distance
            --}
            case containerAndDistance of
                ( Ok container1, Ok distance1 ) ->
                    [ ProfileClick container1 distance1
                    , PointerChange
                    ]

                _ ->
                    []

        _ ->
            []
