module Tools.StravaTools exposing (..)

import Actions exposing (ToolAction)
import ColourPalette exposing (stravaOrange)
import DomainModel exposing (EarthPoint, GPXSource, boundingBox)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Http
import List.Extra
import OAuth as O
import Tools.StravaDataLoad as StravaDataLoad exposing (..)
import Tools.StravaTypes as StraveTypes exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Url.Builder as Builder
import ViewPureStyles exposing (displayName, prettyButtonStyles)


type Msg
    = UserChangedRouteId String
    | LoadExternalRoute
    | HandleSegmentData (Result Http.Error StravaSegment)
    | HandleSegmentStreams (Result Http.Error StravaSegmentStreams)
    | HandleRouteData (Result Http.Error StravaRoute)
    | GpxDownloaded (Result Http.Error String)
    | UserChangedSegmentId String
    | LoadSegmentStreams
    | LoadExternalSegment
    | PasteSegment
    | ClearSegment
    | ConnectionInfo O.Token


type StravaStatus
    = StravaDisconnected
    | StravaConnected O.Token


type alias Options =
    { stravaStatus : StravaStatus
    , externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , stravaRoute : StravaRouteStatus
    , stravaStreams : Maybe StravaSegmentStreams
    , lastHttpError : Maybe Http.Error
    , preview : List ( EarthPoint, GPXSource )
    }


defaultOptions : Options
defaultOptions =
    { stravaStatus = StravaDisconnected
    , externalSegmentId = ""
    , externalRouteId = ""
    , externalSegment = SegmentNone
    , stravaRoute = StravaRouteNone
    , stravaStreams = Nothing
    , lastHttpError = Nothing
    , preview = []
    }


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    --TODO: When opened, request Auth state.
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, [] )

        _ ->
            ( options
            , [ Actions.StopFlythroughTicks ]
            )


update :
    Msg
    -> Options
    -> (Msg -> msg)
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg settings wrap track =
    case msg of
        ConnectionInfo token ->
            let _ = Debug.log "got token" token in
            ( { settings | stravaStatus = StravaConnected token }, [] )

        UserChangedRouteId url ->
            let
                routeId =
                    url
                        |> String.split "/"
                        |> List.Extra.last
                        |> Maybe.withDefault ""
            in
            ( { settings | externalRouteId = routeId }
            , []
            )

        UserChangedSegmentId url ->
            let
                segmentId =
                    url
                        |> String.split "/"
                        |> List.Extra.last
                        |> Maybe.withDefault ""
            in
            ( { settings
                | externalSegmentId = segmentId
                , externalSegment = SegmentNone
              }
            , []
            )

        LoadExternalRoute ->
            -- It's a bit convoluted because a tool cannpt issue commands, but
            -- must send instruction by way of Action back to Main.
            case settings.stravaStatus of
                StravaConnected token ->
                    ( { settings | stravaRoute = StravaRouteRequested }
                    , [ Actions.RequestStravaRouteHeader
                            (wrap << HandleRouteData)
                            settings.externalRouteId
                            token
                      ]
                    )

                StravaDisconnected ->
                    ( settings, [] )

        HandleRouteData response ->
            case settings.stravaStatus of
                StravaConnected token ->
                    ( { settings | stravaRoute = stravaProcessRoute response }
                    , [ Actions.RequestStravaRoute
                            (wrap << GpxDownloaded)
                            settings.externalRouteId
                            token
                      ]
                    )

                StravaDisconnected ->
                    ( settings, [] )

        GpxDownloaded response ->
            case response of
                Ok content ->
                    ( settings, [ Actions.LoadGpxFromStrava content ] )

                Err _ ->
                    ( settings, [] )

        LoadExternalSegment ->
            --case getStravaToken authentication of
            --    Just token ->
            --        ( { settings | externalSegment = SegmentRequested }
            --        , []
            --        )
            --
            --    --PostUpdateActions.ActionCommand <|
            --    --    requestStravaSegment
            --    --        (wrap << HandleSegmentData)
            --    --        settings.externalSegmentId
            --    --        token
            --    --)
            --    Nothing ->
            ( settings, [] )

        LoadSegmentStreams ->
            --case getStravaToken authentication of
            --    Just token ->
            --        ( settings, [] )
            --
            --    --, PostUpdateActions.ActionCommand <|
            --    --    requestStravaSegmentStreams
            --    --        (wrap << HandleSegmentStreams)
            --    --        settings.externalSegmentId
            --    --        token
            --    --)
            --    Nothing ->
            ( settings, [] )

        HandleSegmentData response ->
            case track of
                Just isTrack ->
                    ( { settings
                        | externalSegment =
                            stravaProcessSegment
                                response
                                (boundingBox isTrack.trackTree)
                      }
                    , []
                    )

                Nothing ->
                    ( settings, [] )

        HandleSegmentStreams response ->
            case ( track, response, settings.externalSegment ) of
                ( Just isTrack, Ok streams, SegmentOk segment ) ->
                    ( { settings
                        | stravaStreams = Just streams
                        , externalSegment = SegmentPreviewed segment
                      }
                    , []
                    )

                --, PostUpdateActions.ActionPreview
                --)
                ( _, Err err, _ ) ->
                    ( { settings | lastHttpError = Just err }, [] )

                _ ->
                    ( settings, [] )

        PasteSegment ->
            case ( track, settings.externalSegment, settings.stravaStreams ) of
                ( Just isTrack, SegmentPreviewed segment, Just streams ) ->
                    ( { settings
                        | stravaStreams = Nothing
                        , externalSegment = SegmentNone
                      }
                    , []
                    )

                --case buildActions settings isTrack segment streams of
                --    Just action ->
                --        PostUpdateActions.ActionTrackChanged
                --            TrackEditType.EditPreservesIndex
                --            action
                --
                --    Nothing ->
                --        PostUpdateActions.ActionNoOp
                --)
                _ ->
                    ( settings, [] )

        ClearSegment ->
            ( { settings | stravaStreams = Nothing, externalSegment = SegmentNone }
            , []
            )



--, PostUpdateActions.ActionPreview
--)


stravaRouteOption : O.Token -> Options -> (Msg -> msg) -> Element msg
stravaRouteOption auth options wrap =
    let
        routeIdField =
            Input.text [ width (px 150) ]
                { onChange = wrap << UserChangedRouteId
                , text = options.externalRouteId
                , placeholder = Just <| Input.placeholder [] <| text "Strava route ID"
                , label = Input.labelHidden "Strava route ID"
                }

        routeButton =
            button
                prettyButtonStyles
                { onPress = Just <| wrap LoadExternalRoute
                , label = text <| "Fetch route"
                }
    in
    row [ spacing 10 ]
        [ routeIdField
        , routeButton
        ]


viewStravaTab : Options -> (Msg -> msg) -> Maybe (TrackLoaded msg) -> Element msg
viewStravaTab options wrap track =
    let
        segmentIdField =
            Input.text []
                { onChange = wrap << UserChangedSegmentId
                , text = options.externalSegmentId
                , placeholder = Just <| Input.placeholder [] <| text "Segment ID"
                , label = Input.labelHidden "Segment ID"
                }

        segmentButton =
            -- Make this button serve two functions.
            -- 1. After a URL change, to load the segment header;
            -- 2. After header loaded, to load and paste the streams.
            case options.externalSegment of
                SegmentOk segment ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap LoadSegmentStreams
                        , label = text "Preview"
                        }

                SegmentPreviewed segment ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap PasteSegment
                        , label = text "Paste"
                        }

                SegmentNone ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap LoadExternalSegment
                        , label = text <| "Fetch header"
                        }

                SegmentNotInRoute _ ->
                    text "This segment is not\ncontained in the route"

                _ ->
                    none

        clearButton =
            case options.externalSegment of
                SegmentNone ->
                    none

                _ ->
                    button
                        prettyButtonStyles
                        { onPress = Just <| wrap ClearSegment
                        , label = text "Clear"
                        }

        segmentInfo =
            case options.externalSegment of
                SegmentRequested ->
                    text "Waiting for segment"

                SegmentError err ->
                    text err

                SegmentNone ->
                    text "Segment data not loaded, or not yet."

                SegmentOk segment ->
                    text segment.name

                SegmentPreviewed segment ->
                    text "In preview"

                SegmentNotInRoute segment ->
                    text segment.name

        stravaLink =
            let
                stravaUrl =
                    Builder.crossOrigin stravaApiRoot [ "routes", options.externalRouteId ] []
            in
            case options.stravaRoute of
                StravaRouteOk _ ->
                    column [ Font.size 14, padding 5 ]
                        [ displayName <| Just options.externalRouteId
                        , newTabLink [ Font.color stravaOrange ]
                            { url = stravaUrl
                            , label = text "View on Strava"
                            }
                        ]

                _ ->
                    none
    in
    column
        [ spacing 10
        , padding 10
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        case options.stravaStatus of
            StravaConnected token ->
                [ stravaLink
                , row [ spacing 10 ]
                    [ stravaRouteOption token options wrap
                    , segmentIdField
                    , segmentButton
                    , clearButton
                    ]
                , segmentInfo
                ]

            StravaDisconnected ->
                [ text "Please connect to Strava" ]
