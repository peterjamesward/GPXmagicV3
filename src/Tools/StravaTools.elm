module Tools.StravaTools exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import ColourPalette exposing (stravaOrange)
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, boundingBox, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Http
import Iso8601
import Length
import List.Extra
import OAuth as O
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import Time
import ToolTip exposing (localisedTooltip, myTooltip, tooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.StravaDataLoad exposing (..)
import Tools.StravaOptions exposing (Options, StravaStatus(..))
import Tools.StravaTypes exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Url.Builder as Builder
import ViewPureStyles exposing (displayName, neatToolsBorder, prettyButtonStyles)


toolId =
    "strava"


type Msg
    = UserChangedRouteId String
    | LoadExternalRoute
    | LoadActivity
    | HandleSegmentData (Result Http.Error StravaSegment)
    | HandleSegmentStreams (Result Http.Error StravaSegmentStreams)
    | HandleRouteData (Result Http.Error StravaRoute)
    | GpxDownloaded (Result Http.Error String)
    | ActivityDownloaded (Result Http.Error StravaActivity)
    | ActivityStreamsDownloaded (Result Http.Error StravaActivityStreams)
    | UserChangedSegmentId String
    | LoadSegmentStreams
    | LoadExternalSegment
    | PasteSegment
    | ClearSegment
    | ConnectionInfo O.Token
    | DisplayInfo String String


defaultOptions : Options
defaultOptions =
    { stravaStatus = StravaDisconnected
    , externalSegmentId = ""
    , externalRouteId = ""
    , externalSegment = SegmentNone
    , stravaRoute = StravaRouteNone
    , activity = StravaActivityNone
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
            -- It's a bit convoluted because a tool cannot issue commands, but
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

        LoadActivity ->
            -- It's a bit convoluted because a tool cannot issue commands, but
            -- must send instruction by way of Action back to Main.
            case settings.stravaStatus of
                StravaConnected token ->
                    ( settings
                    , [ Actions.RequestStravaActivity
                            (wrap << ActivityDownloaded)
                            settings.externalRouteId
                            token
                      ]
                    )

                StravaDisconnected ->
                    ( settings, [] )

        HandleRouteData response ->
            case settings.stravaStatus of
                StravaConnected token ->
                    let
                        result =
                            stravaProcessRoute response
                    in
                    ( { settings | stravaRoute = result }
                    , case result of
                        StravaRouteOk _ ->
                            [ Actions.RequestStravaRoute
                                (wrap << GpxDownloaded)
                                settings.externalRouteId
                                token
                            ]

                        _ ->
                            []
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
            case settings.stravaStatus of
                StravaConnected token ->
                    ( { settings | externalSegment = SegmentRequested }
                    , [ Actions.RequestStravaSegment
                            (wrap << HandleSegmentData)
                            settings.externalSegmentId
                            token
                      ]
                    )

                StravaDisconnected ->
                    ( settings, [] )

        HandleSegmentData response ->
            case track of
                Just isTrack ->
                    ( { settings
                        | externalSegment =
                            stravaProcessSegment
                                response
                                isTrack
                      }
                    , []
                    )

                Nothing ->
                    ( settings, [] )

        LoadSegmentStreams ->
            case settings.stravaStatus of
                StravaConnected token ->
                    ( settings
                    , [ Actions.RequestStravaSegmentStreams
                            (wrap << HandleSegmentStreams)
                            settings.externalSegmentId
                            token
                      ]
                    )

                StravaDisconnected ->
                    ( settings, [] )

        HandleSegmentStreams response ->
            case ( track, response, settings.externalSegment ) of
                ( Just isTrack, Ok streams, SegmentOk segment ) ->
                    let
                        newSettings =
                            { settings
                                | stravaStreams = Just streams
                                , externalSegment = SegmentPreviewed segment
                                , preview =
                                    pointsFromStreams
                                        isTrack
                                        segment
                                        streams
                            }
                    in
                    ( newSettings, previewActions newSettings stravaOrange isTrack )

                ( _, Err err, _ ) ->
                    ( { settings | lastHttpError = Just err }, [] )

                _ ->
                    ( settings, [] )

        ActivityDownloaded response ->
            case ( settings.stravaStatus, response ) of
                ( StravaConnected token, Ok header ) ->
                    let
                        newSettings =
                            { settings
                                | activity = StravaActivityGotHeader header
                            }
                    in
                    ( newSettings
                    , [ Actions.RequestStravaActivityStreams
                            (wrap << ActivityStreamsDownloaded)
                            settings.externalRouteId
                            token
                      ]
                    )

                ( _, Err err ) ->
                    ( { settings | lastHttpError = Just err }, [] )

                _ ->
                    ( settings, [] )

        ActivityStreamsDownloaded response ->
            case response of
                Ok streams ->
                    case settings.activity of
                        StravaActivityGotHeader header ->
                            --Make track from streams data
                            --Don't need it in setting anymore once working.
                            ( { settings
                                | activity = StravaActivityNone
                              }
                            , [ Actions.TrackFromStravaActivity header streams ]
                            )

                        _ ->
                            ( { settings
                                | activity = StravaActivityError "state error"
                              }
                            , []
                            )

                Err err ->
                    ( { settings | lastHttpError = Just err }, [] )

        PasteSegment ->
            case ( track, settings.externalSegment ) of
                ( Just isTrack, SegmentPreviewed segment ) ->
                    -- Note that we pass the OLD settings to the paste action.
                    ( { settings
                        | stravaStreams = Nothing
                        , externalSegment = SegmentNone
                        , preview = []
                      }
                    , [ PasteStravaSegment settings
                      , HidePreview "strava"
                      , TrackHasChanged
                      ]
                    )

                _ ->
                    ( settings, [] )

        ClearSegment ->
            ( { settings | stravaStreams = Nothing, externalSegment = SegmentNone }
            , []
            )

        DisplayInfo tool tag ->
            ( settings, [ Actions.DisplayInfo tool tag ] )


segmentName : Options -> Maybe String
segmentName options =
    case options.externalSegment of
        SegmentOk segment ->
            Just segment.name

        SegmentPreviewed segment ->
            Just segment.name

        _ ->
            Nothing


extractFromLngLat latlng =
    case latlng of
        [ latitude, longitude ] ->
            { longitude = Direction2d.fromAngle <| Angle.degrees longitude
            , latitude = Angle.degrees latitude
            , altitude = Quantity.zero
            , timestamp = Nothing
            }

        _ ->
            { longitude = Direction2d.positiveX
            , latitude = Quantity.zero
            , altitude = Quantity.zero
            , timestamp = Nothing
            }


trackFromActivity : StravaActivity -> StravaActivityStreams -> Maybe (TrackLoaded msg)
trackFromActivity header streams =
    --TODO: Need activity header to get a name.
    let
        baseTimeMillis =
            Iso8601.toTime header.activityStart
                |> Result.toMaybe
                |> Maybe.withDefault (Time.millisToPosix 0)
                |> Time.posixToMillis

        pointTime t =
            Time.millisToPosix <| baseTimeMillis + (1000 * t)

        gpx =
            List.map3 makePoint
                streams.latLngs
                streams.altitude
                streams.time

        makePoint latLng alt time =
            GPXSource
                (Direction2d.fromAngle <| Angle.degrees latLng.lng)
                (Angle.degrees latLng.lat)
                (Length.meters alt)
                (Just <| pointTime time)
    in
    TrackLoaded.trackFromPoints header.activityName gpx


pointsFromStreams :
    TrackLoaded msg
    -> StravaSegment
    -> StravaSegmentStreams
    -> List PreviewPoint
pointsFromStreams track segment streams =
    -- We need to apply the base point shift but using the original base point.
    -- We can fudge this by consing it to the track.
    let
        startGpx =
            extractFromLngLat segment.start_latlng

        fromStart =
            DomainModel.nearestToLonLat
                startGpx
                track.currentPosition
                track.trackTree
                track.referenceLonLat
                track.leafIndex

        asGpx =
            List.map2
                (\latLon alt ->
                    GPXSource
                        (Direction2d.fromAngle <| Angle.degrees latLon.lng)
                        (Angle.degrees latLon.lat)
                        (Length.meters alt)
                        Nothing
                )
                streams.latLngs.data
                streams.altitude.data

        asEarthPoints =
            List.map
                (DomainModel.pointFromGpxWithReference track.referenceLonLat)
                asGpx
    in
    TrackLoaded.asPreviewPoints track
        (DomainModel.distanceFromIndex fromStart track.trackTree)
        asEarthPoints


previewActions : Options -> Color -> TrackLoaded msg -> List (ToolAction msg)
previewActions options colour track =
    [ ShowPreview
        { tag = "strava"
        , shape = PreviewCircle
        , colour = colour
        , points = options.preview
        }
    ]


paste :
    Options
    -> TrackLoaded msg
    -> ( Maybe PeteTree, List GPXSource, ( Int, Int ) )
paste options track =
    -- This will auto-reverse the segment if needed to match our direction of travel.
    -- That's just based on track indexes. Might be a proble if we traverse it
    -- more than once, but let's park that thought.
    case ( options.externalSegment, options.preview ) of
        ( SegmentPreviewed segment, x1 :: xs ) ->
            let
                segmentStartGpx =
                    extractFromLngLat segment.start_latlng

                segmentEndGpx =
                    extractFromLngLat segment.end_latlng

                pStartingTrackPoint =
                    -- Our first track point will be replaced with the first stream point
                    DomainModel.nearestToLonLat
                        segmentStartGpx
                        0
                        track.trackTree
                        track.referenceLonLat
                        track.leafIndex

                pEndingTrackPoint =
                    -- Our last track point will be replaced with the last stream point
                    DomainModel.nearestToLonLat
                        segmentEndGpx
                        0
                        track.trackTree
                        track.referenceLonLat
                        track.leafIndex

                ( readyToPaste, useStart, useEnd ) =
                    if pEndingTrackPoint < pStartingTrackPoint then
                        ( List.reverse options.preview
                        , pEndingTrackPoint
                        , pStartingTrackPoint
                        )

                    else
                        ( options.preview, pStartingTrackPoint, pEndingTrackPoint )

                newTree =
                    DomainModel.replaceRange
                        useStart
                        (skipCount track.trackTree - useEnd)
                        track.referenceLonLat
                        (List.map .gpx readyToPaste)
                        track.trackTree

                oldPoints =
                    DomainModel.extractPointsInRange
                        useStart
                        (skipCount track.trackTree - useEnd)
                        track.trackTree
            in
            ( newTree
            , oldPoints |> List.map Tuple.second
            , ( useStart, useEnd )
            )

        _ ->
            ( Nothing, [], ( 0, 0 ) )


viewStravaTab : I18NOptions.Location -> Options -> (Msg -> msg) -> Maybe (TrackLoaded msg) -> Element msg
viewStravaTab location options wrap track =
    let
        i18n =
            I18N.text location toolId

        segmentIdField =
            Input.text
                [ width (minimum 150 <| fill)

                --, tooltip below (localisedTooltip location toolId "segmenttip")
                ]
                { onChange = wrap << UserChangedSegmentId
                , text = options.externalSegmentId
                , placeholder = Just <| Input.placeholder [] <| i18n "segmentid"
                , label = Input.labelHidden "Segment ID"
                }

        routeIdField =
            Input.text
                [ width <| px 150 ]
                { onChange = wrap << UserChangedRouteId
                , text = options.externalRouteId
                , placeholder = Just <| Input.placeholder [] <| i18n "routeid"
                , label = Input.labelHidden "Strava route or activity ID"
                }

        segmentButton =
            -- Make this button serve two functions.
            -- 1. After a URL change, to load the segment header;
            -- 2. After header loaded, to load and paste the streams.
            case options.externalSegment of
                SegmentOk segment ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrap LoadSegmentStreams
                        , label = i18n "preview"
                        }

                SegmentPreviewed segment ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrap PasteSegment
                        , label = i18n "paste"
                        }

                SegmentNone ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrap LoadExternalSegment
                        , label = i18n "fetch"
                        }

                SegmentNotInRoute _ ->
                    i18n "badsegment"

                _ ->
                    none

        clearButton =
            case options.externalSegment of
                SegmentNone ->
                    none

                _ ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrap ClearSegment
                        , label = i18n "clear"
                        }

        segmentInfo =
            case options.externalSegment of
                SegmentRequested ->
                    i18n "waiting"

                SegmentError err ->
                    text err

                SegmentNone ->
                    i18n "none"

                SegmentOk segment ->
                    text segment.name

                SegmentPreviewed segment ->
                    i18n "loaded"

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
                            , label = i18n "view"
                            }
                        ]

                StravaRouteError err ->
                    paragraph [] [ text err ]

                _ ->
                    none

        routeButton =
            button
                neatToolsBorder
                { onPress = Just <| wrap LoadExternalRoute
                , label = i18n "route"
                }

        activityButton =
            button
                neatToolsBorder
                { onPress = Just <| wrap LoadActivity
                , label = i18n "activity"
                }
    in
    column
        [ spacing 10
        , padding 10
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        case ( options.stravaStatus, track ) of
            ( StravaConnected token, Just _ ) ->
                [ stravaLink
                , row [ spacing 10 ]
                    [ routeIdField
                    , routeButton
                    , activityButton
                    ]
                , row [ spacing 10 ]
                    [ segmentIdField
                    , segmentButton
                    ]
                , clearButton
                , segmentInfo
                ]

            ( StravaConnected token, Nothing ) ->
                [ stravaLink
                , row [ spacing 10 ]
                    [ routeIdField
                    , routeButton
                    , activityButton
                    ]
                , paragraph [] [ i18n "about" ]
                ]

            ( StravaDisconnected, _ ) ->
                [ i18n "connect" ]
