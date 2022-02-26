module Tools.StravaTools exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import ColourPalette exposing (stravaOrange)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, boundingBox, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Http
import Length
import List.Extra
import OAuth as O
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import ToolTip exposing (myTooltip, tooltip)
import Tools.StravaDataLoad exposing (..)
import Tools.StravaOptions exposing (Options, StravaStatus(..))
import Tools.StravaTypes exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Url.Builder as Builder
import ViewPureStyles exposing (displayName, neatToolsBorder, prettyButtonStyles)


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
                                (boundingBox isTrack.trackTree)
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
            { longitude = Direction2d.fromAngle <| Angle.degrees segment.start_longitude
            , latitude = Angle.degrees segment.start_latitude
            , altitude = Quantity.zero
            }

        fromStart =
            DomainModel.nearestToLonLat
                startGpx
                track.currentPosition
                track.trackTree

        asGpx =
            List.map2
                (\latLon alt ->
                    GPXSource
                        (Direction2d.fromAngle <| Angle.degrees latLon.lng)
                        (Angle.degrees latLon.lat)
                        (Length.meters alt)
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
                    GPXSource
                        (Direction2d.fromAngle <| Angle.degrees segment.start_longitude)
                        (Angle.degrees segment.start_latitude)
                        Quantity.zero

                segmentEndGpx =
                    GPXSource
                        (Direction2d.fromAngle <| Angle.degrees segment.end_longitude)
                        (Angle.degrees segment.end_latitude)
                        Quantity.zero

                pStartingTrackPoint =
                    -- Our first track point will be replaced with the first stream point
                    DomainModel.nearestToLonLat
                        segmentStartGpx
                        0
                        track.trackTree

                pEndingTrackPoint =
                    -- Our last track point will be replaced with the last stream point
                    DomainModel.nearestToLonLat
                        segmentEndGpx
                        0
                        track.trackTree

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


viewStravaTab : Options -> (Msg -> msg) -> Maybe (TrackLoaded msg) -> Element msg
viewStravaTab options wrap track =
    let
        segmentIdField =
            Input.text
                [ width (minimum 150 <| fill)
                , tooltip below (myTooltip "Paste in a segment number or URL")
                ]
                { onChange = wrap << UserChangedSegmentId
                , text = options.externalSegmentId
                , placeholder = Just <| Input.placeholder [] <| text "Segment ID"
                , label = Input.labelHidden "Segment ID"
                }

        routeIdField =
            Input.text
                [ width (minimum 150 <| fill)
                , tooltip below (myTooltip "Paste in a route number or URL")
                ]
                { onChange = wrap << UserChangedRouteId
                , text = options.externalRouteId
                , placeholder = Just <| Input.placeholder [] <| text "Strava route ID"
                , label = Input.labelHidden "Strava route ID"
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
                        , label = text "Preview"
                        }

                SegmentPreviewed segment ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrap PasteSegment
                        , label = text "Paste"
                        }

                SegmentNone ->
                    button
                        neatToolsBorder
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
                        neatToolsBorder
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

        routeButton =
            button
                neatToolsBorder
                { onPress = Just <| wrap LoadExternalRoute
                , label = text <| "Fetch route"
                }
    in
    wrappedRow
        [ spacing 10
        , padding 10
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        case ( options.stravaStatus, track ) of
            ( StravaConnected token, Just _ ) ->
                [ stravaLink
                , routeIdField
                , routeButton
                , segmentIdField
                , segmentButton
                , clearButton
                , segmentInfo
                ]

            ( StravaConnected token, Nothing ) ->
                [ stravaLink
                , routeIdField
                , routeButton
                , paragraph [] [ text """To load a segment from Strava, you need a route
                 that contains the segment geographicaly.""" ]
                ]

            ( StravaDisconnected, _ ) ->
                [ text "Please connect to Strava" ]
