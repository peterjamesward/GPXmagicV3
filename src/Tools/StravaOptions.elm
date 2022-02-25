module Tools.StravaOptions exposing (..)

import DomainModel exposing (EarthPoint, GPXSource)
import Http
import OAuth as O
import PreviewData exposing (PreviewPoint)
import Tools.StravaTypes exposing (StravaRouteStatus, StravaSegmentStatus, StravaSegmentStreams)


type alias Options =
    { stravaStatus : StravaStatus
    , externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , stravaRoute : StravaRouteStatus
    , stravaStreams : Maybe StravaSegmentStreams
    , lastHttpError : Maybe Http.Error
    , preview : List PreviewPoint
    }


type StravaStatus
    = StravaDisconnected
    | StravaConnected O.Token
