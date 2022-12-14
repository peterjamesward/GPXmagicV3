module Tools.StravaOptions exposing (Options, StravaStatus(..))

import Http
import OAuth as O
import PreviewData exposing (PreviewPoint)
import Tools.StravaTypes exposing (StravaActivityStatus, StravaRouteStatus, StravaSegmentStatus, StravaSegmentStreams)


type alias Options =
    { stravaStatus : StravaStatus
    , externalSegmentId : String
    , externalRouteId : String
    , externalSegment : StravaSegmentStatus
    , activity : StravaActivityStatus
    , stravaRoute : StravaRouteStatus
    , stravaStreams : Maybe StravaSegmentStreams
    , lastHttpError : Maybe Http.Error
    , preview : List PreviewPoint
    , adjustSegmentAltitude : Bool
    }


type StravaStatus
    = StravaDisconnected
    | StravaConnected O.Token
