module ModelRecord exposing (..)

import AbruptDirectionChanges
import DomainModel exposing (GPXSource, PeteTree)
import GeoCodeDecoders exposing (IpInfo)
import LocalCoords exposing (LocalCoords)
import OAuthTypes as O
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import SplitPane.SplitPane as SplitPane
import Time
import ToolsProforma exposing (ToolEntry)
import ViewContextThirdPerson
import ViewingMode exposing (ViewingMode)



--TODO: Try distinct records for Loaded and NotLoaded state, avoiding Maybes.


type Model
    = Model ModelRecord


type alias ModelRecord =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , stravaAuthentication : O.Model
    , trackTree : Maybe PeteTree
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , scene : List (Entity LocalCoords)
    , currentPosition : Int
    , viewMode : ViewingMode
    , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , ipInfo : Maybe IpInfo
    , mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , viewContext : Maybe ViewContextThirdPerson.ContextThirdPerson
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , leftDockInternal : SplitPane.State
    , rightDockLeftEdge : SplitPane.State
    , rightDockInternal : SplitPane.State
    , bottomDockTopEdge : SplitPane.State

    -- Tools
    , tools : List ToolEntry

    -- Tool specific options
            , directionChangeOptions : AbruptDirectionChanges.Options
    }
