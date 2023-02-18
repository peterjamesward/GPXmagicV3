module Model exposing (..)

import Browser.Dom as Dom
import Dict exposing (Dict)
import Element
import File exposing (File)
import GeoCodeDecoders exposing (IpInfo)
import Html.Events.Extra.Mouse as Mouse
import Http
import LandUseDataTypes
import OAuthTypes as O exposing (OAuthMsg)
import PaneContext
import PaneLayoutManager
import Pixels exposing (Pixels)
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import SplitPane.SplitPane as SplitPane
import SvgPathExtractor
import Time
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.OneClickQuickFix
import Tools.RGTOptions
import ToolsController
import TrackLoaded exposing (TrackLoaded)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | TryRemoteLoad
    | GpxFromUrl (Result Http.Error String)
    | ToggleLoadOptionMenu
    | ToggleRGTOptions
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | StorageMessage E.Value
    | SplitLeftDockRightEdge SplitPane.Msg
    | SplitRightDockLeftEdge SplitPane.Msg
    | Resize Int Int
    | GotWindowSize (Result Dom.Error Dom.Viewport)
    | ToolsMsg ToolsController.ToolMsg
    | DismissModalMessage
    | PaneMsg PaneLayoutManager.Msg
    | ToggleToolPopup
    | BackgroundColour Element.Color
    | Language I18NOptions.Location
    | ToggleLanguageEditor
    | RestoreDefaultToolLayout
    | WriteGpxFile
    | FilenameChange String
    | TimeToUpdateMemory
    | OneClickMsg Tools.OneClickQuickFix.Msg
    | FetchElevationsFromMap
    | ReplaceTrackOnMapAfterStyleChange
    | SvgMsg SvgPathExtractor.Msg
    | FlythroughTick Time.Posix
    | HideInfoPopup
    | ReceivedLandUseData (Result Http.Error LandUseDataTypes.OSMLandUseData)
    | I18NMsg I18N.Msg
    | BackgroundClick Mouse.Event
    | DisplayWelcome
    | RGTOptions Tools.RGTOptions.Msg
    | ProfilePaint
    | NoOp


type alias Model =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , ipInfo : Maybe IpInfo
    , stravaAuthentication : O.Model
    , loadOptionsMenuOpen : Bool
    , svgFileOptions : SvgPathExtractor.Options
    , location : I18NOptions.Location
    , rgtOptionsVisible : Bool
    , loadFromUrl : Maybe Url

    -- Track stuff
    , track : Maybe (TrackLoaded Msg)

    -- Visuals (scenes now in PaneLayoutManager)
    , previews : Dict String PreviewData
    , flythroughRunning : Bool
    , needsRendering : Bool
    , mapPointsDraggable : Bool

    -- Layout stuff
    , windowSize : ( Float, Float )
    , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
    , modalMessage : Maybe String
    , paneLayoutOptions : PaneContext.PaneLayoutOptions
    , infoText : Maybe ( String, String )
    , welcomeDisplayed : Bool

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , rightDockLeftEdge : SplitPane.State

    -- Tools
    , toolOptions : ToolsController.Options Msg
    , isPopupOpen : Bool
    , backgroundColour : Element.Color
    , languageEditorOpen : Bool
    , languageEditor : I18NOptions.Options
    , rgtOptions : Tools.RGTOptions.Options
    }
