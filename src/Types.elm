module Types exposing (..)

import OAuthTypes as O


type alias FrontendModel =
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
    , paneLayoutOptions : PaneLayoutManager.Options
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


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
