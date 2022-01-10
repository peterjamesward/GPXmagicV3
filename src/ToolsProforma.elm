module ToolsProforma exposing (..)

import Color exposing (Color)
import DomainModel exposing (PeteTree)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import Json.Encode as E
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)
import TrackInfoBox
import ViewPureStyles exposing (useIcon)


type ToolState
    = Expanded
    | Contracted
    | Disabled


type ToolDock
    = DockUpperLeft
    | DockLowerLeft
    | DockUpperRight
    | DockLowerRight
    | DockBottom
    | DockNone


type ToolType
    = ToolTrackInfo


type ToolMsg
    = ToolPopupToggle ToolType
    | ToolDockSelect ToolType ToolDock
    | ToolColourSelect ToolType Element.Color
    | ToolStateToggle ToolType


type alias ToolEntry =
    { toolType : ToolType
    , label : String
    , info : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , tabColour : Element.Color
    , isPopupOpen : Bool
    }


tools : List ToolEntry
tools =
    -- One list or five, or six? Try one. Arguably a Dict but POITROAE.
    [ toolEntryForTrackInfoBox
    ]


toolEntryForTrackInfoBox : ToolEntry
toolEntryForTrackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Summary info"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperLeft
    , tabColour = FlatColors.AussiePalette.beekeeper
    , isPopupOpen = False
    }


toggleToolPopup : ToolType -> ToolEntry -> ToolEntry
toggleToolPopup toolType tool =
    if tool.toolType == toolType then
        { tool | isPopupOpen = not tool.isPopupOpen }

    else
        tool


setDock : ToolType -> ToolDock -> ToolEntry -> ToolEntry
setDock toolType dock tool =
    if tool.toolType == toolType then
        { tool | dock = dock }

    else
        tool


update :
    ToolMsg
    -> (ToolMsg -> msg)
    -> { model | tools : List ToolEntry }
    -> ( { model | tools : List ToolEntry }, Cmd msg )
update toolMsg msgWrapper model =
    case toolMsg of
        ToolPopupToggle toolType ->
            ( { model | tools = List.map (toggleToolPopup toolType) model.tools }
            , Cmd.none
            )

        ToolDockSelect toolType toolDock ->
            ( { model | tools = List.map (setDock toolType toolDock) model.tools }
            , Cmd.none
            )

        ToolColourSelect toolType color ->
            ( model, Cmd.none )

        ToolStateToggle toolType ->
            ( model, Cmd.none )



--View stuff


toolsForDock :
    ToolDock
    -> (ToolMsg -> msg)
    ->
        { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
        }
    -> Element msg
toolsForDock dock msgWrapper model =
    column [] <|
        (model.tools
            |> List.filter (\tool -> tool.dock == dock)
            |> List.map (viewTool msgWrapper model))


viewTool :
    (ToolMsg -> msg)
    -> { model | trackTree : Maybe PeteTree }
    -> ToolEntry
    -> Element msg
viewTool msgWrapper model toolEntry =
    column [ width fill ]
        [ row
            [ width fill
            , padding 4
            , spacing 8
            , Font.color FlatColors.AussiePalette.beekeeper
            , Background.color toolEntry.tabColour
            ]
            [ useIcon FeatherIcons.settings
            , text toolEntry.label
            ]
        , TrackInfoBox.trackInfoBox model.trackTree
        ]
