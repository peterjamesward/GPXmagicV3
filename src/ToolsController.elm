module ToolsController exposing (..)

import Actions exposing (ToolAction)
import Element exposing (..)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (roundEach)
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.SwedishPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import List.Extra
import Tools.AbruptDirectionChanges as AbruptDirectionChanges
import Tools.DeletePoints as DeletePoints
import Tools.Pointers as Pointers
import Tools.TrackInfoBox as TrackInfoBox
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (contrastingColour, neatToolsBorder, useIcon)
import ViewThirdPerson exposing (stopProp)


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
    | ToolAbruptDirectionChanges
    | ToolDeletePoints
    | ToolPointers


type alias Options =
    -- Tool specific options
    { tools : List ToolEntry
    , directionChangeOptions : AbruptDirectionChanges.Options
    , deleteOptions : DeletePoints.Options
    , pointerOptions : Pointers.Options
    }


defaultOptions : Options
defaultOptions =
    { tools = defaultTools
    , directionChangeOptions = AbruptDirectionChanges.defaultOptions
    , deleteOptions = DeletePoints.defaultOptions
    , pointerOptions = Pointers.defaultOptions
    }


type ToolMsg
    = ToolPopupToggle ToolType
    | ToolDockSelect ToolType ToolDock
    | ToolColourSelect ToolType Element.Color
    | ToolStateToggle ToolType ToolState
    | DirectionChanges AbruptDirectionChanges.Msg
    | DeletePoints DeletePoints.Msg
    | PointerMsg Pointers.Msg
    | ToolNoOp


type alias ToolEntry =
    { toolType : ToolType
    , label : String
    , info : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , tabColour : Element.Color
    , textColour : Element.Color
    , isPopupOpen : Bool
    }


defaultTools : List ToolEntry
defaultTools =
    -- One list or five, or six? Try one. Arguably a Dict but POITROAE.
    [ pointersTool
    , trackInfoBox
    , directionChangeTool
    , deleteTool
    ]


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Summary info"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperLeft
    , tabColour = FlatColors.AussiePalette.beekeeper
    , textColour = contrastingColour FlatColors.AussiePalette.beekeeper
    , isPopupOpen = False
    }


directionChangeTool : ToolEntry
directionChangeTool =
    { toolType = ToolAbruptDirectionChanges
    , label = "Direction changes"
    , info = "These may need smoothing"
    , video = Nothing
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.AussiePalette.deepKoamaru
    , textColour = contrastingColour FlatColors.AussiePalette.deepKoamaru
    , isPopupOpen = False
    }


pointersTool : ToolEntry
pointersTool =
    { toolType = ToolPointers
    , label = "Pointers"
    , info = "Use to bracket edits"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperRight
    , tabColour = FlatColors.AussiePalette.quinceJelly
    , textColour = contrastingColour FlatColors.AussiePalette.quinceJelly
    , isPopupOpen = False
    }


deleteTool : ToolEntry
deleteTool =
    { toolType = ToolDeletePoints
    , label = "Delete points"
    , info = "Away with ye"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.AussiePalette.pinkGlamour
    , textColour = contrastingColour FlatColors.AussiePalette.pinkGlamour
    , isPopupOpen = False
    }


toggleToolPopup : ToolType -> ToolEntry -> ToolEntry
toggleToolPopup toolType tool =
    if tool.toolType == toolType then
        { tool | isPopupOpen = not tool.isPopupOpen }

    else
        tool


setToolState : ToolType -> ToolState -> ToolEntry -> ToolEntry
setToolState toolType state tool =
    if tool.toolType == toolType then
        { tool | state = state }

    else
        tool


nextToolState : ToolState -> ToolState
nextToolState state =
    case state of
        Expanded ->
            Contracted

        Contracted ->
            Expanded

        Disabled ->
            Disabled


setDock : ToolType -> ToolDock -> ToolEntry -> ToolEntry
setDock toolType dock tool =
    if tool.toolType == toolType then
        { tool | dock = dock }

    else
        tool


setColour : ToolType -> Element.Color -> ToolEntry -> ToolEntry
setColour toolType colour tool =
    if tool.toolType == toolType then
        { tool
            | tabColour = colour
            , textColour = contrastingColour colour
        }

    else
        tool


getColour : ToolType -> List ToolEntry -> Element.Color
getColour toolType entries =
    entries
        |> List.Extra.find (\tab -> tab.toolType == toolType)
        |> Maybe.map .tabColour
        |> Maybe.withDefault FlatColors.SwedishPalette.freeSpeechBlue


update :
    ToolMsg
    -> Maybe TrackLoaded
    -> (ToolMsg -> msg)
    -> Options
    -> ( Options, List (ToolAction msg) )
update toolMsg isTrack msgWrapper options =
    case toolMsg of
        ToolNoOp ->
            ( options, [] )

        ToolPopupToggle toolType ->
            ( { options | tools = List.map (toggleToolPopup toolType) options.tools }
            , []
            )

        ToolDockSelect toolType toolDock ->
            ( { options | tools = List.map (setDock toolType toolDock) options.tools }
            , []
            )

        ToolColourSelect toolType color ->
            -- Instantly reflect colour changes in preview.
            { options | tools = List.map (setColour toolType color) options.tools }
                |> toolStateHasChanged toolType Expanded isTrack

        ToolStateToggle toolType newState ->
            -- Record the new state, but also let the tool know!
            { options | tools = List.map (setToolState toolType newState) options.tools }
                |> toolStateHasChanged toolType newState isTrack

        DirectionChanges msg ->
            -- Delegate to tool here...
            let
                ( newOptions, actions ) =
                    AbruptDirectionChanges.update
                        msg
                        options.directionChangeOptions
                        (getColour ToolAbruptDirectionChanges options.tools)
                        isTrack
            in
            ( { options | directionChangeOptions = newOptions }
            , actions
            )

        DeletePoints msg ->
            -- Delegate to tool here...
            let
                ( newOptions, actions ) =
                    DeletePoints.update
                        msg
                        options.deleteOptions
                        (getColour ToolDeletePoints options.tools)
                        isTrack
            in
            ( { options | deleteOptions = newOptions }
            , actions
            )

        PointerMsg msg ->
            ( options, [] )


refreshOpenTools :
    Maybe TrackLoaded
    -> Options
    -> ( Options, List (ToolAction msg) )
refreshOpenTools isTrack options =
    -- Track, or something has changed; tool data is stale.
    -- Same impact as tools being opened, so we'll re-use that.
    let
        refreshOpenTool entry ( inputOptions, collectingActions ) =
            if entry.state == Expanded then
                let
                    ( incrementalModel, incrementalActions ) =
                        toolStateHasChanged entry.toolType Expanded isTrack inputOptions
                in
                ( incrementalModel, incrementalActions ++ collectingActions )

            else
                ( inputOptions, collectingActions )
    in
    options.tools |> List.foldl refreshOpenTool ( options, [] )


toolStateHasChanged :
    ToolType
    -> ToolState
    -> Maybe TrackLoaded
    -> Options
    -> ( Options, List (ToolAction msg) )
toolStateHasChanged toolType newState isTrack options =
    case toolType of
        ToolTrackInfo ->
            ( options, [] )

        ToolAbruptDirectionChanges ->
            -- Would like an OO style dispatch table here but what with each tool
            -- having its own options, that's more tricky than it's worth.
            let
                ( newToolOptions, actions ) =
                    AbruptDirectionChanges.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.directionChangeOptions
                        isTrack

                newOptions =
                    { options | directionChangeOptions = newToolOptions }
            in
            ( newOptions, actions )

        ToolDeletePoints ->
            let
                ( newToolOptions, actions ) =
                    DeletePoints.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.deleteOptions
                        isTrack

                newOptions =
                    { options | deleteOptions = newToolOptions }
            in
            ( newOptions, actions )

        ToolPointers ->
            ( options, [] )



--View stuff


toolsForDock :
    ToolDock
    -> (ToolMsg -> msg)
    -> Maybe TrackLoaded
    -> Options
    -> Element msg
toolsForDock dock msgWrapper isTrack options =
    wrappedRow [ spacing 4, scrollbarY, height fill ] <|
        (options.tools
            |> List.filter (\t -> t.dock == dock)
            |> List.map (viewTool msgWrapper isTrack options)
        )


viewTool :
    (ToolMsg -> msg)
    -> Maybe TrackLoaded
    -> Options
    -> ToolEntry
    -> Element msg
viewTool msgWrapper isTrack options toolEntry =
    column
        [ width fill
        , alignTop
        , htmlAttribute (style "vertical-align" "top")
        , spacing 0
        , Border.width 2
        , Border.color toolEntry.tabColour
        , Border.rounded 8
        , inFront <|
            column
                [ alignRight
                , moveDown 26
                , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ToolNoOp >> msgWrapper)
                , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ToolNoOp >> msgWrapper)
                , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ToolNoOp >> msgWrapper)
                , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ToolNoOp >> msgWrapper)
                , htmlAttribute (style "z-index" "20")
                ]
                [ showDockOptions msgWrapper toolEntry
                , showColourOptions msgWrapper toolEntry
                ]
        ]
        [ row
            [ width fill
            , spacing 8
            , padding 4
            , Background.color toolEntry.tabColour
            , Font.color toolEntry.textColour
            ]
            [ Input.button [ centerX ]
                { onPress =
                    Just <|
                        msgWrapper <|
                            ToolStateToggle toolEntry.toolType <|
                                nextToolState toolEntry.state
                , label = text toolEntry.label
                }
            , Input.button [ alignRight ]
                { onPress = Just <| msgWrapper <| ToolPopupToggle toolEntry.toolType
                , label = useIcon FeatherIcons.settings
                }
            ]
        , if toolEntry.state == Expanded then
            viewToolByType msgWrapper toolEntry isTrack options

          else
            none
        ]


showDockOptions : (ToolMsg -> msg) -> ToolEntry -> Element msg
showDockOptions msgWrapper toolEntry =
    if toolEntry.isPopupOpen then
        row
            neatToolsBorder
            [ Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperLeft
                , label = useIcon FeatherIcons.arrowUpLeft
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockLowerLeft
                , label = useIcon FeatherIcons.arrowDownLeft
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockBottom
                , label = useIcon FeatherIcons.arrowDown
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockLowerRight
                , label = useIcon FeatherIcons.arrowDownRight
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperRight
                , label = useIcon FeatherIcons.arrowUpRight
                }
            ]

    else
        none


showColourOptions : (ToolMsg -> msg) -> ToolEntry -> Element msg
showColourOptions msgWrapper toolEntry =
    let
        colourBlock colour =
            Input.button
                [ Background.color colour, width <| px 20, height <| px 20 ]
                { label = none
                , onPress = Just <| msgWrapper <| ToolColourSelect toolEntry.toolType colour
                }
    in
    if toolEntry.isPopupOpen then
        column
            (alignRight :: neatToolsBorder)
            [ row []
                [ colourBlock FlatColors.SwedishPalette.highlighterPink
                , colourBlock FlatColors.SwedishPalette.darkPeriwinkle
                , colourBlock FlatColors.SwedishPalette.megaman
                , colourBlock FlatColors.SwedishPalette.freshTurquoise
                , colourBlock FlatColors.SwedishPalette.mintyGreen
                ]
            , row []
                [ colourBlock FlatColors.SwedishPalette.sizzlingRed
                , colourBlock FlatColors.SwedishPalette.freeSpeechBlue
                , colourBlock FlatColors.SwedishPalette.spiroDiscoBall
                , colourBlock FlatColors.SwedishPalette.jadeDust
                , colourBlock FlatColors.SwedishPalette.greenTeal
                ]
            , row []
                [ colourBlock FlatColors.SwedishPalette.narenjiOrange
                , colourBlock FlatColors.SwedishPalette.yrielYellow
                , colourBlock FlatColors.SwedishPalette.sunsetOrange
                , colourBlock FlatColors.SwedishPalette.hintOfElusiveBlue
                , colourBlock FlatColors.SwedishPalette.goodNight
                ]
            , row []
                [ colourBlock FlatColors.SwedishPalette.chromeYellow
                , colourBlock FlatColors.SwedishPalette.vibrantYellow
                , colourBlock FlatColors.SwedishPalette.redOrange
                , colourBlock FlatColors.SwedishPalette.londonSquare
                , colourBlock FlatColors.SwedishPalette.blackPearl
                ]
            ]

    else
        none


viewToolByType :
    (ToolMsg -> msg)
    -> ToolEntry
    -> Maybe TrackLoaded
    -> Options
    -> Element msg
viewToolByType msgWrapper entry isTrack options =
    case entry.toolType of
        ToolTrackInfo ->
            TrackInfoBox.trackInfoBox isTrack

        ToolAbruptDirectionChanges ->
            AbruptDirectionChanges.view (msgWrapper << DirectionChanges) options.directionChangeOptions

        ToolDeletePoints ->
            DeletePoints.view (msgWrapper << DeletePoints) options.deleteOptions

        ToolPointers ->
            Pointers.view (msgWrapper << PointerMsg) options.pointerOptions
