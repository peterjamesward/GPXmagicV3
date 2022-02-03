module ToolsController exposing (..)

import Actions exposing (ToolAction(..))
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
import Json.Decode as D exposing (field)
import Json.Encode as E
import List.Extra
import Tools.AbruptDirectionChanges as AbruptDirectionChanges
import Tools.BezierSplines
import Tools.DeletePoints as DeletePoints
import Tools.Pointers as Pointers
import Tools.TrackInfoBox as TrackInfoBox
import Tools.UndoRedo as UndoRedo
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
    | ToolUndoRedo
    | ToolBezierSplines


type alias Options =
    -- Tool specific options
    { tools : List ToolEntry
    , directionChangeOptions : AbruptDirectionChanges.Options
    , deleteOptions : DeletePoints.Options
    , pointerOptions : Pointers.Options
    , undoRedoOptions : UndoRedo.Options
    , imperial : Bool
    , bezierSplineOptions : Tools.BezierSplines.Options
    }


defaultOptions : Options
defaultOptions =
    { tools = defaultTools
    , directionChangeOptions = AbruptDirectionChanges.defaultOptions
    , deleteOptions = DeletePoints.defaultOptions
    , pointerOptions = Pointers.defaultOptions
    , undoRedoOptions = UndoRedo.defaultOptions
    , imperial = False
    , bezierSplineOptions = Tools.BezierSplines.defaultOptions
    }


type ToolMsg
    = ToolPopupToggle ToolType
    | ToolDockSelect ToolType ToolDock
    | ToolColourSelect ToolType Element.Color
    | ToolStateToggle ToolType ToolState
    | DirectionChanges AbruptDirectionChanges.Msg
    | DeletePoints DeletePoints.Msg
    | PointerMsg Pointers.Msg
    | UndoRedoMsg UndoRedo.Msg
    | ToggleImperial
    | ToolNoOp
    | ToolBezierMsg Tools.BezierSplines.Msg


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
    , undoRedoTool
    , trackInfoBox
    , directionChangeTool
    , deleteTool
    , bezierSplinesTool
    ]


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Summary info"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperLeft
    , tabColour = FlatColors.SwedishPalette.blackPearl
    , textColour = contrastingColour FlatColors.SwedishPalette.blackPearl
    , isPopupOpen = False
    }


undoRedoTool : ToolEntry
undoRedoTool =
    { toolType = ToolUndoRedo
    , label = "Undo & Redo"
    , info = "Like time travel"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperRight
    , tabColour = FlatColors.SwedishPalette.blackPearl
    , textColour = contrastingColour FlatColors.SwedishPalette.blackPearl
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
    , tabColour = FlatColors.SwedishPalette.blackPearl
    , textColour = contrastingColour FlatColors.SwedishPalette.blackPearl
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
    , tabColour = FlatColors.SwedishPalette.blackPearl
    , textColour = contrastingColour FlatColors.SwedishPalette.blackPearl
    , isPopupOpen = False
    }


bezierSplinesTool : ToolEntry
bezierSplinesTool =
    { toolType = ToolBezierSplines
    , label = "Bezier splines"
    , info = "Make it smoother"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerRight
    , tabColour = FlatColors.SwedishPalette.blackPearl
    , textColour = contrastingColour FlatColors.SwedishPalette.blackPearl
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


isToolOpen : ToolType -> List ToolEntry -> Bool
isToolOpen toolType entries =
    List.Extra.find
        (\tab -> tab.toolType == toolType && tab.state == Expanded)
        entries
        /= Nothing


update :
    ToolMsg
    -> Maybe (TrackLoaded msg)
    -> (ToolMsg -> msg)
    -> Options
    -> ( Options, List (ToolAction msg) )
update toolMsg isTrack msgWrapper options =
    case toolMsg of
        ToolNoOp ->
            ( options, [] )

        ToolPopupToggle toolType ->
            ( { options | tools = List.map (toggleToolPopup toolType) options.tools }
            , [ StoreLocally "tools" <| encodeToolState options ]
            )

        ToolDockSelect toolType toolDock ->
            ( { options | tools = List.map (setDock toolType toolDock) options.tools }
            , [ StoreLocally "tools" <| encodeToolState options ]
            )

        ToolColourSelect toolType color ->
            -- Instantly reflect colour changes in preview.
            let
                newOptions =
                    { options | tools = List.map (setColour toolType color) options.tools }
            in
            if isToolOpen toolType options.tools then
                toolStateHasChanged toolType Expanded isTrack newOptions

            else
                ( newOptions, [ StoreLocally "tools" <| encodeToolState options ] )

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
            let
                ( newOptions, actions ) =
                    Pointers.update
                        msg
                        options.pointerOptions
                        (getColour ToolPointers options.tools)
                        isTrack
            in
            ( { options | pointerOptions = newOptions }
            , actions
            )

        UndoRedoMsg msg ->
            let
                ( newOptions, actions ) =
                    UndoRedo.update
                        msg
                        options.undoRedoOptions
                        (getColour ToolUndoRedo options.tools)
                        isTrack
            in
            ( { options | undoRedoOptions = newOptions }
            , actions
            )

        ToolBezierMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.BezierSplines.update
                        msg
                        options.bezierSplineOptions
                        (getColour ToolBezierSplines options.tools)
                        isTrack
            in
            ( { options | bezierSplineOptions = newOptions }
            , actions
            )

        ToggleImperial ->
            let
                newOptions =
                    { options | imperial = not options.imperial }
            in
            ( newOptions, [ StoreLocally "measure" <| E.bool newOptions.imperial ] )


refreshOpenTools :
    Maybe (TrackLoaded msg)
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
    -> Maybe (TrackLoaded msg)
    -> Options
    -> ( Options, List (ToolAction msg) )
toolStateHasChanged toolType newState isTrack options =
    case toolType of
        ToolTrackInfo ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

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
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

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
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolPointers ->
            let
                ( newToolOptions, actions ) =
                    Pointers.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.pointerOptions
                        isTrack

                newOptions =
                    { options | pointerOptions = newToolOptions }
            in
            ( newOptions, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolUndoRedo ->
            ( options, [] )

        ToolBezierSplines ->
            let
                ( newToolOptions, actions ) =
                    Tools.BezierSplines.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.bezierSplineOptions
                        isTrack

                newOptions =
                    { options | bezierSplineOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )



--View stuff


toolsForDock :
    ToolDock
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options
    -> Element msg
toolsForDock dock msgWrapper isTrack options =
    wrappedRow
        [ spacing 4 ]
    <|
        (options.tools
            |> List.filter (\t -> t.dock == dock)
            |> List.map (viewTool msgWrapper isTrack options)
        )


viewTool :
    (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options
    -> ToolEntry
    -> Element msg
viewTool msgWrapper isTrack options toolEntry =
    column
        [ width fill
        , alignTop
        , htmlAttribute (style "vertical-align" "top")
        , spacing 0
        , Border.width 4
        , Border.color toolEntry.tabColour
        , Border.rounded 8
        , Background.color toolEntry.tabColour
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
            , height <| px 24

            --, padding 4
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
        , el [ Border.rounded 8, width fill, height fill ] <|
            if toolEntry.state == Expanded then
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
    -> Maybe (TrackLoaded msg)
    -> Options
    -> Element msg
viewToolByType msgWrapper entry isTrack options =
    el
        [ centerX, padding 2, width fill, Border.rounded 4 ]
    <|
        case entry.toolType of
            ToolTrackInfo ->
                TrackInfoBox.trackInfoBox isTrack options.imperial

            ToolAbruptDirectionChanges ->
                AbruptDirectionChanges.view (msgWrapper << DirectionChanges) options.directionChangeOptions

            ToolDeletePoints ->
                DeletePoints.view (msgWrapper << DeletePoints) options.deleteOptions

            ToolPointers ->
                Pointers.view (msgWrapper << PointerMsg) options.pointerOptions isTrack

            ToolUndoRedo ->
                UndoRedo.view (msgWrapper << UndoRedoMsg) options.undoRedoOptions isTrack

            ToolBezierSplines ->
                Tools.BezierSplines.view (msgWrapper << ToolBezierMsg) options.bezierSplineOptions

-- Local storage management


type alias StoredTool =
    { toolType : String
    , state : String
    , dock : String
    , tab : ColourTriplet
    , text : ColourTriplet
    }


type alias ColourTriplet =
    { red : Float
    , green : Float
    , blue : Float
    }


encodeType : ToolType -> String
encodeType toolType =
    case toolType of
        ToolTrackInfo ->
            "ToolTrackInfo"

        ToolAbruptDirectionChanges ->
            "ToolAbruptDirectionChanges"

        ToolDeletePoints ->
            "ToolDeletePoints"

        ToolPointers ->
            "ToolPointers"

        ToolUndoRedo ->
            "ToolUndoRedo"

        ToolBezierSplines ->
            "ToolBezierSplines"


encodeColour : Element.Color -> E.Value
encodeColour colour =
    let
        { red, green, blue, alpha } =
            toRgb colour
    in
    E.object
        [ ( "red", E.float red )
        , ( "green", E.float green )
        , ( "blue", E.float blue )
        ]


decodeColour : ColourTriplet -> Element.Color
decodeColour { red, green, blue } =
    Element.fromRgb
        { red = red
        , green = green
        , blue = blue
        , alpha = 1.0
        }


encodeState : ToolState -> String
encodeState state =
    case state of
        Expanded ->
            "expanded"

        Contracted ->
            "contracted"

        Disabled ->
            "disabled"


decodeState : String -> ToolState
decodeState state =
    case state of
        "expanded" ->
            Expanded

        "contracted" ->
            Contracted

        "disabled" ->
            Disabled

        _ ->
            Contracted


encodeDock : ToolDock -> String
encodeDock dock =
    case dock of
        DockUpperLeft ->
            "upperleft"

        DockLowerLeft ->
            "lowerleft"

        DockUpperRight ->
            "upperright"

        DockLowerRight ->
            "lowerright"

        DockBottom ->
            "bottom"

        DockNone ->
            "none"


decodeDock : String -> ToolDock
decodeDock dock =
    case dock of
        "upperleft" ->
            DockUpperLeft

        "lowerleft" ->
            DockLowerLeft

        "upperright" ->
            DockUpperRight

        "lowerright" ->
            DockLowerRight

        "bottom" ->
            DockBottom

        "none" ->
            DockNone

        _ ->
            DockUpperRight


encodeOneTool : ToolEntry -> E.Value
encodeOneTool tool =
    E.object
        [ ( "type", E.string <| encodeType tool.toolType )
        , ( "state", E.string <| encodeState tool.state )
        , ( "dock", E.string <| encodeDock tool.dock )
        , ( "tab", encodeColour tool.tabColour )
        , ( "text", encodeColour tool.textColour )
        ]


encodeToolState : Options -> E.Value
encodeToolState options =
    E.list identity <| List.map encodeOneTool options.tools


colourDecoder =
    D.map3 ColourTriplet
        (field "red" D.float)
        (field "green" D.float)
        (field "blue" D.float)


toolDecoder =
    D.map5 StoredTool
        (field "type" D.string)
        (field "state" D.string)
        (field "dock" D.string)
        (field "tab" colourDecoder)
        (field "text" colourDecoder)


restoreStoredValues : Options -> D.Value -> Options
restoreStoredValues options values =
    -- Care! Need to overlay restored values on to the current tools.
    let
        toolsAsStored =
            D.decodeValue (D.list toolDecoder) values

        useStoredSettings : List StoredTool -> ToolEntry -> ToolEntry
        useStoredSettings stored tool =
            case List.Extra.find (\fromStore -> fromStore.toolType == encodeType tool.toolType) stored of
                Just found ->
                    { tool
                        | state = decodeState found.state
                        , dock = decodeDock found.dock
                        , tabColour = decodeColour found.tab
                        , textColour = decodeColour found.text
                    }

                Nothing ->
                    tool
    in
    case toolsAsStored of
        Ok stored ->
            { options | tools = List.map (useStoredSettings stored) options.tools }

        Err error ->
            options


restoreMeasure : Options -> D.Value -> Options
restoreMeasure options value =
    -- Care! Need to overlay restored values on to the current tools.
    let
        decoded =
            D.decodeValue D.bool value
    in
    case decoded of
        Ok setting ->
            { options | imperial = setting }

        Err error ->
            options


imperialToggleMenuEntry msgWrapper options =
    Input.button [ alignRight ]
        { onPress = Just <| msgWrapper ToggleImperial
        , label =
            if options.imperial then
                text "Use metric measures"

            else
                text "Use imperial measures"
        }
