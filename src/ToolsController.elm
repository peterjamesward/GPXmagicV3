module ToolsController exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (roundEach)
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.FlatUIPalette
import FlatColors.SwedishPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D exposing (field)
import Json.Encode as E exposing (string)
import List.Extra
import Time
import ToolTip exposing (myTooltip, tooltip)
import Tools.BendSmoother
import Tools.BendSmootherOptions
import Tools.BezierOptions
import Tools.BezierSplines
import Tools.CentroidAverage
import Tools.CentroidAverageOptions
import Tools.CurveFormer
import Tools.CurveFormerOptions
import Tools.DeletePoints as DeletePoints
import Tools.DirectionChanges as AbruptDirectionChanges
import Tools.DisplaySettings
import Tools.DisplaySettingsOptions
import Tools.Flythrough
import Tools.GradientProblems
import Tools.Interpolate
import Tools.InterpolateOptions
import Tools.LimitGradientOptions
import Tools.LimitGradients
import Tools.MoveScaleRotate
import Tools.MoveScaleRotateOptions
import Tools.Nudge
import Tools.NudgeOptions
import Tools.OutAndBack
import Tools.OutAndBackOptions
import Tools.Pointers as Pointers
import Tools.Simplify
import Tools.StravaTools
import Tools.TrackInfoBox as TrackInfoBox
import Tools.UndoRedo as UndoRedo
import TrackLoaded exposing (TrackLoaded)
import View3dCommonElements exposing (stopProp)
import ViewPureStyles exposing (..)


type ToolState
    = Expanded
    | Contracted
    | Disabled


type ToolType
    = ToolTrackInfo
    | ToolAbruptDirectionChanges
    | ToolDeletePoints
    | ToolPointers
    | ToolUndoRedo
    | ToolBezierSplines
    | ToolCentroidAverage
    | ToolCurveFormer
    | ToolBendSmoother
    | ToolNudge
    | ToolGradientProblems
    | ToolDisplaySettings
    | ToolOutAndBack
    | ToolSimplify
    | ToolInterpolate
    | ToolLimitGradient
    | ToolMoveScaleRotate
    | ToolFlythrough
    | ToolStrava


type alias Options =
    -- Tool specific options
    { tools : List ToolEntry
    , docks : Dict String DockSettings
    , directionChangeOptions : AbruptDirectionChanges.Options
    , deleteOptions : DeletePoints.Options
    , pointerOptions : Pointers.Options
    , undoRedoOptions : UndoRedo.Options
    , imperial : Bool
    , bezierSplineOptions : Tools.BezierOptions.Options
    , centroidAverageOptions : Tools.CentroidAverageOptions.Options
    , curveFormerOptions : Tools.CurveFormerOptions.Options
    , bendSmootherOptions : Tools.BendSmootherOptions.Options
    , nudgeOptions : Tools.NudgeOptions.Options
    , infoOptions : TrackInfoBox.Options
    , gradientProblemOptions : Tools.GradientProblems.Options
    , displaySettings : Tools.DisplaySettingsOptions.Options
    , outAndBackSettings : Tools.OutAndBackOptions.Options
    , simplifySettings : Tools.Simplify.Options
    , interpolateSettings : Tools.InterpolateOptions.Options
    , limitGradientSettings : Tools.LimitGradientOptions.Options
    , moveScaleRotateSettings : Tools.MoveScaleRotateOptions.Options
    , flythroughSettings : Tools.Flythrough.Options
    , stravaSettings : Tools.StravaTools.Options
    }


defaultOptions : Options
defaultOptions =
    { tools = defaultTools
    , docks = Dict.fromList dockList
    , directionChangeOptions = AbruptDirectionChanges.defaultOptions
    , deleteOptions = DeletePoints.defaultOptions
    , pointerOptions = Pointers.defaultOptions
    , undoRedoOptions = UndoRedo.defaultOptions
    , imperial = False
    , bezierSplineOptions = Tools.BezierSplines.defaultOptions
    , centroidAverageOptions = Tools.CentroidAverage.defaultOptions
    , curveFormerOptions = Tools.CurveFormer.defaultOptions
    , bendSmootherOptions = Tools.BendSmoother.defaultOptions
    , nudgeOptions = Tools.Nudge.defaultOptions
    , infoOptions = TrackInfoBox.defaultOptions
    , gradientProblemOptions = Tools.GradientProblems.defaultOptions
    , displaySettings = Tools.DisplaySettings.defaultOptions
    , outAndBackSettings = Tools.OutAndBack.defaultOptions
    , simplifySettings = Tools.Simplify.defaultOptions
    , interpolateSettings = Tools.Interpolate.defaultOptions
    , limitGradientSettings = Tools.LimitGradients.defaultOptions
    , moveScaleRotateSettings = Tools.MoveScaleRotate.defaultOptions
    , flythroughSettings = Tools.Flythrough.defaultOptions
    , stravaSettings = Tools.StravaTools.defaultOptions
    }


type ToolMsg
    = ToolPopupToggle ToolType
    | ToolDockSelect ToolType ToolDock
    | ToolColourSelect ToolType Element.Color
    | ToolStateToggle ToolType ToolState
    | DockPopupToggle String
    | DockNameChange String String
    | DirectionChanges AbruptDirectionChanges.Msg
    | DeletePoints DeletePoints.Msg
    | PointerMsg Pointers.Msg
    | UndoRedoMsg UndoRedo.Msg
    | ToggleImperial
    | ToolNoOp
    | ToolBezierMsg Tools.BezierSplines.Msg
    | ToolCentroidMsg Tools.CentroidAverage.Msg
    | ToolCurveFormerMsg Tools.CurveFormer.Msg
    | ToolBendSmootherMsg Tools.BendSmoother.Msg
    | ToolNudgeMsg Tools.Nudge.Msg
    | ToolInfoMsg TrackInfoBox.Msg
    | ToolGradientChangeMsg Tools.GradientProblems.Msg
    | ToolDisplaySettingMsg Tools.DisplaySettings.Msg
    | ToolOutAndBackMsg Tools.OutAndBack.Msg
    | ToolSimplifyMsg Tools.Simplify.Msg
    | ToolInterpolateMsg Tools.Interpolate.Msg
    | ToolLimitGradientMsg Tools.LimitGradients.Msg
    | ToolMoveScaleRotateMsg Tools.MoveScaleRotate.Msg
    | ToolFlythroughMsg Tools.Flythrough.Msg
    | ToolStravaMsg Tools.StravaTools.Msg


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
    , displaySettingsTool
    , directionChangeTool
    , gradientChangeTool
    , deleteTool
    , bezierSplinesTool
    , centroidAverageTool
    , curveFormerTool
    , bendSmootherTool
    , nudgeTool
    , outAndBackTool
    , simplifyTool
    , interpolateTool
    , limitGradientTool
    , moveScaleRotateTool
    , flythroughTool
    , stravaTool
    ]


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Information"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Contracted
    , dock = DockUpperLeft
    , tabColour = FlatColors.FlatUIPalette.peterRiver
    , textColour = contrastingColour FlatColors.FlatUIPalette.peterRiver
    , isPopupOpen = False
    }


displaySettingsTool : ToolEntry
displaySettingsTool =
    { toolType = ToolDisplaySettings
    , label = "Display"
    , info = "How it looks"
    , video = Nothing
    , state = Contracted
    , dock = DockUpperLeft
    , tabColour = FlatColors.FlatUIPalette.peterRiver
    , textColour = contrastingColour FlatColors.FlatUIPalette.peterRiver
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
    , tabColour = FlatColors.FlatUIPalette.sunFlower
    , textColour = contrastingColour FlatColors.FlatUIPalette.sunFlower
    , isPopupOpen = False
    }


directionChangeTool : ToolEntry
directionChangeTool =
    { toolType = ToolAbruptDirectionChanges
    , label = "Direction changes"
    , info = "These may need smoothing"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.peterRiver
    , textColour = contrastingColour FlatColors.FlatUIPalette.peterRiver
    , isPopupOpen = False
    }


gradientChangeTool : ToolEntry
gradientChangeTool =
    { toolType = ToolGradientProblems
    , label = "Gradient problems"
    , info = "These may need smoothing"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.peterRiver
    , textColour = contrastingColour FlatColors.FlatUIPalette.peterRiver
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
    , tabColour = FlatColors.FlatUIPalette.orange
    , textColour = contrastingColour FlatColors.FlatUIPalette.orange
    , isPopupOpen = False
    }


deleteTool : ToolEntry
deleteTool =
    { toolType = ToolDeletePoints
    , label = "Delete points"
    , info = "Away with ye"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerRight
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
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
    , tabColour = FlatColors.FlatUIPalette.amethyst
    , textColour = contrastingColour FlatColors.FlatUIPalette.amethyst
    , isPopupOpen = False
    }


centroidAverageTool : ToolEntry
centroidAverageTool =
    { toolType = ToolCentroidAverage
    , label = "Centroid Average"
    , info = "Make it smoother"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerRight
    , tabColour = FlatColors.FlatUIPalette.amethyst
    , textColour = contrastingColour FlatColors.FlatUIPalette.amethyst
    , isPopupOpen = False
    }


curveFormerTool : ToolEntry
curveFormerTool =
    { toolType = ToolCurveFormer
    , label = "Radiused bends"
    , info = "Make it smoother"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerRight
    , tabColour = FlatColors.FlatUIPalette.turquoise
    , textColour = contrastingColour FlatColors.FlatUIPalette.turquoise
    , isPopupOpen = False
    }


bendSmootherTool : ToolEntry
bendSmootherTool =
    { toolType = ToolBendSmoother
    , label = "Classic bends"
    , info = "Make it smoother"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerRight
    , tabColour = FlatColors.FlatUIPalette.greenSea
    , textColour = contrastingColour FlatColors.FlatUIPalette.greenSea
    , isPopupOpen = False
    }


nudgeTool : ToolEntry
nudgeTool =
    { toolType = ToolNudge
    , label = "Nudge"
    , info = "Make it smoother"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerRight
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


outAndBackTool : ToolEntry
outAndBackTool =
    { toolType = ToolOutAndBack
    , label = "Out and Back"
    , info = "ET go home"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


simplifyTool : ToolEntry
simplifyTool =
    { toolType = ToolSimplify
    , label = "Simplify"
    , info = "Reduce noise"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


interpolateTool : ToolEntry
interpolateTool =
    { toolType = ToolInterpolate
    , label = "Add points"
    , info = "Add points"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


limitGradientTool : ToolEntry
limitGradientTool =
    { toolType = ToolLimitGradient
    , label = "Limit Gradients"
    , info = "Limit Gradients"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


moveScaleRotateTool : ToolEntry
moveScaleRotateTool =
    { toolType = ToolMoveScaleRotate
    , label = "Move & Scale"
    , info = "Lift & Shifts"
    , video = Nothing
    , state = Contracted
    , dock = DockLowerLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


flythroughTool : ToolEntry
flythroughTool =
    { toolType = ToolFlythrough
    , label = "Fly-through"
    , info = "Fly-through"
    , video = Nothing
    , state = Contracted
    , dock = DockUpperLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


stravaTool : ToolEntry
stravaTool =
    { toolType = ToolStrava
    , label = "Strava"
    , info = "Strava"
    , video = Nothing
    , state = Contracted
    , dock = DockUpperLeft
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


toggleToolPopup : ToolType -> ToolEntry -> ToolEntry
toggleToolPopup toolType tool =
    if tool.toolType == toolType then
        { tool | isPopupOpen = not tool.isPopupOpen }

    else
        { tool | isPopupOpen = False }


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
        { tool | dock = dock, isPopupOpen = False }

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
            let
                newOptions =
                    { options | tools = List.map (toggleToolPopup toolType) options.tools }
            in
            ( newOptions
            , [ StoreLocally "tools" <| encodeToolState newOptions ]
            )

        ToolDockSelect toolType toolDock ->
            let
                newOptions =
                    { options | tools = List.map (setDock toolType toolDock) options.tools }
            in
            ( newOptions
            , [ StoreLocally "tools" <| encodeToolState newOptions ]
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
                ( newOptions, [ StoreLocally "tools" <| encodeToolState newOptions ] )

        ToolStateToggle toolType newState ->
            -- Record the new state, but also let the tool know!
            { options | tools = List.map (setToolState toolType newState) options.tools }
                |> toolStateHasChanged toolType newState isTrack

        DirectionChanges msg ->
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

        ToolGradientChangeMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.GradientProblems.update
                        msg
                        options.gradientProblemOptions
                        (getColour ToolGradientProblems options.tools)
                        isTrack
            in
            ( { options | gradientProblemOptions = newOptions }
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

        ToolCentroidMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.CentroidAverage.update
                        msg
                        options.centroidAverageOptions
                        (getColour ToolCentroidAverage options.tools)
                        isTrack
            in
            ( { options | centroidAverageOptions = newOptions }
            , actions
            )

        ToolCurveFormerMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.CurveFormer.update
                        msg
                        options.curveFormerOptions
                        (getColour ToolCurveFormer options.tools)
                        isTrack
            in
            ( { options | curveFormerOptions = newOptions }
            , actions
            )

        ToolBendSmootherMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.BendSmoother.update
                        msg
                        options.bendSmootherOptions
                        (getColour ToolBendSmoother options.tools)
                        isTrack
            in
            ( { options | bendSmootherOptions = newOptions }
            , actions
            )

        ToolNudgeMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Nudge.update
                        msg
                        options.nudgeOptions
                        (getColour ToolNudge options.tools)
                        isTrack
            in
            ( { options | nudgeOptions = newOptions }
            , actions
            )

        ToggleImperial ->
            let
                newOptions =
                    { options | imperial = not options.imperial }
            in
            ( newOptions, [ StoreLocally "measure" <| E.bool newOptions.imperial ] )

        ToolInfoMsg infoMsg ->
            let
                newOptions =
                    TrackInfoBox.update
                        infoMsg
                        options.infoOptions
            in
            ( { options | infoOptions = newOptions }
            , []
            )

        ToolDisplaySettingMsg m ->
            let
                ( newOptions, actions ) =
                    Tools.DisplaySettings.update
                        m
                        options.displaySettings
            in
            ( { options | displaySettings = newOptions }
            , actions
            )

        ToolOutAndBackMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.OutAndBack.update
                        msg
                        options.outAndBackSettings
                        isTrack
            in
            ( { options | outAndBackSettings = newOptions }
            , actions
            )

        ToolSimplifyMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Simplify.update
                        msg
                        options.simplifySettings
                        (getColour ToolSimplify options.tools)
                        isTrack
            in
            ( { options | simplifySettings = newOptions }
            , actions
            )

        ToolInterpolateMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Interpolate.update
                        msg
                        options.interpolateSettings
                        (getColour ToolInterpolate options.tools)
                        isTrack
            in
            ( { options | interpolateSettings = newOptions }
            , actions
            )

        ToolLimitGradientMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.LimitGradients.update
                        msg
                        options.limitGradientSettings
                        (getColour ToolLimitGradient options.tools)
                        isTrack
            in
            ( { options | limitGradientSettings = newOptions }
            , actions
            )

        DockPopupToggle id ->
            case Dict.get id options.docks of
                Just dock ->
                    let
                        newDocks =
                            Dict.insert id
                                { dock | dockPopupOpen = not dock.dockPopupOpen }
                                options.docks

                        newOptions =
                            { options | docks = newDocks }
                    in
                    ( newOptions, [] )

                Nothing ->
                    ( options, [] )

        DockNameChange int string ->
            case Dict.get int options.docks of
                Just dock ->
                    let
                        newDocks =
                            Dict.insert int
                                { dock | dockLabel = string }
                                options.docks

                        newOptions =
                            { options | docks = newDocks }
                    in
                    ( newOptions, [ StoreLocally "docks" <| encodeDockState options.docks ] )

                Nothing ->
                    ( options, [] )

        ToolMoveScaleRotateMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.MoveScaleRotate.update
                        msg
                        options.moveScaleRotateSettings
                        (getColour ToolMoveScaleRotate options.tools)
                        isTrack
            in
            ( { options | moveScaleRotateSettings = newOptions }
            , actions
            )

        ToolFlythroughMsg flyMsg ->
            let
                ( newOptions, actions ) =
                    case isTrack of
                        Just track ->
                            Tools.Flythrough.update
                                options.flythroughSettings
                                flyMsg
                                track

                        Nothing ->
                            ( options.flythroughSettings, [] )
            in
            ( { options | flythroughSettings = newOptions }
            , actions
            )

        ToolStravaMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.StravaTools.update
                        msg
                        options.stravaSettings
                        (ToolStravaMsg >> msgWrapper)
                        isTrack
            in
            ( { options | stravaSettings = newOptions }
            , actions
            )


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

        ToolCentroidAverage ->
            let
                ( newToolOptions, actions ) =
                    Tools.CentroidAverage.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.centroidAverageOptions
                        isTrack

                newOptions =
                    { options | centroidAverageOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolCurveFormer ->
            let
                ( newToolOptions, actions ) =
                    Tools.CurveFormer.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.curveFormerOptions
                        isTrack

                newOptions =
                    { options | curveFormerOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolBendSmoother ->
            let
                ( newToolOptions, actions ) =
                    Tools.BendSmoother.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.bendSmootherOptions
                        isTrack

                newOptions =
                    { options | bendSmootherOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolNudge ->
            let
                ( newToolOptions, actions ) =
                    Tools.Nudge.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.nudgeOptions
                        isTrack

                newOptions =
                    { options | nudgeOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolGradientProblems ->
            let
                ( newToolOptions, actions ) =
                    Tools.GradientProblems.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.gradientProblemOptions
                        isTrack

                newOptions =
                    { options | gradientProblemOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolDisplaySettings ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolOutAndBack ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolSimplify ->
            let
                ( newToolOptions, actions ) =
                    Tools.Simplify.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.simplifySettings
                        isTrack

                newOptions =
                    { options | simplifySettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolInterpolate ->
            let
                ( newToolOptions, actions ) =
                    Tools.Interpolate.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.interpolateSettings
                        isTrack

                newOptions =
                    { options | interpolateSettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolLimitGradient ->
            let
                ( newToolOptions, actions ) =
                    Tools.LimitGradients.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.limitGradientSettings
                        isTrack

                newOptions =
                    { options | limitGradientSettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolMoveScaleRotate ->
            let
                ( newToolOptions, actions ) =
                    Tools.MoveScaleRotate.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.moveScaleRotateSettings
                        isTrack

                newOptions =
                    { options | moveScaleRotateSettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolFlythrough ->
            let
                ( newToolOptions, actions ) =
                    Tools.Flythrough.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.flythroughSettings
                        isTrack

                newOptions =
                    { options | flythroughSettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolStrava ->
            let
                ( newToolOptions, actions ) =
                    Tools.StravaTools.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.stravaSettings
                        isTrack

                newOptions =
                    { options | stravaSettings = newToolOptions }
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
    column [ width fill, spacing 5 ]
        [ showDockHeader msgWrapper dock options.docks
        , wrappedRow
            [ spacing 4, width fill ]
          <|
            (options.tools
                |> List.filter (\t -> t.dock == dock)
                |> List.map (viewTool msgWrapper isTrack options)
            )
        ]


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
            [ Input.button
                [ centerX
                , tooltip below <|
                    myTooltip <|
                        case toolEntry.state of
                            Expanded ->
                                "Click to close"

                            Contracted ->
                                "Click to open"

                            Disabled ->
                                "Disabled"
                ]
                { onPress =
                    Just <|
                        msgWrapper <|
                            ToolStateToggle toolEntry.toolType <|
                                nextToolState toolEntry.state
                , label = text toolEntry.label
                }
            , Input.button [ alignRight ]
                { onPress = Just <| msgWrapper <| ToolPopupToggle toolEntry.toolType
                , label = useIconWithSize 14 FeatherIcons.settings
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
                [ tooltip below (myTooltip "Move to upper left") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperLeft
                , label = useIcon FeatherIcons.arrowUpLeft
                }
            , Input.button
                [ tooltip below (myTooltip "Move to lower left") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockLowerLeft
                , label = useIcon FeatherIcons.arrowDownLeft
                }
            , Input.button
                [ tooltip below (myTooltip "Move to bottom centre") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockBottom
                , label = useIcon FeatherIcons.arrowDown
                }
            , Input.button
                [ tooltip below (myTooltip "Move to lower right") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockLowerRight
                , label = useIcon FeatherIcons.arrowDownRight
                }
            , Input.button
                [ tooltip below (myTooltip "Move to upper right") ]
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
                [ colourBlock FlatColors.FlatUIPalette.turquoise
                , colourBlock FlatColors.FlatUIPalette.emerald
                , colourBlock FlatColors.FlatUIPalette.peterRiver
                , colourBlock FlatColors.FlatUIPalette.amethyst
                , colourBlock FlatColors.FlatUIPalette.wetAsphalt
                ]
            , row []
                [ colourBlock FlatColors.FlatUIPalette.greenSea
                , colourBlock FlatColors.FlatUIPalette.nephritis
                , colourBlock FlatColors.FlatUIPalette.belizeHole
                , colourBlock FlatColors.FlatUIPalette.wisteria
                , colourBlock FlatColors.FlatUIPalette.midnightBlue
                ]
            , row []
                [ colourBlock FlatColors.FlatUIPalette.sunFlower
                , colourBlock FlatColors.FlatUIPalette.carrot
                , colourBlock FlatColors.FlatUIPalette.alizarin
                , colourBlock FlatColors.FlatUIPalette.clouds
                , colourBlock FlatColors.FlatUIPalette.concrete
                ]
            , row []
                [ colourBlock FlatColors.FlatUIPalette.orange
                , colourBlock FlatColors.FlatUIPalette.pumpkin
                , colourBlock FlatColors.FlatUIPalette.pomegranate
                , colourBlock FlatColors.FlatUIPalette.silver
                , colourBlock FlatColors.FlatUIPalette.asbestos
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
                TrackInfoBox.view (msgWrapper << ToolInfoMsg) options.imperial isTrack options.infoOptions

            ToolAbruptDirectionChanges ->
                AbruptDirectionChanges.view
                    options.imperial
                    (msgWrapper << DirectionChanges)
                    options.directionChangeOptions
                    isTrack

            ToolGradientProblems ->
                Tools.GradientProblems.view
                    (msgWrapper << ToolGradientChangeMsg)
                    options.gradientProblemOptions
                    isTrack

            ToolDeletePoints ->
                DeletePoints.view (msgWrapper << DeletePoints) options.deleteOptions

            ToolPointers ->
                Pointers.view
                    options.imperial
                    (msgWrapper << PointerMsg)
                    options.pointerOptions
                    isTrack

            ToolUndoRedo ->
                UndoRedo.view (msgWrapper << UndoRedoMsg) options.undoRedoOptions isTrack

            ToolBezierSplines ->
                Tools.BezierSplines.view (msgWrapper << ToolBezierMsg) options.bezierSplineOptions

            ToolCentroidAverage ->
                Tools.CentroidAverage.view (msgWrapper << ToolCentroidMsg) options.centroidAverageOptions

            ToolCurveFormer ->
                Tools.CurveFormer.view
                    options.imperial
                    (msgWrapper << ToolCurveFormerMsg)
                    options.curveFormerOptions
                    isTrack

            ToolBendSmoother ->
                Tools.BendSmoother.view
                    options.imperial
                    (msgWrapper << ToolBendSmootherMsg)
                    options.bendSmootherOptions
                    isTrack

            ToolNudge ->
                Tools.Nudge.view
                    options.imperial
                    options.nudgeOptions
                    (msgWrapper << ToolNudgeMsg)
                    isTrack

            ToolDisplaySettings ->
                Tools.DisplaySettings.view
                    (msgWrapper << ToolDisplaySettingMsg)
                    options.displaySettings

            ToolOutAndBack ->
                Tools.OutAndBack.view
                    options.imperial
                    (msgWrapper << ToolOutAndBackMsg)
                    options.outAndBackSettings
                    isTrack

            ToolSimplify ->
                Tools.Simplify.view
                    (msgWrapper << ToolSimplifyMsg)
                    options.simplifySettings
                    isTrack

            ToolInterpolate ->
                Tools.Interpolate.view
                    options.imperial
                    (msgWrapper << ToolInterpolateMsg)
                    options.interpolateSettings
                    isTrack

            ToolLimitGradient ->
                Tools.LimitGradients.view
                    options.limitGradientSettings
                    (msgWrapper << ToolLimitGradientMsg)

            ToolMoveScaleRotate ->
                Tools.MoveScaleRotate.view
                    options.imperial
                    options.moveScaleRotateSettings
                    (msgWrapper << ToolMoveScaleRotateMsg)
                    isTrack

            ToolFlythrough ->
                Tools.Flythrough.view
                    options.imperial
                    options.flythroughSettings
                    (msgWrapper << ToolFlythroughMsg)

            ToolStrava ->
                Tools.StravaTools.viewStravaTab
                    options.stravaSettings
                    (msgWrapper << ToolStravaMsg)
                    isTrack



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

        ToolCentroidAverage ->
            "ToolCentroidAverage"

        ToolCurveFormer ->
            "ToolCurveFormer"

        ToolBendSmoother ->
            "ToolBendSmoother"

        ToolNudge ->
            "ToolNudge"

        ToolGradientProblems ->
            "ToolAbruptGradientChanges"

        ToolDisplaySettings ->
            "ToolDisplaySettings"

        ToolOutAndBack ->
            "ToolOutAndBack"

        ToolSimplify ->
            "ToolSimplify"

        ToolInterpolate ->
            "ToolInterpolate"

        ToolLimitGradient ->
            "ToolLimitGradient"

        ToolMoveScaleRotate ->
            "ToolMoveScaleRotate"

        ToolFlythrough ->
            "ToolFlythrough"

        ToolStrava ->
            "ToolStrava"


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


encodeDockState : Dict String DockSettings -> E.Value
encodeDockState docks =
    docks
        |> Dict.map (\k v -> v.dockLabel)
        |> E.dict identity E.string


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


restoreDockSettings : Options -> D.Value -> Options
restoreDockSettings options values =
    let
        storedSettings =
            D.decodeValue (D.dict D.string) values

        useStoredSettings : Dict String String -> Dict String DockSettings
        useStoredSettings settings =
            Dict.foldl updateDock options.docks settings

        updateDock : String -> String -> Dict String DockSettings -> Dict String DockSettings
        updateDock k v dict =
            dict
                |> Dict.update k
                    (\dock ->
                        case dock of
                            Just isDock ->
                                Just { isDock | dockLabel = v }

                            Nothing ->
                                Nothing
                    )
    in
    case storedSettings of
        Ok stored ->
            { options | docks = useStoredSettings stored }

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



-- Dock stuff


type alias DockSettings =
    { dockPopupOpen : Bool
    , dockLabel : String
    , dockLabelColour : Element.Color
    }


type ToolDock
    = DockUpperLeft
    | DockLowerLeft
    | DockUpperRight
    | DockLowerRight
    | DockBottom
    | DockNone


defaultDockColour =
    FlatColors.FlatUIPalette.wetAsphalt


dockList =
    [ ( "1", DockSettings False "Information" defaultDockColour )
    , ( "2", DockSettings False "Some tools" defaultDockColour )
    , ( "3", DockSettings False "Space for tools" defaultDockColour )
    , ( "4", DockSettings False "Bend tools" defaultDockColour )
    , ( "5", DockSettings False "Basics" defaultDockColour )
    ]


showDockHeader : (ToolMsg -> msg) -> ToolDock -> Dict String DockSettings -> Element msg
showDockHeader msgWrapper dockId docks =
    let
        dockNumber =
            case dockId of
                DockUpperLeft ->
                    "1"

                DockLowerLeft ->
                    "2"

                DockUpperRight ->
                    "5"

                DockLowerRight ->
                    "4"

                DockBottom ->
                    "3"

                DockNone ->
                    "0"

        dock =
            Dict.get dockNumber docks
    in
    case dock of
        Nothing ->
            none

        Just dockSettings ->
            row
                [ width fill
                , spacing 8
                , height <| px 24
                , Background.color dockSettings.dockLabelColour
                , Font.color <| contrastingColour dockSettings.dockLabelColour
                ]
                [ Input.button [ tooltip below (myTooltip "Click to edit label") ]
                    { onPress = Just <| msgWrapper <| DockPopupToggle dockNumber
                    , label = useIcon FeatherIcons.edit
                    }
                , case dockSettings.dockPopupOpen of
                    True ->
                        Input.text
                            [ Font.color defaultDockColour
                            , onEnter (DockPopupToggle dockNumber |> msgWrapper)
                            ]
                            { onChange = DockNameChange dockNumber >> msgWrapper
                            , text = dockSettings.dockLabel
                            , placeholder = Nothing
                            , label = Input.labelHidden "name"
                            }

                    False ->
                        text dockSettings.dockLabel
                ]


flythroughTick : Options -> Time.Posix -> TrackLoaded msg -> ( Options, List (ToolAction msg) )
flythroughTick options posix track =
    let
        ( updatedFlythrough, actions ) =
            Tools.Flythrough.advanceFlythrough posix options.flythroughSettings track
    in
    ( { options | flythroughSettings = updatedFlythrough }
    , actions
    )
