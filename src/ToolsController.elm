module ToolsController exposing (..)

import Actions exposing (ToolAction(..))
import ColourPalette exposing (stravaOrange)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (roundEach)
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import FeatherIcons
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import FlatColors.SwedishPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D exposing (field)
import Json.Encode as E exposing (string)
import List.Extra
import Time
import ToolTip exposing (localisedTooltip, myTooltip, tooltip)
import Tools.BendSmoother
import Tools.BendSmootherOptions
import Tools.BezierOptions
import Tools.BezierSplines
import Tools.CentroidAverage
import Tools.CentroidAverageOptions
import Tools.CurveFormer
import Tools.CurveFormerOptions
import Tools.DeletePoints as DeletePoints
import Tools.DirectionChanges as DirectionChanges
import Tools.DisplaySettings
import Tools.DisplaySettingsOptions
import Tools.Essentials
import Tools.Flythrough
import Tools.GradientProblems
import Tools.Graph
import Tools.GraphOptions
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Interpolate
import Tools.InterpolateOptions
import Tools.Intersections
import Tools.LandUse
import Tools.MoveAndStretch
import Tools.MoveAndStretchOptions
import Tools.MoveScaleRotate
import Tools.MoveScaleRotateOptions
import Tools.NamedSegment
import Tools.NamedSegmentOptions
import Tools.Nudge
import Tools.NudgeOptions
import Tools.OutAndBack
import Tools.OutAndBackOptions
import Tools.ProfileSmooth
import Tools.ProfileSmoothOptions
import Tools.Simplify
import Tools.SmartSmoother
import Tools.SmartSmootherOptions
import Tools.SplitAndJoin
import Tools.SplitAndJoinOptions
import Tools.StartFinish
import Tools.StartFinishTypes exposing (Loopiness(..))
import Tools.Straightener
import Tools.StravaOptions
import Tools.StravaTools
import Tools.Timestamp
import Tools.TimestampOptions
import Tools.TrackInfoBox as TrackInfoBox
import TrackLoaded exposing (TrackLoaded)
import ViewPureStyles exposing (..)


type ToolState
    = Expanded
    | Contracted
    | Disabled
    | AlwaysOpen
    | SettingsOpen
    | SettingsClosed -- hack.


type ToolType
    = ToolTrackInfo
    | ToolAbruptDirectionChanges
    | ToolDeletePoints
    | ToolEssentials
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
    | ToolProfileSmooth
    | ToolMoveScaleRotate
    | ToolFlythrough
    | ToolStrava
    | ToolMoveAndStretch
    | ToolStartFinish
    | ToolSplitAndJoin
    | ToolIntersections
    | ToolStraighten
    | ToolGraph
    | ToolSettings
    | ToolLandUse
    | ToolSmartSmoother
    | ToolNamedSegments
    | ToolTimestamps


type ToolCategory
    = TcInformation
    | TcBends
    | TcGradients
    | TcWholeTrack
    | TcRoute
    | TcMisc


type alias Options msg =
    -- Tool specific options
    { tools : List ToolEntry
    , docks : Dict String DockSettings
    , directionChangeOptions : DirectionChanges.Options
    , deleteOptions : DeletePoints.Options
    , essentialOptions : Tools.Essentials.Options
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
    , profileSmoothSettings : Tools.ProfileSmoothOptions.Options
    , moveScaleRotateSettings : Tools.MoveScaleRotateOptions.Options
    , flythroughSettings : Tools.Flythrough.Options
    , stravaSettings : Tools.StravaOptions.Options
    , moveAndStretchSettings : Tools.MoveAndStretchOptions.Options
    , startFinishOptions : Tools.StartFinishTypes.Options
    , splitAndJoinOptions : Tools.SplitAndJoinOptions.Options
    , intersectionOptions : Tools.Intersections.Options
    , straightenOptions : Tools.Straightener.Options
    , graphOptions : Tools.GraphOptions.Options msg
    , landUseOptions : Tools.LandUse.Options
    , smartSmootherOptions : Tools.SmartSmootherOptions.Options
    , namedSegmentOptions : Tools.NamedSegmentOptions.Options
    , timestampOptions : Tools.TimestampOptions.Options
    }


defaultOptions : Options msg
defaultOptions =
    { tools = defaultTools
    , docks = Dict.fromList dockList
    , directionChangeOptions = DirectionChanges.defaultOptions
    , deleteOptions = DeletePoints.defaultOptions
    , essentialOptions = Tools.Essentials.defaultOptions
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
    , profileSmoothSettings = Tools.ProfileSmooth.defaultOptions
    , moveScaleRotateSettings = Tools.MoveScaleRotate.defaultOptions
    , flythroughSettings = Tools.Flythrough.defaultOptions
    , stravaSettings = Tools.StravaTools.defaultOptions
    , moveAndStretchSettings = Tools.MoveAndStretch.defaultOptions
    , startFinishOptions = Tools.StartFinish.defaultOptions
    , splitAndJoinOptions = Tools.SplitAndJoin.defaultOptions
    , intersectionOptions = Tools.Intersections.defaultOptions
    , straightenOptions = Tools.Straightener.defaultOptions
    , graphOptions = Tools.Graph.defaultOptions
    , landUseOptions = Tools.LandUse.defaultOptions
    , smartSmootherOptions = Tools.SmartSmoother.defaultOptions
    , namedSegmentOptions = Tools.NamedSegment.defaultOptions
    , timestampOptions = Tools.Timestamp.defaultOptions
    }


type ToolMsg
    = ToolPopupToggle ToolType
    | ToolDockSelect ToolType ToolDock
    | ToolColourSelect ToolType Element.Color
    | ToolStateToggle ToolType ToolState
    | DockPopupToggle String
    | DockNameChange String String
    | DisplayInfo String String
    | DirectionChanges DirectionChanges.Msg
    | DeletePoints DeletePoints.Msg
    | ToolEssentialsMsg Tools.Essentials.Msg
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
    | ToolProfileSmoothMsg Tools.ProfileSmooth.Msg
    | ToolMoveScaleRotateMsg Tools.MoveScaleRotate.Msg
    | ToolFlythroughMsg Tools.Flythrough.Msg
    | ToolStravaMsg Tools.StravaTools.Msg
    | ToolMoveAndStretchMsg Tools.MoveAndStretch.Msg
    | ToolStartFinishMsg Tools.StartFinish.Msg
    | ToolSplitJoinMsg Tools.SplitAndJoin.Msg
    | ToolIntersectionMsg Tools.Intersections.Msg
    | ToolStraightenMsg Tools.Straightener.Msg
    | ToolGraphMsg Tools.Graph.Msg
    | ToolLandUseMsg Tools.LandUse.Msg
    | ToolSmartSmootherMsg Tools.SmartSmoother.Msg
    | ToolNamedSegmentMsg Tools.NamedSegment.Msg
    | ToolTimestampMsg Tools.Timestamp.Msg


type alias ToolEntry =
    { toolType : ToolType
    , toolId : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , tabColour : Element.Color
    , textColour : Element.Color
    , isPopupOpen : Bool
    , categories : List ToolCategory
    }


defaultTools : List ToolEntry
defaultTools =
    [ toolSettings
    , essentialsTool
    , trackInfoBox
    , displaySettingsTool
    , directionChangeTool
    , gradientChangeTool
    , deleteTool
    , bezierSplinesTool
    , centroidAverageTool
    , curveFormerTool
    , bendSmootherTool
    , smartSmootherTool
    , nudgeTool
    , outAndBackTool
    , simplifyTool
    , interpolateTool
    , profileSmoothTool
    , namedSegmentTool
    , moveScaleRotateTool
    , flythroughTool
    , stravaTool
    , moveAndStretchTool
    , startFinishTool
    , splitAndJoinTool
    , intersectionsTool
    , straightenTool
    , graphTool
    , landUseTool
    , timestampTool
    ]


toolSettings : ToolEntry
toolSettings =
    { toolType = ToolSettings
    , toolId = "tools"
    , video = Just "https://youtu.be/nQJtjDy_Qi4"
    , state = SettingsClosed
    , dock = DockUpperRight
    , tabColour = rgtPurple
    , textColour = contrastingColour FlatColors.FlatUIPalette.midnightBlue
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , toolId = TrackInfoBox.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.turquoise
    , textColour = contrastingColour FlatColors.FlatUIPalette.turquoise
    , isPopupOpen = False
    , categories = [ TcInformation ]
    }


displaySettingsTool : ToolEntry
displaySettingsTool =
    { toolType = ToolDisplaySettings
    , toolId = Tools.DisplaySettings.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.emerald
    , textColour = contrastingColour FlatColors.FlatUIPalette.emerald
    , isPopupOpen = False
    , categories = [ TcInformation ]
    }


directionChangeTool : ToolEntry
directionChangeTool =
    { toolType = ToolAbruptDirectionChanges
    , toolId = DirectionChanges.toolId
    , video = Just "https://youtu.be/IzjoHTQN0Lk"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.amethyst
    , textColour = contrastingColour FlatColors.FlatUIPalette.amethyst
    , isPopupOpen = False
    , categories = [ TcInformation ]
    }


gradientChangeTool : ToolEntry
gradientChangeTool =
    { toolType = ToolGradientProblems
    , toolId = Tools.GradientProblems.toolId
    , video = Just "https://youtu.be/IMn-MkxYFtc"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.wetAsphalt
    , textColour = contrastingColour FlatColors.FlatUIPalette.wetAsphalt
    , isPopupOpen = False
    , categories = [ TcGradients ]
    }


essentialsTool : ToolEntry
essentialsTool =
    { toolType = ToolEssentials
    , toolId = Tools.Essentials.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = AlwaysOpen
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.orange
    , textColour = contrastingColour FlatColors.FlatUIPalette.orange
    , isPopupOpen = False
    , categories = [] -- Always visible anyway
    }


deleteTool : ToolEntry
deleteTool =
    { toolType = ToolDeletePoints
    , toolId = DeletePoints.toolId
    , video = Just "https://youtu.be/3qobNm46TQw"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.greenSea
    , textColour = contrastingColour FlatColors.FlatUIPalette.greenSea
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


bezierSplinesTool : ToolEntry
bezierSplinesTool =
    { toolType = ToolBezierSplines
    , toolId = Tools.BezierSplines.toolId
    , video = Just "https://youtu.be/UuDfZYagvIU"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.nephritis
    , textColour = contrastingColour FlatColors.FlatUIPalette.nephritis
    , isPopupOpen = False
    , categories = [ TcWholeTrack, TcBends, TcGradients ]
    }


centroidAverageTool : ToolEntry
centroidAverageTool =
    { toolType = ToolCentroidAverage
    , toolId = Tools.CentroidAverage.toolId
    , video = Just "https://youtu.be/1C8clUhpQ20"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.belizeHole
    , textColour = contrastingColour FlatColors.FlatUIPalette.belizeHole
    , isPopupOpen = False
    , categories = [ TcGradients, TcBends, TcWholeTrack ]
    }


curveFormerTool : ToolEntry
curveFormerTool =
    { toolType = ToolCurveFormer
    , toolId = Tools.CurveFormer.toolId
    , video = Just "https://youtu.be/J81QZ6P6nV4"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.wisteria
    , textColour = contrastingColour FlatColors.FlatUIPalette.wisteria
    , isPopupOpen = False
    , categories = [ TcBends ]
    }


bendSmootherTool : ToolEntry
bendSmootherTool =
    { toolType = ToolBendSmoother
    , toolId = Tools.BendSmoother.toolId
    , video = Just "https://youtu.be/Qahop5xkuP0"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.midnightBlue
    , textColour = contrastingColour FlatColors.FlatUIPalette.midnightBlue
    , isPopupOpen = False
    , categories = [ TcBends ]
    }


smartSmootherTool : ToolEntry
smartSmootherTool =
    { toolType = ToolSmartSmoother
    , toolId = Tools.SmartSmoother.toolId
    , video = Just "https://youtu.be/6cSTQgvcRuw"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.sunFlower
    , textColour = contrastingColour FlatColors.FlatUIPalette.sunFlower
    , isPopupOpen = False
    , categories = [ TcBends, TcWholeTrack, TcGradients ]
    }


nudgeTool : ToolEntry
nudgeTool =
    { toolType = ToolNudge
    , toolId = Tools.Nudge.toolId
    , video = Just "https://youtu.be/lZslQzyplPM"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.sunFlower
    , textColour = contrastingColour FlatColors.FlatUIPalette.sunFlower
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


outAndBackTool : ToolEntry
outAndBackTool =
    { toolType = ToolOutAndBack
    , toolId = Tools.OutAndBack.toolId
    , video = Just "https://youtu.be/7gh5-r5uuOs"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.carrot
    , textColour = contrastingColour FlatColors.FlatUIPalette.carrot
    , isPopupOpen = False
    , categories = [ TcRoute ]
    }


simplifyTool : ToolEntry
simplifyTool =
    { toolType = ToolSimplify
    , toolId = Tools.Simplify.toolId
    , video = Just "https://youtu.be/dmK9PIlH04c"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.alizarin
    , textColour = contrastingColour FlatColors.FlatUIPalette.alizarin
    , isPopupOpen = False
    , categories = [ TcWholeTrack ]
    }


interpolateTool : ToolEntry
interpolateTool =
    { toolType = ToolInterpolate
    , toolId = Tools.Interpolate.toolId
    , video = Just "https://youtu.be/i5rALJ_42n0"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.clouds
    , textColour = contrastingColour FlatColors.FlatUIPalette.clouds
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


profileSmoothTool : ToolEntry
profileSmoothTool =
    { toolType = ToolProfileSmooth
    , toolId = Tools.ProfileSmooth.toolId
    , video = Just "https://youtu.be/XJGYt8LfTvQ"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    , categories = [ TcGradients ]
    }


namedSegmentTool : ToolEntry
namedSegmentTool =
    { toolType = ToolNamedSegments
    , toolId = Tools.NamedSegment.toolId
    , video = Nothing
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = rgtPurple
    , textColour = contrastingColour rgtPurple
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


moveScaleRotateTool : ToolEntry
moveScaleRotateTool =
    { toolType = ToolMoveScaleRotate
    , toolId = Tools.MoveScaleRotate.toolId
    , video = Just "https://youtu.be/tWLm1vhASCQ"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.pomegranate
    , textColour = contrastingColour FlatColors.FlatUIPalette.pomegranate
    , isPopupOpen = False
    , categories = [ TcMisc, TcWholeTrack ]
    }


flythroughTool : ToolEntry
flythroughTool =
    { toolType = ToolFlythrough
    , toolId = Tools.Flythrough.toolId
    , video = Just "https://youtu.be/w9M2zund_6s"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.pumpkin
    , textColour = contrastingColour FlatColors.FlatUIPalette.pumpkin
    , isPopupOpen = False
    , categories = [ TcInformation ]
    }


stravaTool : ToolEntry
stravaTool =
    { toolType = ToolStrava
    , toolId = Tools.StravaTools.toolId
    , video = Just "https://youtu.be/plG5rP0bbug"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = stravaOrange
    , textColour = contrastingColour stravaOrange
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


moveAndStretchTool : ToolEntry
moveAndStretchTool =
    { toolType = ToolMoveAndStretch
    , toolId = Tools.MoveAndStretch.toolId
    , video = Just "https://youtu.be/gnDlQMxf8wk"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.silver
    , textColour = contrastingColour FlatColors.FlatUIPalette.silver
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


startFinishTool : ToolEntry
startFinishTool =
    { toolType = ToolStartFinish
    , toolId = Tools.StartFinish.toolId
    , video = Just "https://youtu.be/NPcFRKKfx0w"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.asbestos
    , textColour = contrastingColour FlatColors.FlatUIPalette.asbestos
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


splitAndJoinTool : ToolEntry
splitAndJoinTool =
    { toolType = ToolSplitAndJoin
    , toolId = Tools.SplitAndJoin.toolId
    , video = Just "https://youtu.be/2dHqHWjyT7w"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.turquoise
    , textColour = contrastingColour FlatColors.FlatUIPalette.turquoise
    , isPopupOpen = False
    , categories = [ TcMisc, TcWholeTrack ]
    }


intersectionsTool : ToolEntry
intersectionsTool =
    { toolType = ToolIntersections
    , toolId = Tools.Intersections.toolId
    , video = Just "https://youtu.be/iWI1ASujFR4"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.emerald
    , textColour = contrastingColour FlatColors.FlatUIPalette.emerald
    , isPopupOpen = False
    , categories = [ TcInformation ]
    }


straightenTool : ToolEntry
straightenTool =
    { toolType = ToolStraighten
    , toolId = Tools.Straightener.toolId
    , video = Just "https://youtu.be/B_LX9BmuoxE"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.peterRiver
    , textColour = contrastingColour FlatColors.FlatUIPalette.peterRiver
    , isPopupOpen = False
    , categories = [ TcMisc ]
    }


graphTool : ToolEntry
graphTool =
    { toolType = ToolGraph
    , toolId = Tools.Graph.toolId
    , video = Just "https://youtu.be/90GZbpgZjnw"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.amethyst
    , textColour = contrastingColour FlatColors.FlatUIPalette.amethyst
    , isPopupOpen = False
    , categories = [ TcRoute ]
    }


landUseTool : ToolEntry
landUseTool =
    { toolType = ToolLandUse
    , toolId = Tools.LandUse.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.wetAsphalt
    , textColour = contrastingColour FlatColors.FlatUIPalette.wetAsphalt
    , isPopupOpen = False
    , categories = [ TcInformation ]
    }


timestampTool : ToolEntry
timestampTool =
    { toolType = ToolTimestamps
    , toolId = Tools.Timestamp.toolId
    , video = Nothing
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.FlatUIPalette.emerald
    , textColour = contrastingColour FlatColors.FlatUIPalette.emerald
    , isPopupOpen = False
    , categories = [ TcInformation ]
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

        AlwaysOpen ->
            AlwaysOpen

        SettingsOpen ->
            SettingsClosed

        SettingsClosed ->
            SettingsOpen


lockToolOpen : Bool -> String -> Options msg -> Options msg
lockToolOpen keepOpen id options =
    let
        tools =
            options.tools

        newTools =
            tools
                |> List.Extra.updateIf (\tool -> tool.toolId == id)
                    (\tool ->
                        { tool
                            | state =
                                if keepOpen then
                                    AlwaysOpen

                                else
                                    Expanded
                        }
                    )
    in
    { options | tools = newTools }


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
    -> Options msg
    -> ( Options msg, List (ToolAction msg) )
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

        DisplayInfo id tag ->
            ( options, [ Actions.DisplayInfo id tag ] )

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
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            DirectionChanges.update
                                msg
                                options.directionChangeOptions
                                (getColour ToolAbruptDirectionChanges options.tools)
                                track
                    in
                    ( { options | directionChangeOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

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

        ToolTimestampMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Timestamp.update
                        msg
                        options.timestampOptions
                        (getColour ToolGradientProblems options.tools)
                        isTrack
            in
            ( { options | timestampOptions = newOptions }
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

        ToolEssentialsMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Essentials.update
                        msg
                        options.essentialOptions
                        (getColour ToolEssentials options.tools)
                        isTrack
            in
            ( { options | essentialOptions = newOptions }
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
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.BendSmoother.update
                                msg
                                options.bendSmootherOptions
                                (getColour ToolBendSmoother options.tools)
                                track
                    in
                    ( { options | bendSmootherOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolNudgeMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.Nudge.update
                                msg
                                options.nudgeOptions
                                (getColour ToolNudge options.tools)
                                track
                    in
                    ( { options | nudgeOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

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
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.Simplify.update
                                msg
                                options.simplifySettings
                                (getColour ToolSimplify options.tools)
                                track
                    in
                    ( { options | simplifySettings = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

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

        ToolProfileSmoothMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.ProfileSmooth.update
                                msg
                                options.profileSmoothSettings
                                (getColour ToolProfileSmooth options.tools)
                                track
                    in
                    ( { options | profileSmoothSettings = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

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
                    ( newOptions, [ StoreLocally "docks" <| encodeDockState newOptions.docks ] )

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

        ToolMoveAndStretchMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.MoveAndStretch.update
                                msg
                                options.moveAndStretchSettings
                                (ToolMoveAndStretchMsg >> msgWrapper)
                                (getColour ToolMoveAndStretch options.tools)
                                track
                    in
                    ( { options | moveAndStretchSettings = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolStartFinishMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.StartFinish.update
                                msg
                                options.startFinishOptions
                                track
                    in
                    ( { options | startFinishOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolSplitJoinMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.SplitAndJoin.update
                                msg
                                options.splitAndJoinOptions
                                track
                                (msgWrapper << ToolSplitJoinMsg)
                    in
                    ( { options | splitAndJoinOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolIntersectionMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.Intersections.update
                                msg
                                options.intersectionOptions
                                (msgWrapper << ToolIntersectionMsg)
                    in
                    ( { options | intersectionOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolStraightenMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Straightener.update
                        msg
                        options.straightenOptions
            in
            ( { options | straightenOptions = newOptions }
            , actions
            )

        ToolGraphMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.Graph.update
                                msg
                                options.graphOptions
                                track
                                (msgWrapper << ToolGraphMsg)
                    in
                    ( { options | graphOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolLandUseMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.LandUse.update
                        msg
                        (msgWrapper << ToolLandUseMsg)
                        options.landUseOptions
            in
            ( { options | landUseOptions = newOptions }
            , actions
            )

        ToolSmartSmootherMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.SmartSmoother.update
                                msg
                                options.smartSmootherOptions
                                (getColour ToolSmartSmoother options.tools)
                                track
                    in
                    ( { options | smartSmootherOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolNamedSegmentMsg msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.NamedSegment.update
                                msg
                                options.namedSegmentOptions
                                track
                                (msgWrapper << ToolNamedSegmentMsg)
                    in
                    ( { options | namedSegmentOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )


refreshOpenTools :
    Maybe (TrackLoaded msg)
    -> Options msg
    -> ( Options msg, List (ToolAction msg) )
refreshOpenTools isTrack options =
    -- Track, or something has changed; tool data is stale.
    -- Same impact as tools being opened, so we'll re-use that.
    let
        refreshOpenTool entry ( inputOptions, collectingActions ) =
            if entry.state == Expanded || entry.state == AlwaysOpen then
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
    -> Options msg
    -> ( Options msg, List (ToolAction msg) )
toolStateHasChanged toolType newState isTrack options =
    case toolType of
        ToolTrackInfo ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolAbruptDirectionChanges ->
            -- Would like an OO style dispatch table here but what with each tool
            -- having its own options, that's more tricky than it's worth.
            let
                ( newToolOptions, actions ) =
                    DirectionChanges.toolStateChange
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

        ToolEssentials ->
            let
                ( newToolOptions, actions ) =
                    Tools.Essentials.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.essentialOptions
                        isTrack

                newOptions =
                    { options | essentialOptions = newToolOptions }
            in
            ( newOptions, [ StoreLocally "tools" <| encodeToolState options ] )

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

        ToolTimestamps ->
            let
                ( newToolOptions, actions ) =
                    Tools.Timestamp.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.timestampOptions
                        isTrack

                newOptions =
                    { options | timestampOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolProfileSmooth ->
            let
                ( newToolOptions, actions ) =
                    Tools.ProfileSmooth.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.profileSmoothSettings
                        isTrack

                newOptions =
                    { options | profileSmoothSettings = newToolOptions }
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

        ToolMoveAndStretch ->
            let
                ( newToolOptions, actions ) =
                    Tools.MoveAndStretch.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.moveAndStretchSettings
                        isTrack

                newOptions =
                    { options | moveAndStretchSettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolStartFinish ->
            let
                ( newToolOptions, actions ) =
                    Tools.StartFinish.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.startFinishOptions
                        isTrack

                newOptions =
                    { options | startFinishOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolSplitAndJoin ->
            let
                ( newToolOptions, actions ) =
                    Tools.SplitAndJoin.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.splitAndJoinOptions
                        isTrack

                newOptions =
                    { options | splitAndJoinOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolIntersections ->
            let
                ( newToolOptions, actions ) =
                    Tools.Intersections.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.intersectionOptions
                        isTrack

                newOptions =
                    { options | intersectionOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolStraighten ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolGraph ->
            let
                ( newGraphOptions, actions ) =
                    Tools.Graph.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.graphOptions
                        isTrack

                newOptions =
                    { options | graphOptions = newGraphOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolSettings ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolLandUse ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolSmartSmoother ->
            let
                ( newToolOptions, actions ) =
                    Tools.SmartSmoother.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.smartSmootherOptions
                        isTrack

                newOptions =
                    { options | smartSmootherOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolNamedSegments ->
            let
                ( newToolOptions, actions ) =
                    Tools.NamedSegment.toolStateChange
                        (newState == Expanded)
                        (getColour toolType options.tools)
                        options.namedSegmentOptions
                        isTrack

                newOptions =
                    { options | namedSegmentOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )



--View stuff


toolsForDock :
    I18NOptions.Location
    -> ToolDock
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> Element msg
toolsForDock location dock msgWrapper isTrack options =
    column [ width fill, height fill ]
        [ column [ width fill, height fill, spacing 5, scrollbarY ]
            [ column [ width fill, spacing 5 ]
                (options.tools
                    |> List.filter
                        (\t -> t.dock == dock && (t.state == AlwaysOpen || t.state == SettingsOpen || t.state == SettingsClosed))
                    |> List.map (viewTool location msgWrapper isTrack options)
                )
            , wrappedRow
                -- Open tools
                []
              <|
                (options.tools
                    |> List.filter (\t -> t.dock == dock && t.state == Expanded)
                    |> List.map (viewTool location msgWrapper isTrack options)
                )
            , wrappedRow
                -- Closed tools
                []
              <|
                (options.tools
                    |> List.filter (\t -> t.dock == dock && t.state == Contracted)
                    |> List.map (viewTool location msgWrapper isTrack options)
                )
            ]
        ]


viewToolSettings : I18NOptions.Location -> Options msg -> (ToolMsg -> msg) -> Element msg
viewToolSettings location options wrapper =
    let
        optionHelper =
            compactRadioButton << I18N.localisedString location "tools"

        fullOptionList tool =
            if (tool.toolType == ToolSettings) || (tool.toolType == ToolEssentials) then
                [ Input.optionWith DockUpperLeft <| optionHelper "onleft"
                , Input.optionWith DockUpperRight <| optionHelper "onright"
                , Input.optionWith tool.dock <| optionHelper "blank"
                ]

            else
                [ Input.optionWith DockUpperLeft <| optionHelper "onleft"
                , Input.optionWith DockUpperRight <| optionHelper "onright"
                , Input.optionWith DockNone <| optionHelper "hidden"
                ]

        locationChoices : ToolEntry -> Element msg
        locationChoices tool =
            Input.radioRow
                [ spacing 5
                , paddingEach { top = 4, left = 4, bottom = 0, right = 0 }
                ]
                { onChange = wrapper << ToolDockSelect tool.toolType
                , selected = Just tool.dock
                , label =
                    Input.labelRight [ paddingXY 10 0 ] <|
                        row [ spacing 4 ]
                            [ infoButton (wrapper <| DisplayInfo tool.toolId "info")
                            , I18N.text location tool.toolId "label"
                            ]
                , options = fullOptionList tool
                }
    in
    column
        [ width fill
        , height <| px 300
        , scrollbarY
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , padding 10
        , spacing 3
        ]
    <|
        List.map locationChoices options.tools


viewTool :
    I18NOptions.Location
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> ToolEntry
    -> Element msg
viewTool location msgWrapper isTrack options toolEntry =
    -- Possible performance gain by being lazy here. Who knows?
    Element.Lazy.lazy5
        viewToolLazy
        location
        msgWrapper
        isTrack
        options
        toolEntry


viewToolLazy :
    I18NOptions.Location
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> ToolEntry
    -> Element msg
viewToolLazy location msgWrapper isTrack options toolEntry =
    el [ padding 2, width fill, alignTop ] <|
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
                    [ showDockOptions location msgWrapper toolEntry
                    , showColourOptions msgWrapper toolEntry
                    ]
            ]
            [ row
                [ width fill
                , spacing 8
                , height <| px 24
                , Background.color toolEntry.tabColour
                , Font.color toolEntry.textColour
                ]
                [ case toolEntry.video of
                    Just video ->
                        newTabLink
                            [ alignLeft
                            , tooltip below (myTooltip "Watch the video")
                            , htmlAttribute <|
                                Mouse.onWithOptions
                                    "click"
                                    stopProp
                                    (always << msgWrapper <| ToolNoOp)
                            ]
                            { url = video
                            , label = useIconWithSize 18 FeatherIcons.youtube
                            }

                    Nothing ->
                        none
                , Input.button
                    [ centerX ]
                    { onPress =
                        Just <|
                            msgWrapper <|
                                ToolStateToggle toolEntry.toolType <|
                                    nextToolState toolEntry.state
                    , label =
                        row [ alignLeft, spacing 10 ]
                            [ case toolEntry.state of
                                Expanded ->
                                    useIconWithSize 16 <| FeatherIcons.chevronsUp

                                Contracted ->
                                    useIconWithSize 16 <| FeatherIcons.chevronsDown

                                Disabled ->
                                    useIconWithSize 16 <| FeatherIcons.slash

                                AlwaysOpen ->
                                    none

                                SettingsOpen ->
                                    useIconWithSize 16 <| FeatherIcons.chevronsUp

                                SettingsClosed ->
                                    useIconWithSize 16 <| FeatherIcons.chevronsDown
                            , I18N.text location toolEntry.toolId "label"
                            ]
                    }
                , Input.button
                    [ alignRight
                    , htmlAttribute <|
                        Mouse.onWithOptions
                            "click"
                            stopProp
                            (always << msgWrapper <| ToolPopupToggle toolEntry.toolType)
                    ]
                    { onPress = Just <| msgWrapper <| ToolPopupToggle toolEntry.toolType
                    , label = useIconWithSize 14 FeatherIcons.settings
                    }
                ]
            , el [ Border.rounded 8, width fill, height fill ] <|
                if toolEntry.state == Expanded || toolEntry.state == AlwaysOpen || toolEntry.state == SettingsOpen then
                    viewToolByType location msgWrapper toolEntry isTrack options

                else
                    none
            ]


showDockOptions : I18NOptions.Location -> (ToolMsg -> msg) -> ToolEntry -> Element msg
showDockOptions location msgWrapper toolEntry =
    if toolEntry.isPopupOpen then
        row
            neatToolsBorder
            [ Input.button
                [ tooltip below (localisedTooltip location "tools" "left") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperLeft
                , label = useIcon FeatherIcons.arrowLeft
                }
            , Input.button
                [ tooltip below (localisedTooltip location "tools" "right") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperRight
                , label = useIcon FeatherIcons.arrowRight
                }
            ]

    else
        none


clearPopups : Options msg -> Options msg
clearPopups options =
    let
        clearPopup tool =
            { tool | isPopupOpen = False }
    in
    { options | tools = List.map clearPopup options.tools }


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
                , colourBlock rgtDark
                ]
            , row []
                [ colourBlock FlatColors.FlatUIPalette.greenSea
                , colourBlock FlatColors.FlatUIPalette.nephritis
                , colourBlock FlatColors.FlatUIPalette.belizeHole
                , colourBlock rgtPurple
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
    I18NOptions.Location
    -> (ToolMsg -> msg)
    -> ToolEntry
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> Element msg
viewToolByType location msgWrapper entry isTrack options =
    el
        [ centerX, padding 2, width fill, Border.rounded 4 ]
    <|
        case entry.toolType of
            ToolTimestamps ->
                Tools.Timestamp.view
                    location
                    options.imperial
                    (msgWrapper << ToolTimestampMsg)
                    options.timestampOptions
                    isTrack

            ToolTrackInfo ->
                TrackInfoBox.view
                    location
                    (msgWrapper << ToolInfoMsg)
                    options.imperial
                    isTrack
                    options.infoOptions

            ToolAbruptDirectionChanges ->
                DirectionChanges.view
                    location
                    options.imperial
                    (msgWrapper << DirectionChanges)
                    options.directionChangeOptions
                    isTrack

            ToolGradientProblems ->
                Tools.GradientProblems.view
                    location
                    options.imperial
                    (msgWrapper << ToolGradientChangeMsg)
                    options.gradientProblemOptions
                    isTrack

            ToolDeletePoints ->
                case isTrack of
                    Just track ->
                        DeletePoints.view
                            location
                            (msgWrapper << DeletePoints)
                            options.deleteOptions
                            track

                    Nothing ->
                        noTrackMessage location

            ToolEssentials ->
                Tools.Essentials.view
                    location
                    options.imperial
                    (msgWrapper << ToolEssentialsMsg)
                    options.essentialOptions
                    isTrack

            ToolBezierSplines ->
                case isTrack of
                    Just track ->
                        Tools.BezierSplines.view
                            location
                            (msgWrapper << ToolBezierMsg)
                            options.bezierSplineOptions
                            track

                    Nothing ->
                        noTrackMessage location

            ToolCentroidAverage ->
                case isTrack of
                    Just track ->
                        Tools.CentroidAverage.view
                            location
                            (msgWrapper << ToolCentroidMsg)
                            options.centroidAverageOptions
                            track

                    Nothing ->
                        noTrackMessage location

            ToolCurveFormer ->
                Tools.CurveFormer.view
                    location
                    options.imperial
                    (msgWrapper << ToolCurveFormerMsg)
                    options.curveFormerOptions
                    isTrack

            ToolBendSmoother ->
                Tools.BendSmoother.view
                    location
                    options.imperial
                    (msgWrapper << ToolBendSmootherMsg)
                    options.bendSmootherOptions
                    isTrack

            ToolNudge ->
                Tools.Nudge.view
                    location
                    options.imperial
                    options.nudgeOptions
                    (msgWrapper << ToolNudgeMsg)
                    isTrack

            ToolDisplaySettings ->
                Tools.DisplaySettings.view
                    location
                    (msgWrapper << ToolDisplaySettingMsg)
                    options.displaySettings

            ToolOutAndBack ->
                Tools.OutAndBack.view
                    location
                    options.imperial
                    (msgWrapper << ToolOutAndBackMsg)
                    options.outAndBackSettings
                    isTrack

            ToolSimplify ->
                Tools.Simplify.view
                    location
                    (msgWrapper << ToolSimplifyMsg)
                    options.simplifySettings
                    isTrack

            ToolInterpolate ->
                Tools.Interpolate.view
                    location
                    options.imperial
                    (msgWrapper << ToolInterpolateMsg)
                    options.interpolateSettings
                    isTrack

            ToolProfileSmooth ->
                case isTrack of
                    Just track ->
                        Tools.ProfileSmooth.view
                            location
                            options.profileSmoothSettings
                            (msgWrapper << ToolProfileSmoothMsg)
                            track

                    Nothing ->
                        noTrackMessage location

            ToolMoveScaleRotate ->
                Tools.MoveScaleRotate.view
                    location
                    options.imperial
                    options.moveScaleRotateSettings
                    (msgWrapper << ToolMoveScaleRotateMsg)
                    isTrack

            ToolFlythrough ->
                Tools.Flythrough.view
                    location
                    options.imperial
                    options.flythroughSettings
                    (msgWrapper << ToolFlythroughMsg)

            ToolStrava ->
                Tools.StravaTools.viewStravaTab
                    location
                    options.stravaSettings
                    (msgWrapper << ToolStravaMsg)
                    isTrack

            ToolMoveAndStretch ->
                case isTrack of
                    Just track ->
                        Tools.MoveAndStretch.view
                            location
                            options.imperial
                            options.moveAndStretchSettings
                            (msgWrapper << ToolMoveAndStretchMsg)
                            track

                    Nothing ->
                        noTrackMessage location

            ToolStartFinish ->
                case isTrack of
                    Just track ->
                        Tools.StartFinish.view
                            location
                            options.imperial
                            options.startFinishOptions
                            track
                            (msgWrapper << ToolStartFinishMsg)

                    Nothing ->
                        noTrackMessage location

            ToolSplitAndJoin ->
                case isTrack of
                    Just track ->
                        Tools.SplitAndJoin.view
                            location
                            options.imperial
                            options.splitAndJoinOptions
                            (msgWrapper << ToolSplitJoinMsg)
                            track

                    Nothing ->
                        noTrackMessage location

            ToolIntersections ->
                case isTrack of
                    Just track ->
                        Tools.Intersections.view
                            location
                            options.imperial
                            (msgWrapper << ToolIntersectionMsg)
                            options.intersectionOptions
                            track

                    Nothing ->
                        noTrackMessage location

            ToolStraighten ->
                case isTrack of
                    Just track ->
                        Tools.Straightener.view
                            location
                            (msgWrapper << ToolStraightenMsg)
                            options.straightenOptions
                            track

                    Nothing ->
                        noTrackMessage location

            ToolGraph ->
                case isTrack of
                    Just track ->
                        Tools.Graph.view
                            location
                            options.imperial
                            (msgWrapper << ToolGraphMsg)
                            options.graphOptions

                    Nothing ->
                        noTrackMessage location

            ToolSettings ->
                viewToolSettings location options msgWrapper

            ToolLandUse ->
                Tools.LandUse.view
                    location
                    (msgWrapper << ToolLandUseMsg)
                    options.landUseOptions
                    isTrack

            ToolSmartSmoother ->
                case isTrack of
                    Just track ->
                        Tools.SmartSmoother.view
                            location
                            options.imperial
                            (msgWrapper << ToolSmartSmootherMsg)
                            options.smartSmootherOptions
                            track

                    Nothing ->
                        noTrackMessage location

            ToolNamedSegments ->
                case isTrack of
                    Just track ->
                        Tools.NamedSegment.view
                            location
                            options.imperial
                            (msgWrapper << ToolNamedSegmentMsg)
                            options.namedSegmentOptions
                            track

                    Nothing ->
                        noTrackMessage location



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
        ToolTimestamps ->
            "ToolTimestamps"

        ToolTrackInfo ->
            "ToolTrackInfo"

        ToolAbruptDirectionChanges ->
            "ToolAbruptDirectionChanges"

        ToolDeletePoints ->
            "ToolDeletePoints"

        ToolEssentials ->
            "ToolEssentials"

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

        ToolProfileSmooth ->
            "ToolProfileSmooth"

        ToolMoveScaleRotate ->
            "ToolMoveScaleRotate"

        ToolFlythrough ->
            "ToolFlythrough"

        ToolStrava ->
            "ToolStrava"

        ToolMoveAndStretch ->
            "ToolMoveAndStretch"

        ToolStartFinish ->
            "ToolStartFinish"

        ToolSplitAndJoin ->
            "ToolSplitAndJoin"

        ToolIntersections ->
            "ToolIntersections"

        ToolStraighten ->
            "ToolStraighten"

        ToolGraph ->
            "ToolGraph"

        ToolSettings ->
            "ToolSettings"

        ToolLandUse ->
            "ToolLandUse"

        ToolSmartSmoother ->
            "ToolTreeSmoother"

        ToolNamedSegments ->
            "ToolNamedSegments"


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

        AlwaysOpen ->
            "always"

        SettingsOpen ->
            "open"

        SettingsClosed ->
            "closed"


decodeState : String -> ToolState
decodeState state =
    case state of
        "expanded" ->
            Expanded

        "contracted" ->
            Contracted

        "disabled" ->
            Disabled

        "always" ->
            AlwaysOpen

        "closed" ->
            SettingsClosed

        "open" ->
            SettingsOpen

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


encodeToolState : Options msg -> E.Value
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


restoreStoredValues : Options msg -> D.Value -> Options msg
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


restoreDockSettings : Options msg -> D.Value -> Options msg
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


restoreMeasure : Options msg -> D.Value -> Options msg
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


imperialToggleMenuEntry location msgWrapper options =
    Input.button [ alignRight ]
        { onPress = Just <| msgWrapper ToggleImperial
        , label =
            if options.imperial then
                I18N.text location "main" "metric"

            else
                I18N.text location "main" "imperial"
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
    [ ( "1", DockSettings False "Upper left" defaultDockColour )
    , ( "2", DockSettings False "Lower left" defaultDockColour )
    , ( "3", DockSettings False "Central" defaultDockColour )
    , ( "4", DockSettings False "Lower right" defaultDockColour )
    , ( "5", DockSettings False "Upper right" defaultDockColour )
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


flythroughTick : Options msg -> Time.Posix -> TrackLoaded msg -> ( Options msg, List (ToolAction msg) )
flythroughTick options posix track =
    let
        ( updatedFlythrough, actions ) =
            Tools.Flythrough.advanceFlythrough posix options.flythroughSettings track
    in
    ( { options | flythroughSettings = updatedFlythrough }
    , actions
    )
