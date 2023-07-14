module ToolsController exposing
    ( ColourTriplet
    , DockSettings
    , Options
    , ToolDock(..)
    , ToolEntry
    , ToolMsg(..)
    , ToolState(..)
    , ToolType(..)
    , anyToolsInLeftDock
    , applyPaintTool
    , clearPopups
    , colourDecoder
    , decodeColour
    , defaultOptions
    , encodeColour
    , isToolOpen
    , makePaintPreview
    , refreshOpenTools
    , restoreDockSettings
    , restoreMeasure
    , restoreSettings
    , restoreStoredValues
    , setToolState
    , subscriptions
    , toolsForDock
    , update
    , viewToolForPainting
    )

import Actions exposing (ToolAction(..))
import ColourPalette exposing (stravaOrange)
import CommonToolStyles exposing (noTrackMessage)
import Dict exposing (Dict)
import DomainModel
import Drag3dCommonStructures exposing (PointLeafProximity)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.CanadianPalette
import FlatColors.ChinesePalette
import FlatColors.FlatUIPalette
import FlatColors.SwedishPalette
import FlatColors.TurkishPalette
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D exposing (field, maybe)
import Json.Encode as E
import Length
import List.Extra
import PreviewData exposing (PreviewData)
import Quantity
import SystemSettings exposing (SystemSettings)
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
import Tools.I18N as I18N
import Tools.Interpolate as Interpolate
import Tools.InterpolateOptions
import Tools.Intersections
import Tools.LandUse
import Tools.MapMatchingRouter
import Tools.MapMatchingRouterOptions
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
import Tools.StartFinishTypes
import Tools.Straightener
import Tools.StravaOptions
import Tools.StravaTools
import Tools.Timestamp
import Tools.TimestampOptions
import Tools.TrackInfoBox as TrackInfoBox
import Tools.Tracks
import Tools.TracksOptions as Tracks
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
    | ToolSettings
    | ToolLandUse
    | ToolSmartSmoother
    | ToolNamedSegments
    | ToolTimestamps
    | ToolRouting
    | ToolTracks


type alias Options msg =
    -- Tool specific options
    { tools : Dict String ToolEntry
    , docks : Dict String DockSettings
    , azSort : Bool
    , compact : Bool
    , paintTool : Maybe String
    , directionChangeOptions : DirectionChanges.Options
    , deleteOptions : DeletePoints.Options
    , essentialOptions : Tools.Essentials.Options
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
    , landUseOptions : Tools.LandUse.Options
    , smartSmootherOptions : Tools.SmartSmootherOptions.Options
    , namedSegmentOptions : Tools.NamedSegmentOptions.Options
    , timestampOptions : Tools.TimestampOptions.Options
    , routingOptions : Tools.MapMatchingRouterOptions.Options
    , tracksOptions : Tracks.Options msg
    }


defaultOptions : Options msg
defaultOptions =
    { tools = defaultTools
    , docks = Dict.fromList dockList
    , azSort = True
    , compact = True
    , paintTool = Nothing
    , directionChangeOptions = DirectionChanges.defaultOptions
    , deleteOptions = DeletePoints.defaultOptions
    , essentialOptions = Tools.Essentials.defaultOptions
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
    , interpolateSettings = Interpolate.defaultOptions
    , profileSmoothSettings = Tools.ProfileSmooth.defaultOptions
    , moveScaleRotateSettings = Tools.MoveScaleRotate.defaultOptions
    , flythroughSettings = Tools.Flythrough.defaultOptions
    , stravaSettings = Tools.StravaTools.defaultOptions
    , moveAndStretchSettings = Tools.MoveAndStretch.defaultOptions
    , startFinishOptions = Tools.StartFinish.defaultOptions
    , splitAndJoinOptions = Tools.SplitAndJoin.defaultOptions
    , intersectionOptions = Tools.Intersections.defaultOptions
    , straightenOptions = Tools.Straightener.defaultOptions
    , landUseOptions = Tools.LandUse.defaultOptions
    , smartSmootherOptions = Tools.SmartSmoother.defaultOptions
    , namedSegmentOptions = Tools.NamedSegment.defaultOptions
    , timestampOptions = Tools.Timestamp.defaultOptions
    , routingOptions = Tools.MapMatchingRouter.defaultOptions
    , tracksOptions = Tools.Tracks.defaultOptions
    }


type ToolMsg
    = ToolPopupToggle String
    | ToolDockSelect String ToolDock
    | ToolToggleSort Bool
    | ToolToggleCompact Bool
    | ToolToggleVisible String
    | ToolColourSelect String Element.Color
    | ToolStateToggle String ToolState
    | ToolActivate String ToolState
    | DisplayInfo String String
    | DirectionChanges DirectionChanges.Msg
    | DeletePoints DeletePoints.Msg
    | ToolEssentialsMsg Tools.Essentials.Msg
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
    | ToolInterpolateMsg Interpolate.Msg
    | ToolProfileSmoothMsg Tools.ProfileSmooth.Msg
    | ToolMoveScaleRotateMsg Tools.MoveScaleRotate.Msg
    | ToolFlythroughMsg Tools.Flythrough.Msg
    | ToolStravaMsg Tools.StravaTools.Msg
    | ToolMoveAndStretchMsg Tools.MoveAndStretch.Msg
    | ToolStartFinishMsg Tools.StartFinish.Msg
    | ToolSplitJoinMsg Tools.SplitAndJoin.Msg
    | ToolIntersectionMsg Tools.Intersections.Msg
    | ToolStraightenMsg Tools.Straightener.Msg
    | ToolLandUseMsg Tools.LandUse.Msg
    | ToolSmartSmootherMsg Tools.SmartSmoother.Msg
    | ToolNamedSegmentMsg Tools.NamedSegment.Msg
    | ToolTimestampMsg Tools.Timestamp.Msg
    | ToolRoutingMsg Tools.MapMatchingRouter.Msg
    | ToolTracksMsg Tools.Tracks.Msg
    | ToolSetPaintTool String


type alias ToolEntry =
    { toolType : ToolType
    , toolId : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , isVisible : Bool
    , tabColour : Element.Color
    , textColour : Element.Color
    , isPopupOpen : Bool
    }


defaultTools : Dict String ToolEntry
defaultTools =
    Dict.fromList orderedTools


orderedTools : List ( String, ToolEntry )
orderedTools =
    -- So we can impose a familiar ordering on the display.
    [ ( toolSettings.toolId, toolSettings )
    , ( essentialsTool.toolId, essentialsTool )
    , ( trackInfoBox.toolId, trackInfoBox )
    , ( displaySettingsTool.toolId, displaySettingsTool )
    , ( directionChangeTool.toolId, directionChangeTool )
    , ( gradientChangeTool.toolId, gradientChangeTool )
    , ( deleteTool.toolId, deleteTool )
    , ( bezierSplinesTool.toolId, bezierSplinesTool )
    , ( centroidAverageTool.toolId, centroidAverageTool )
    , ( curveFormerTool.toolId, curveFormerTool )
    , ( bendSmootherTool.toolId, bendSmootherTool )
    , ( smartSmootherTool.toolId, smartSmootherTool )
    , ( nudgeTool.toolId, nudgeTool )
    , ( outAndBackTool.toolId, outAndBackTool )
    , ( simplifyTool.toolId, simplifyTool )
    , ( interpolateTool.toolId, interpolateTool )
    , ( profileSmoothTool.toolId, profileSmoothTool )
    , ( namedSegmentTool.toolId, namedSegmentTool )
    , ( moveScaleRotateTool.toolId, moveScaleRotateTool )
    , ( flythroughTool.toolId, flythroughTool )
    , ( stravaTool.toolId, stravaTool )
    , ( moveAndStretchTool.toolId, moveAndStretchTool )
    , ( startFinishTool.toolId, startFinishTool )
    , ( splitAndJoinTool.toolId, splitAndJoinTool )
    , ( intersectionsTool.toolId, intersectionsTool )
    , ( straightenTool.toolId, straightenTool )
    , ( landUseTool.toolId, landUseTool )
    , ( timestampTool.toolId, timestampTool )
    , ( routingTool.toolId, routingTool )
    , ( tracksTool.toolId, tracksTool )
    ]


paintingTools =
    -- Keep controller here in charge of keeping track of what is painting.
    Dict.fromList <|
        [ ( deleteTool.toolId, deleteTool )
        , ( bezierSplinesTool.toolId, bezierSplinesTool )
        , ( centroidAverageTool.toolId, centroidAverageTool )
        , ( curveFormerTool.toolId, curveFormerTool )
        , ( bendSmootherTool.toolId, bendSmootherTool )
        , ( smartSmootherTool.toolId, smartSmootherTool )
        , ( nudgeTool.toolId, nudgeTool )
        , ( simplifyTool.toolId, simplifyTool )
        , ( interpolateTool.toolId, interpolateTool )
        , ( profileSmoothTool.toolId, profileSmoothTool )
        , ( straightenTool.toolId, straightenTool )
        ]


toolSettings : ToolEntry
toolSettings =
    { toolType = ToolSettings
    , toolId = "tools"
    , video = Just "https://youtu.be/ZFpk8btZC_g"
    , state = SettingsClosed
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.ChinesePalette.grisaille
    , textColour = contrastingColour FlatColors.ChinesePalette.grisaille
    , isPopupOpen = False
    }


defaultToolColour =
    FlatColors.FlatUIPalette.peterRiver


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , toolId = TrackInfoBox.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.turquoise
    , textColour = contrastingColour FlatColors.FlatUIPalette.turquoise
    , isPopupOpen = False
    }


displaySettingsTool : ToolEntry
displaySettingsTool =
    { toolType = ToolDisplaySettings
    , toolId = Tools.DisplaySettings.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.AussiePalette.greenlandGreen
    , textColour = contrastingColour FlatColors.AussiePalette.greenlandGreen
    , isPopupOpen = False
    }


directionChangeTool : ToolEntry
directionChangeTool =
    { toolType = ToolAbruptDirectionChanges
    , toolId = DirectionChanges.toolId
    , video = Just "https://youtu.be/IzjoHTQN0Lk"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


gradientChangeTool : ToolEntry
gradientChangeTool =
    { toolType = ToolGradientProblems
    , toolId = Tools.GradientProblems.toolId
    , video = Just "https://youtu.be/IMn-MkxYFtc"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


essentialsTool : ToolEntry
essentialsTool =
    { toolType = ToolEssentials
    , toolId = Tools.Essentials.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = AlwaysOpen
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.orange
    , textColour = contrastingColour FlatColors.FlatUIPalette.orange
    , isPopupOpen = False
    }


deleteTool : ToolEntry
deleteTool =
    { toolType = ToolDeletePoints
    , toolId = DeletePoints.toolId
    , video = Just "https://youtu.be/3qobNm46TQw"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.greenSea
    , textColour = contrastingColour FlatColors.FlatUIPalette.greenSea
    , isPopupOpen = False
    }


bezierSplinesTool : ToolEntry
bezierSplinesTool =
    { toolType = ToolBezierSplines
    , toolId = Tools.BezierSplines.toolId
    , video = Just "https://youtu.be/UuDfZYagvIU"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.nephritis
    , textColour = contrastingColour FlatColors.FlatUIPalette.nephritis
    , isPopupOpen = False
    }


centroidAverageTool : ToolEntry
centroidAverageTool =
    { toolType = ToolCentroidAverage
    , toolId = Tools.CentroidAverage.toolId
    , video = Just "https://youtu.be/1C8clUhpQ20"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.belizeHole
    , textColour = contrastingColour FlatColors.FlatUIPalette.belizeHole
    , isPopupOpen = False
    }


curveFormerTool : ToolEntry
curveFormerTool =
    { toolType = ToolCurveFormer
    , toolId = Tools.CurveFormer.toolId
    , video = Just "https://youtu.be/J81QZ6P6nV4"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


bendSmootherTool : ToolEntry
bendSmootherTool =
    { toolType = ToolBendSmoother
    , toolId = Tools.BendSmoother.toolId
    , video = Just "https://youtu.be/Qahop5xkuP0"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


smartSmootherTool : ToolEntry
smartSmootherTool =
    { toolType = ToolSmartSmoother
    , toolId = Tools.SmartSmoother.toolId
    , video = Just "https://youtu.be/6cSTQgvcRuw"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.sunFlower
    , textColour = contrastingColour FlatColors.FlatUIPalette.sunFlower
    , isPopupOpen = False
    }


nudgeTool : ToolEntry
nudgeTool =
    { toolType = ToolNudge
    , toolId = Tools.Nudge.toolId
    , video = Just "https://youtu.be/lZslQzyplPM"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.TurkishPalette.mandarinSorbet
    , textColour = contrastingColour FlatColors.TurkishPalette.mandarinSorbet
    , isPopupOpen = False
    }


outAndBackTool : ToolEntry
outAndBackTool =
    { toolType = ToolOutAndBack
    , toolId = Tools.OutAndBack.toolId
    , video = Just "https://youtu.be/7gh5-r5uuOs"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.carrot
    , textColour = contrastingColour FlatColors.FlatUIPalette.carrot
    , isPopupOpen = False
    }


simplifyTool : ToolEntry
simplifyTool =
    { toolType = ToolSimplify
    , toolId = Tools.Simplify.toolId
    , video = Just "https://youtu.be/dmK9PIlH04c"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


interpolateTool : ToolEntry
interpolateTool =
    { toolType = ToolInterpolate
    , toolId = Interpolate.toolId
    , video = Just "https://youtu.be/i5rALJ_42n0"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.CanadianPalette.jigglypuff
    , textColour = contrastingColour FlatColors.CanadianPalette.jigglypuff
    , isPopupOpen = False
    }


profileSmoothTool : ToolEntry
profileSmoothTool =
    { toolType = ToolProfileSmooth
    , toolId = Tools.ProfileSmooth.toolId
    , video = Just "https://youtu.be/XJGYt8LfTvQ"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.concrete
    , textColour = contrastingColour FlatColors.FlatUIPalette.concrete
    , isPopupOpen = False
    }


namedSegmentTool : ToolEntry
namedSegmentTool =
    { toolType = ToolNamedSegments
    , toolId = Tools.NamedSegment.toolId
    , video = Just "https://youtu.be/PaJeI8yahBI"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


moveScaleRotateTool : ToolEntry
moveScaleRotateTool =
    { toolType = ToolMoveScaleRotate
    , toolId = Tools.MoveScaleRotate.toolId
    , video = Just "https://youtu.be/tWLm1vhASCQ"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


flythroughTool : ToolEntry
flythroughTool =
    { toolType = ToolFlythrough
    , toolId = Tools.Flythrough.toolId
    , video = Just "https://youtu.be/w9M2zund_6s"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


stravaTool : ToolEntry
stravaTool =
    { toolType = ToolStrava
    , toolId = Tools.StravaTools.toolId
    , video = Just "https://youtu.be/plG5rP0bbug"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = stravaOrange
    , textColour = contrastingColour stravaOrange
    , isPopupOpen = False
    }


moveAndStretchTool : ToolEntry
moveAndStretchTool =
    { toolType = ToolMoveAndStretch
    , toolId = Tools.MoveAndStretch.toolId
    , video = Just "https://youtu.be/gnDlQMxf8wk"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.TurkishPalette.spiroDiscoBall
    , textColour = contrastingColour FlatColors.TurkishPalette.spiroDiscoBall
    , isPopupOpen = False
    }


startFinishTool : ToolEntry
startFinishTool =
    { toolType = ToolStartFinish
    , toolId = Tools.StartFinish.toolId
    , video = Just "https://youtu.be/NPcFRKKfx0w"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


splitAndJoinTool : ToolEntry
splitAndJoinTool =
    { toolType = ToolSplitAndJoin
    , toolId = Tools.SplitAndJoin.toolId
    , video = Just "https://youtu.be/2dHqHWjyT7w"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.turquoise
    , textColour = contrastingColour FlatColors.FlatUIPalette.turquoise
    , isPopupOpen = False
    }


intersectionsTool : ToolEntry
intersectionsTool =
    { toolType = ToolIntersections
    , toolId = Tools.Intersections.toolId
    , video = Just "https://youtu.be/iWI1ASujFR4"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.emerald
    , textColour = contrastingColour FlatColors.FlatUIPalette.emerald
    , isPopupOpen = False
    }


straightenTool : ToolEntry
straightenTool =
    { toolType = ToolStraighten
    , toolId = Tools.Straightener.toolId
    , video = Just "https://youtu.be/B_LX9BmuoxE"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.AussiePalette.spicedNectarine
    , textColour = contrastingColour FlatColors.AussiePalette.spicedNectarine
    , isPopupOpen = False
    }


landUseTool : ToolEntry
landUseTool =
    { toolType = ToolLandUse
    , toolId = Tools.LandUse.toolId
    , video = Just "https://youtu.be/SgiVpQYxG8I"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = defaultToolColour
    , textColour = contrastingColour defaultToolColour
    , isPopupOpen = False
    }


timestampTool : ToolEntry
timestampTool =
    { toolType = ToolTimestamps
    , toolId = Tools.Timestamp.toolId
    , video = Nothing
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.emerald
    , textColour = contrastingColour FlatColors.FlatUIPalette.emerald
    , isPopupOpen = False
    }


routingTool : ToolEntry
routingTool =
    { toolType = ToolRouting
    , toolId = Tools.MapMatchingRouter.toolId
    , video = Just "https://youtu.be/MYBTArdb_X0"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.TurkishPalette.neonBlue
    , textColour = contrastingColour FlatColors.TurkishPalette.neonBlue
    , isPopupOpen = False
    }


tracksTool : ToolEntry
tracksTool =
    { toolType = ToolTracks
    , toolId = Tools.Tracks.toolId
    , video = Just "https://youtu.be/WOWuwMD7bO0"
    , state = Contracted
    , isVisible = True
    , dock = DockRight
    , tabColour = FlatColors.FlatUIPalette.peterRiver
    , textColour = contrastingColour FlatColors.FlatUIPalette.peterRiver
    , isPopupOpen = False
    }


toolIsSpecial : ToolEntry -> Bool
toolIsSpecial tool =
    -- Must not be allowed to remove these.
    tool.toolId == toolSettings.toolId || tool.toolId == essentialsTool.toolId


toggleToolPopup : String -> String -> ToolEntry -> ToolEntry
toggleToolPopup targetToolId toolId tool =
    -- This, we apply to all tools, to ensure only one popup is ever open.
    if tool.toolId == targetToolId then
        { tool | isPopupOpen = not tool.isPopupOpen }

    else
        { tool | isPopupOpen = False }


setToolState : String -> ToolState -> Dict String ToolEntry -> Dict String ToolEntry
setToolState toolId state tools =
    Dict.update
        toolId
        (Maybe.map (\tool -> { tool | state = state }))
        tools


toggleToolVisible : String -> Dict String ToolEntry -> Dict String ToolEntry
toggleToolVisible toolId tools =
    Dict.update
        toolId
        (Maybe.map
            (\tool ->
                if not <| toolIsSpecial tool then
                    { tool | isVisible = not tool.isVisible, isPopupOpen = False }

                else
                    tool
            )
        )
        tools


unHideTool : String -> Dict String ToolEntry -> Dict String ToolEntry
unHideTool toolId tools =
    Dict.update
        toolId
        (Maybe.map (\tool -> { tool | isVisible = True, isPopupOpen = False }))
        tools


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


setDock : String -> ToolDock -> Dict String ToolEntry -> Dict String ToolEntry
setDock toolId dock tools =
    Dict.update
        toolId
        (Maybe.map (\tool -> { tool | dock = dock, isPopupOpen = False }))
        tools


setColour : String -> Element.Color -> Dict String ToolEntry -> Dict String ToolEntry
setColour toolId colour tools =
    Dict.update
        toolId
        (Maybe.map
            (\tool ->
                { tool
                    | tabColour = colour
                    , textColour = contrastingColour colour
                }
            )
        )
        tools


getColour : String -> Dict String ToolEntry -> Element.Color
getColour toolId tools =
    tools
        |> Dict.get toolId
        |> Maybe.map .tabColour
        |> Maybe.withDefault FlatColors.SwedishPalette.freeSpeechBlue


isToolOpen : String -> Dict String ToolEntry -> Bool
isToolOpen toolId tools =
    tools
        |> Dict.get toolId
        |> Maybe.map (\tab -> (tab.state == Expanded) && tab.isVisible)
        |> Maybe.withDefault False


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

        ToolPopupToggle toolId ->
            let
                newOptions =
                    { options | tools = Dict.map (toggleToolPopup toolId) options.tools }
            in
            ( newOptions
            , [ StoreLocally "tools" <| encodeToolState newOptions ]
            )

        DisplayInfo id tag ->
            ( options, [ Actions.DisplayInfo id tag ] )

        ToolDockSelect toolId toolDock ->
            let
                newOptions =
                    { options | tools = setDock toolId toolDock options.tools }
            in
            ( newOptions
            , [ StoreLocally "tools" <| encodeToolState newOptions ]
            )

        ToolColourSelect toolId color ->
            -- Instantly reflect colour changes in preview.
            let
                newOptions =
                    { options | tools = setColour toolId color options.tools }
            in
            if isToolOpen toolId options.tools then
                toolStateHasChanged toolId True isTrack newOptions

            else
                ( newOptions, [ StoreLocally "tools" <| encodeToolState newOptions ] )

        ToolStateToggle toolId newState ->
            -- Record the new state, but also let the tool know!
            let
                newOptions =
                    { options | tools = setToolState toolId newState options.tools }
            in
            toolStateHasChanged
                toolId
                (isToolOpen toolId newOptions.tools)
                isTrack
                newOptions

        ToolActivate toolId newState ->
            -- Record the new state, but also let the tool know!
            let
                newOptions =
                    { options
                        | tools =
                            setToolState toolId newState <|
                                unHideTool toolId options.tools
                    }
            in
            toolStateHasChanged
                toolId
                (isToolOpen toolId newOptions.tools)
                isTrack
                newOptions

        DirectionChanges msg ->
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            DirectionChanges.update
                                msg
                                options.directionChangeOptions
                                (getColour directionChangeTool.toolId options.tools)
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
                        (getColour gradientChangeTool.toolId options.tools)
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
                        (getColour gradientChangeTool.toolId options.tools)
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
                        (getColour deleteTool.toolId options.tools)
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
                        (getColour essentialsTool.toolId options.tools)
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
                        (getColour bezierSplinesTool.toolId options.tools)
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
                        (getColour centroidAverageTool.toolId options.tools)
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
                        (getColour curveFormerTool.toolId options.tools)
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
                                (getColour bendSmootherTool.toolId options.tools)
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
                                (getColour nudgeTool.toolId options.tools)
                                track
                    in
                    ( { options | nudgeOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

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
                                (getColour simplifyTool.toolId options.tools)
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
                    Interpolate.update
                        msg
                        options.interpolateSettings
                        (getColour interpolateTool.toolId options.tools)
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
                                (getColour profileSmoothTool.toolId options.tools)
                                track
                    in
                    ( { options | profileSmoothSettings = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolMoveScaleRotateMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.MoveScaleRotate.update
                        msg
                        options.moveScaleRotateSettings
                        (getColour moveScaleRotateTool.toolId options.tools)
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
                                (getColour moveAndStretchTool.toolId options.tools)
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
                Just _ ->
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
            case isTrack of
                Just track ->
                    let
                        ( newOptions, actions ) =
                            Tools.Straightener.update
                                msg
                                options.straightenOptions
                                track
                    in
                    ( { options | straightenOptions = newOptions }
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
                                (getColour smartSmootherTool.toolId options.tools)
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
                                (getColour namedSegmentTool.toolId options.tools)
                                (msgWrapper << ToolNamedSegmentMsg)
                    in
                    ( { options | namedSegmentOptions = newOptions }
                    , actions
                    )

                Nothing ->
                    ( options, [] )

        ToolRoutingMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.MapMatchingRouter.update
                        msg
                        options.routingOptions
                        (msgWrapper << ToolRoutingMsg)
            in
            ( { options | routingOptions = newOptions }
            , actions
            )

        ToolTracksMsg msg ->
            let
                ( newOptions, actions ) =
                    Tools.Tracks.update
                        msg
                        options.tracksOptions
            in
            ( { options | tracksOptions = newOptions }
            , actions
            )

        ToolToggleSort azSort ->
            let
                newOptions =
                    { options | azSort = azSort }
            in
            ( newOptions
            , [ StoreLocally "tool:tools" <| encodeToolSummaryState newOptions ]
            )

        ToolToggleCompact compact ->
            let
                newOptions =
                    { options | compact = compact }
            in
            ( newOptions
            , [ StoreLocally "tool:tools" <| encodeToolSummaryState newOptions ]
            )

        ToolToggleVisible toolId ->
            -- Record the new state, but also let the tool know!
            let
                newOptions =
                    { options | tools = toggleToolVisible toolId options.tools }
            in
            toolStateHasChanged
                toolId
                (isToolOpen toolId newOptions.tools)
                isTrack
                newOptions

        ToolSetPaintTool toolId ->
            let
                newPaintTool =
                    if Just toolId == options.paintTool then
                        Nothing

                    else
                        Just toolId

                ( newOptions, actions ) =
                    -- See if this will hide any current preview.
                    toolStateHasChanged
                        toolId
                        False
                        isTrack
                        { options | paintTool = newPaintTool }
            in
            ( newOptions
            , actions
            )


refreshOpenTools :
    Maybe (TrackLoaded msg)
    -> Options msg
    -> ( Options msg, List (ToolAction msg) )
refreshOpenTools isTrack options =
    -- Track, or something, has changed; tool data is stale.
    -- Same impact as tools being opened, so we'll re-use that.
    let
        refreshOpenTool toolId entry ( inputOptions, collectingActions ) =
            if entry.state == Expanded || entry.state == AlwaysOpen then
                let
                    ( incrementalModel, incrementalActions ) =
                        toolStateHasChanged toolId True isTrack inputOptions
                in
                ( incrementalModel, incrementalActions ++ collectingActions )

            else
                ( inputOptions, collectingActions )
    in
    options.tools |> Dict.foldl refreshOpenTool ( options, [] )


toolStateHasChanged :
    String
    -> Bool
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> ( Options msg, List (ToolAction msg) )
toolStateHasChanged toolId requestPreviews isTrack options =
    --TODO: Factor out the "StoreLocally" aspect.
    --If using freehand paint mode, only preview the tool that is the painting tool.
    let
        showPreviews =
            case options.paintTool of
                Just paintTool ->
                    False

                --requestPreviews && (toolId == paintTool)
                Nothing ->
                    requestPreviews
    in
    case
        Dict.get toolId options.tools
            |> Maybe.map .toolType
            |> Maybe.withDefault ToolTrackInfo
    of
        ToolTrackInfo ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolAbruptDirectionChanges ->
            let
                ( newToolOptions, actions ) =
                    DirectionChanges.toolStateChange
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
                        options.deleteOptions
                        isTrack

                newOptions =
                    { options | deleteOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolEssentials ->
            let
                ( newToolOptions, _ ) =
                    Tools.Essentials.toolStateChange
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
                        options.simplifySettings
                        isTrack

                newOptions =
                    { options | simplifySettings = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolInterpolate ->
            let
                ( newToolOptions, actions ) =
                    Interpolate.toolStateChange
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
                        options.intersectionOptions
                        isTrack

                newOptions =
                    { options | intersectionOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolStraighten ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolSettings ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolLandUse ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolSmartSmoother ->
            let
                ( newToolOptions, actions ) =
                    Tools.SmartSmoother.toolStateChange
                        showPreviews
                        (getColour toolId options.tools)
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
                        showPreviews
                        (getColour toolId options.tools)
                        options.namedSegmentOptions
                        isTrack

                newOptions =
                    { options | namedSegmentOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )

        ToolRouting ->
            ( options, [ StoreLocally "tools" <| encodeToolState options ] )

        ToolTracks ->
            let
                ( newToolOptions, actions ) =
                    Tools.Tracks.toolStateChange
                        showPreviews
                        options.tracksOptions

                newOptions =
                    { options | tracksOptions = newToolOptions }
            in
            ( newOptions, (StoreLocally "tools" <| encodeToolState options) :: actions )


anyToolsInLeftDock : Options msg -> Bool
anyToolsInLeftDock options =
    orderedTools
        |> List.filterMap (\( id, _ ) -> Dict.get id options.tools)
        |> List.filter (\t -> t.dock == DockLeft)
        |> List.isEmpty
        |> not



--View stuff


toolsForDock :
    SystemSettings
    -> ToolDock
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> Element msg
toolsForDock settings dock msgWrapper isTrack options =
    let
        filterForDock =
            if settings.singleDock then
                always True

            else
                \t -> t.dock == dock

        visibleToolsInOrder =
            orderedTools
                |> List.filterMap (\( id, _ ) -> Dict.get id options.tools)
                |> List.filter filterForDock
                |> List.filter .isVisible
    in
    column [ width fill, height fill ]
        [ column [ width fill, height fill, spacing 5, padding 10, scrollbarY ]
            [ column [ width fill, spacing 4 ]
                (visibleToolsInOrder
                    |> List.filter
                        (\t -> t.state == AlwaysOpen || t.state == SettingsOpen || t.state == SettingsClosed)
                    |> List.map (viewTool settings msgWrapper isTrack options)
                )
            , wrappedRow
                -- Open tools
                [ spacing 4 ]
              <|
                (visibleToolsInOrder
                    |> List.filter (\t -> t.state == Expanded)
                    |> List.map (viewTool settings msgWrapper isTrack options)
                )
            , wrappedRow
                -- Closed tools
                [ spacing 4 ]
              <|
                (visibleToolsInOrder
                    |> List.filter (\t -> t.state == Contracted)
                    |> List.map (viewTool settings msgWrapper isTrack options)
                )
            ]
        ]


viewToolSettings : SystemSettings -> Options msg -> (ToolMsg -> msg) -> Element msg
viewToolSettings settings options wrapper =
    let
        i18n =
            I18N.localisedString settings.location "tools"

        optionHelper =
            compactRadioButton << i18n

        fullOptionList tool =
            if settings.singleDock then
                []

            else
                [ Input.optionWith DockLeft <| optionHelper "onleft"
                , Input.optionWith DockRight <| optionHelper "onright"
                ]

        inactiveCheckbox =
            Element.el
                [ Element.width (Element.px 20)
                , Element.height (Element.px 14)
                , Element.centerY
                , Font.size 9
                , Font.center
                , Border.rounded 3
                , Border.color <| Element.rgb (59 / 255) (153 / 255) (252 / 255)
                ]
                none

        visible : ToolEntry -> Element msg
        visible tool =
            if toolIsSpecial tool then
                inactiveCheckbox

            else
                Input.checkbox [ width (px 20) ]
                    { label = Input.labelHidden "visible"
                    , onChange =
                        wrapper
                            << always
                                (if tool.toolType == ToolSettings || tool.toolType == ToolEssentials then
                                    ToolNoOp

                                 else
                                    ToolToggleVisible tool.toolId
                                )
                    , checked = tool.isVisible
                    , icon = Input.defaultCheckbox
                    }

        locationChoices : ToolEntry -> Element msg
        locationChoices tool =
            Input.radioRow
                [ spacing 5
                , paddingEach { top = 4, left = 4, bottom = 0, right = 0 }
                ]
                { onChange = wrapper << ToolDockSelect tool.toolId
                , selected = Just tool.dock
                , label =
                    Input.labelRight [ paddingXY 10 0 ] <|
                        row [ spacing 4 ]
                            [ visible tool
                            , infoButton (wrapper <| DisplayInfo tool.toolId "info")
                            , compactListing tool
                            ]
                , options = fullOptionList tool
                }

        compactListing : ToolEntry -> Element msg
        compactListing tool =
            Input.button
                [ --spacing 5
                  --, paddingEach { top = 4, left = 4, bottom = 0, right = 0 }
                  Border.width 1
                , Border.color FlatColors.FlatUIPalette.silver
                , Border.rounded 4
                , padding 2
                ]
                { onPress =
                    if tool.toolType == ToolSettings || tool.toolType == ToolEssentials then
                        Nothing

                    else
                        Just <|
                            wrapper <|
                                ToolActivate tool.toolId <|
                                    nextToolState tool.state
                , label = text <| I18N.localisedString settings.location tool.toolId "label"
                }

        sortMethod : Element msg
        sortMethod =
            Input.checkbox []
                { onChange = wrapper << ToolToggleSort
                , icon = Input.defaultCheckbox
                , checked = options.azSort
                , label = Input.labelRight [] (text <| i18n "A-Z")
                }

        compact : Element msg
        compact =
            Input.checkbox []
                { onChange = wrapper << ToolToggleCompact
                , icon = Input.defaultCheckbox
                , checked = options.compact
                , label = Input.labelRight [] (text <| i18n "Compact")
                }

        displayStyle =
            if options.compact then
                compactListing

            else
                locationChoices
    in
    column
        ([ width fill
         , height <|
            if options.compact then
                px 250

            else
                px 400
         , scrollbarY
         , padding 10
         , spacing 3
         ]
            ++ CommonToolStyles.toolContentBoxStyle settings
        )
        [ wrappedRow [ width fill, centerX, spacing 4, padding 4 ] [ sortMethod, compact ]
        , if options.compact then
            wrappedRow [ spacing 6 ] (List.map compactListing <| sortedTools settings options)

          else
            column [] (List.map locationChoices <| sortedTools settings options)
        ]


sortedTools : SystemSettings -> Options msg -> List ToolEntry
sortedTools settings options =
    --If A-Z then sort by labels, otherwise use `orderedList`.
    if options.azSort then
        options.tools
            |> Dict.values
            |> List.sortBy (\tool -> I18N.localisedString settings.location tool.toolId "label")

    else
        orderedTools
            |> List.filterMap (\( id, _ ) -> Dict.get id options.tools)


viewTool :
    SystemSettings
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> ToolEntry
    -> Element msg
viewTool settings msgWrapper isTrack options toolEntry =
    -- Possible performance gain by being lazy here. Who knows?
    Element.Lazy.lazy5
        viewToolLazy
        settings
        msgWrapper
        isTrack
        options
        toolEntry


viewToolLazy :
    SystemSettings
    -> (ToolMsg -> msg)
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> ToolEntry
    -> Element msg
viewToolLazy settings msgWrapper isTrack options toolEntry =
    let
        popup =
            inFront <|
                column
                    [ alignRight
                    , moveDown 26
                    , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ToolNoOp >> msgWrapper)
                    , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ToolNoOp >> msgWrapper)
                    , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ToolNoOp >> msgWrapper)
                    , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ToolNoOp >> msgWrapper)
                    , htmlAttribute (style "z-index" "20")
                    ]
                    [ showDockOptions settings msgWrapper toolEntry
                    , showColourOptions msgWrapper toolEntry
                    ]
    in
    column
        [ width fill
        , alignTop
        , htmlAttribute (style "vertical-align" "top")
        , spacing 0
        , popup
        ]
        [ row
            (case toolEntry.state of
                Expanded ->
                    [ width fill
                    , spacing 8
                    , padding 7
                    , Font.color <| contrastingColour toolEntry.tabColour
                    , Background.color toolEntry.tabColour
                    , Border.roundEach { topRight = 8, topLeft = 8, bottomRight = 0, bottomLeft = 0 }
                    ]

                AlwaysOpen ->
                    [ width fill
                    , height <| px 0
                    ]

                _ ->
                    [ width fill
                    , spacing 8
                    , padding 7
                    , Font.color <| contrastingColour toolEntry.tabColour
                    , Background.color toolEntry.tabColour
                    , Border.rounded 8
                    ]
            )
            [ case ( toolEntry.video, toolEntry.state ) of
                ( Just video, Expanded ) ->
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

                _ ->
                    none
            , Input.button
                [ centerX, centerY ]
                { onPress =
                    Just <|
                        msgWrapper <|
                            ToolStateToggle toolEntry.toolId <|
                                nextToolState toolEntry.state
                , label = I18N.text settings.location toolEntry.toolId "label"
                }
            , if toolEntry.state == Expanded && Dict.member toolEntry.toolId paintingTools then
                Input.button
                    [ alignRight ]
                    { onPress = Just <| msgWrapper <| ToolSetPaintTool toolEntry.toolId
                    , label = useIconWithSize 14 FeatherIcons.penTool
                    }

              else
                none
            , if toolEntry.state == Expanded then
                Input.button
                    [ alignRight
                    , htmlAttribute <|
                        Mouse.onWithOptions
                            "click"
                            stopProp
                            (always << msgWrapper <| ToolPopupToggle toolEntry.toolId)
                    ]
                    { onPress = Just <| msgWrapper <| ToolPopupToggle toolEntry.toolId
                    , label = useIconWithSize 14 FeatherIcons.settings
                    }

              else
                none
            ]
        , if toolEntry.state == Expanded || toolEntry.state == AlwaysOpen || toolEntry.state == SettingsOpen then
            viewToolByType settings msgWrapper toolEntry isTrack options

          else
            none
        ]


viewToolForPainting :
    SystemSettings
    -> Options msg
    -> Maybe String
    -> Element msg
viewToolForPainting settings options toolId =
    case toolId of
        Just isTool ->
            case Dict.get isTool options.tools of
                Just toolEntry ->
                    el
                        [ centerX
                        , alignTop
                        , spacing 8
                        , padding 7
                        , Font.color <| contrastingColour toolEntry.tabColour
                        , Background.color toolEntry.tabColour
                        , Border.color <| contrastingColour toolEntry.tabColour
                        , Border.roundEach { topRight = 0, topLeft = 0, bottomRight = 8, bottomLeft = 8 }
                        ]
                    <|
                        I18N.text settings.location toolEntry.toolId "label"

                Nothing ->
                    none

        Nothing ->
            none


makePaintPreview :
    Options msg
    -> String
    -> PointLeafProximity
    -> PointLeafProximity
    -> TrackLoaded msg
    -> Maybe PreviewData
makePaintPreview options toolId point1 point2 track =
    --Delegate to the tool to do its normal (or not normal) preview creation.
    --Should return Nothing if there is no "solution".
    --We won't even bother calling it unless the end points are distinct and on-track.
    --TODO: Some tools are special (Radiused Bends) and may have their own methods, this is the default.
    let
        ( snap1, snap2 ) =
            ( TrackLoaded.snapToTrack track point1
            , TrackLoaded.snapToTrack track point2
            )

        pointsAreDifferent =
            point1.leafIndex
                /= point2.leafIndex
                || point1.proportionAlong
                /= point2.proportionAlong

        trackWithPaintPointsAdded =
            Just <| TrackLoaded.insertPointsAt snap1 snap2 track
    in
    if
        pointsAreDifferent
            && Quantity.lessThanOrEqualTo (Length.meters 2) point1.distanceFrom
            && Quantity.lessThanOrEqualTo (Length.meters 2) point2.distanceFrom
    then
        let
            ( _, actions ) =
                --Last argument is hack to force preview.
                toolStateHasChanged
                    toolId
                    True
                    trackWithPaintPointsAdded
                    { options | paintTool = Nothing }

            preview =
                actions
                    |> List.filterMap
                        (\act ->
                            case act of
                                ShowPreview previewData ->
                                    Just previewData

                                _ ->
                                    Nothing
                        )
        in
        List.head preview

    else
        Nothing


applyPaintTool : Options msg -> String -> PointLeafProximity -> PointLeafProximity -> TrackLoaded msg -> TrackLoaded msg
applyPaintTool tools toolId point1 point2 track =
    --TODO: Use tool-specific apply, as semantics vary.
    let
        ( snap1, snap2 ) =
            -- Snap to centreline, or things like Insert look poor.
            ( TrackLoaded.snapToTrack track point1
            , TrackLoaded.snapToTrack track point2
            )

        trackWithPaintPointsAdded =
            TrackLoaded.insertPointsAt snap1 snap2 track

        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers trackWithPaintPointsAdded
    in
    case Dict.get toolId tools.tools |> Maybe.map .toolType of
        Just ToolDeletePoints ->
            DeletePoints.delete fromStart fromEnd trackWithPaintPointsAdded

        Just ToolInterpolate ->
            Interpolate.applyFromPaint tools.interpolateSettings trackWithPaintPointsAdded

        _ ->
            applyPaintToolGeneric tools toolId snap1 snap2 track


applyPaintToolGeneric : Options msg -> String -> PointLeafProximity -> PointLeafProximity -> TrackLoaded msg -> TrackLoaded msg
applyPaintToolGeneric tools toolId point1 point2 track =
    --This was good PoC, might serve for tools with "normal" semantics.
    case makePaintPreview tools toolId point1 point2 track of
        Just previewData ->
            let
                trackWithPaintPointsAdded =
                    TrackLoaded.insertPointsAt point1 point2 track

                ( fromStart, fromEnd ) =
                    TrackLoaded.getRangeFromMarkers trackWithPaintPointsAdded

                newTree =
                    DomainModel.replaceRange
                        fromStart
                        fromEnd
                        track.referenceLonLat
                        (List.map .gpx previewData.points)
                        trackWithPaintPointsAdded.trackTree

                sameCountFromEnd x =
                    DomainModel.skipCount (Maybe.withDefault track.trackTree newTree)
                        - (DomainModel.skipCount track.trackTree - x)

                ( newOrange, newPurple ) =
                    case track.markerPosition of
                        Just purple ->
                            if track.currentPosition <= purple then
                                --Orange from start, purple from end
                                ( track.currentPosition
                                , Just <| sameCountFromEnd purple
                                )

                            else
                                --Orange from end, purple from start
                                ( sameCountFromEnd track.currentPosition
                                , Just purple
                                )

                        Nothing ->
                            ( track.currentPosition, Nothing )
            in
            case newTree of
                Just isTree ->
                    { trackWithPaintPointsAdded
                        | trackTree = isTree
                        , currentPosition = newOrange
                        , markerPosition = newPurple
                    }

                Nothing ->
                    track

        Nothing ->
            track


showDockOptions : SystemSettings -> (ToolMsg -> msg) -> ToolEntry -> Element msg
showDockOptions settings msgWrapper toolEntry =
    if toolEntry.isPopupOpen then
        row
            (spacing 4 :: neatToolsBorder)
            [ Input.button
                [ tooltip below (localisedTooltip settings.location "tools" "left") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolId DockLeft
                , label = useIcon FeatherIcons.arrowLeft
                }
            , Input.button
                [ tooltip below (localisedTooltip settings.location "tools" "right") ]
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolId DockRight
                , label = useIcon FeatherIcons.arrowRight
                }
            , if toolEntry.toolType /= ToolSettings && toolEntry.toolType /= ToolEssentials then
                Input.button
                    [ tooltip below (localisedTooltip settings.location "tools" "hide")
                    , paddingEach { left = 20, right = 0, top = 0, bottom = 0 }
                    ]
                    { onPress = Just <| msgWrapper <| ToolToggleVisible toolEntry.toolId
                    , label = useIcon FeatherIcons.eyeOff
                    }

              else
                none
            ]

    else
        none


clearPopups : Options msg -> Options msg
clearPopups options =
    let
        clearPopup _ tool =
            { tool | isPopupOpen = False }
    in
    { options | tools = Dict.map clearPopup options.tools }


showColourOptions : (ToolMsg -> msg) -> ToolEntry -> Element msg
showColourOptions msgWrapper toolEntry =
    let
        colourBlock colour =
            Input.button
                [ Background.color colour, width <| px 20, height <| px 20 ]
                { label = none
                , onPress = Just <| msgWrapper <| ToolColourSelect toolEntry.toolId colour
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
    SystemSettings
    -> (ToolMsg -> msg)
    -> ToolEntry
    -> Maybe (TrackLoaded msg)
    -> Options msg
    -> Element msg
viewToolByType settings msgWrapper entry isTrack options =
    el
        (CommonToolStyles.toolContentBoxStyle settings)
    <|
        case entry.toolType of
            ToolTimestamps ->
                Tools.Timestamp.view
                    settings
                    (msgWrapper << ToolTimestampMsg)
                    options.timestampOptions
                    isTrack

            ToolTrackInfo ->
                TrackInfoBox.view
                    settings
                    (msgWrapper << ToolInfoMsg)
                    isTrack
                    options.infoOptions

            ToolAbruptDirectionChanges ->
                DirectionChanges.view
                    settings
                    (msgWrapper << DirectionChanges)
                    options.directionChangeOptions
                    isTrack

            ToolGradientProblems ->
                Tools.GradientProblems.view
                    settings
                    (msgWrapper << ToolGradientChangeMsg)
                    options.gradientProblemOptions
                    isTrack

            ToolDeletePoints ->
                case isTrack of
                    Just track ->
                        DeletePoints.view
                            settings
                            (msgWrapper << DeletePoints)
                            options.deleteOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolEssentials ->
                Tools.Essentials.view
                    settings
                    (msgWrapper << ToolEssentialsMsg)
                    options.essentialOptions
                    isTrack

            ToolBezierSplines ->
                case isTrack of
                    Just track ->
                        Tools.BezierSplines.view
                            settings
                            (msgWrapper << ToolBezierMsg)
                            options.bezierSplineOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolCentroidAverage ->
                case isTrack of
                    Just track ->
                        Tools.CentroidAverage.view
                            settings
                            (msgWrapper << ToolCentroidMsg)
                            options.centroidAverageOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolCurveFormer ->
                Tools.CurveFormer.view
                    settings
                    (msgWrapper << ToolCurveFormerMsg)
                    options.curveFormerOptions
                    isTrack

            ToolBendSmoother ->
                Tools.BendSmoother.view
                    settings
                    (msgWrapper << ToolBendSmootherMsg)
                    options.bendSmootherOptions
                    isTrack

            ToolNudge ->
                Tools.Nudge.view
                    settings
                    options.nudgeOptions
                    (msgWrapper << ToolNudgeMsg)
                    isTrack

            ToolDisplaySettings ->
                Tools.DisplaySettings.view
                    settings
                    (msgWrapper << ToolDisplaySettingMsg)
                    options.displaySettings

            ToolOutAndBack ->
                Tools.OutAndBack.view
                    settings
                    (msgWrapper << ToolOutAndBackMsg)
                    options.outAndBackSettings
                    isTrack

            ToolSimplify ->
                Tools.Simplify.view
                    settings
                    (msgWrapper << ToolSimplifyMsg)
                    options.simplifySettings
                    isTrack

            ToolInterpolate ->
                Interpolate.view
                    settings
                    (msgWrapper << ToolInterpolateMsg)
                    options.interpolateSettings
                    isTrack

            ToolProfileSmooth ->
                case isTrack of
                    Just track ->
                        Tools.ProfileSmooth.view
                            settings
                            options.profileSmoothSettings
                            (msgWrapper << ToolProfileSmoothMsg)
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolMoveScaleRotate ->
                Tools.MoveScaleRotate.view
                    settings
                    options.moveScaleRotateSettings
                    (msgWrapper << ToolMoveScaleRotateMsg)
                    isTrack

            ToolFlythrough ->
                Tools.Flythrough.view
                    settings
                    options.flythroughSettings
                    (msgWrapper << ToolFlythroughMsg)

            ToolStrava ->
                Tools.StravaTools.viewStravaTab
                    settings
                    options.stravaSettings
                    (msgWrapper << ToolStravaMsg)
                    isTrack

            ToolMoveAndStretch ->
                case isTrack of
                    Just track ->
                        Tools.MoveAndStretch.view
                            settings
                            options.moveAndStretchSettings
                            (msgWrapper << ToolMoveAndStretchMsg)
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolStartFinish ->
                case isTrack of
                    Just track ->
                        Tools.StartFinish.view
                            settings
                            options.startFinishOptions
                            track
                            (msgWrapper << ToolStartFinishMsg)

                    Nothing ->
                        noTrackMessage settings

            ToolSplitAndJoin ->
                case isTrack of
                    Just track ->
                        Tools.SplitAndJoin.view
                            settings
                            options.splitAndJoinOptions
                            (msgWrapper << ToolSplitJoinMsg)
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolIntersections ->
                case isTrack of
                    Just track ->
                        Tools.Intersections.view
                            settings
                            (msgWrapper << ToolIntersectionMsg)
                            options.intersectionOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolStraighten ->
                case isTrack of
                    Just track ->
                        Tools.Straightener.view
                            settings
                            (msgWrapper << ToolStraightenMsg)
                            options.straightenOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolSettings ->
                viewToolSettings settings options msgWrapper

            ToolLandUse ->
                Tools.LandUse.view
                    settings
                    (msgWrapper << ToolLandUseMsg)
                    options.landUseOptions
                    isTrack

            ToolSmartSmoother ->
                case isTrack of
                    Just track ->
                        Tools.SmartSmoother.view
                            settings
                            (msgWrapper << ToolSmartSmootherMsg)
                            options.smartSmootherOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolNamedSegments ->
                case isTrack of
                    Just track ->
                        Tools.NamedSegment.view
                            settings
                            (msgWrapper << ToolNamedSegmentMsg)
                            options.namedSegmentOptions
                            track

                    Nothing ->
                        noTrackMessage settings

            ToolRouting ->
                Tools.MapMatchingRouter.view
                    settings
                    isTrack
                    (msgWrapper << ToolRoutingMsg)
                    options.routingOptions

            ToolTracks ->
                Tools.Tracks.view
                    settings
                    (msgWrapper << ToolTracksMsg)
                    options.tracksOptions



-- Local storage management


type alias StoredTool =
    { toolType : String
    , state : String
    , dock : String
    , tab : ColourTriplet
    , text : ColourTriplet
    , visible : Maybe Bool
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

        ToolSettings ->
            "ToolSettings"

        ToolLandUse ->
            "ToolLandUse"

        ToolSmartSmoother ->
            "ToolTreeSmoother"

        ToolNamedSegments ->
            "ToolNamedSegments"

        ToolRouting ->
            "ToolRouting"

        ToolTracks ->
            "ToolTracks"


encodeColour : Element.Color -> E.Value
encodeColour colour =
    let
        { red, green, blue } =
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
        DockLeft ->
            "upperleft"

        DockRight ->
            "upperright"


decodeDock : String -> ToolDock
decodeDock dock =
    case dock of
        "upperleft" ->
            DockLeft

        "upperright" ->
            DockRight

        _ ->
            DockRight


encodeOneTool : ToolEntry -> E.Value
encodeOneTool tool =
    E.object
        [ ( "type", E.string <| encodeType tool.toolType )
        , ( "state", E.string <| encodeState tool.state )
        , ( "dock", E.string <| encodeDock tool.dock )
        , ( "tab", encodeColour tool.tabColour )
        , ( "text", encodeColour tool.textColour )
        , ( "visible", E.bool tool.isVisible )
        ]


encodeToolState : Options msg -> E.Value
encodeToolState options =
    E.list identity <| List.map encodeOneTool <| Dict.values options.tools


encodeToolSummaryState : Options msg -> E.Value
encodeToolSummaryState options =
    E.object
        [ ( "AZ", E.bool options.azSort )
        , ( "compact", E.bool options.compact )
        ]


colourDecoder =
    D.map3 ColourTriplet
        (field "red" D.float)
        (field "green" D.float)
        (field "blue" D.float)


toolDecoder =
    D.map6 StoredTool
        (field "type" D.string)
        (field "state" D.string)
        (field "dock" D.string)
        (field "tab" colourDecoder)
        (field "text" colourDecoder)
        (maybe (field "visible" D.bool))


restoreStoredValues : Options msg -> D.Value -> Options msg
restoreStoredValues options values =
    -- Care! Need to overlay restored values on to the current tools.
    let
        toolsAsStored =
            D.decodeValue (D.list toolDecoder) values

        useStoredSettings : List StoredTool -> String -> ToolEntry -> ToolEntry
        useStoredSettings stored toolId tool =
            case List.Extra.find (\fromStore -> fromStore.toolType == encodeType tool.toolType) stored of
                Just found ->
                    { tool
                        | state = decodeState found.state
                        , dock = decodeDock found.dock
                        , tabColour = decodeColour found.tab
                        , textColour = decodeColour found.text
                        , isVisible = Maybe.withDefault True found.visible
                    }

                Nothing ->
                    tool
    in
    case toolsAsStored of
        Ok stored ->
            { options | tools = Dict.map (useStoredSettings stored) options.tools }

        Err _ ->
            options


type alias MyOptions =
    { compact : Bool, az : Bool }


restoreSettings : Options msg -> D.Value -> Options msg
restoreSettings options values =
    let
        asStored =
            D.map2 MyOptions
                (D.field "compact" D.bool)
                (D.field "AZ" D.bool)
    in
    case D.decodeValue asStored values of
        Ok { az, compact } ->
            { options | azSort = az, compact = compact }

        Err _ ->
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

        Err _ ->
            options


restoreMeasure : SystemSettings -> D.Value -> SystemSettings
restoreMeasure settings value =
    -- Care! Need to overlay restored values on to the current tools.
    let
        decoded =
            D.decodeValue D.bool value
    in
    case decoded of
        Ok setting ->
            { settings | imperial = setting }

        Err _ ->
            settings



-- Dock stuff


type alias DockSettings =
    { dockPopupOpen : Bool
    , dockLabel : String
    , dockLabelColour : Element.Color
    }


type ToolDock
    = DockLeft
    | DockRight


defaultDockColour =
    FlatColors.FlatUIPalette.wetAsphalt


dockList =
    [ ( "1", DockSettings False "Upper left" defaultDockColour )
    , ( "2", DockSettings False "Lower left" defaultDockColour )
    , ( "3", DockSettings False "Central" defaultDockColour )
    , ( "4", DockSettings False "Lower right" defaultDockColour )
    , ( "5", DockSettings False "Upper right" defaultDockColour )
    ]


subscriptions : Options msg -> Sub ToolMsg
subscriptions options =
    Sub.batch
        [ Sub.map ToolFlythroughMsg <| Tools.Flythrough.subscriptions options.flythroughSettings ]
