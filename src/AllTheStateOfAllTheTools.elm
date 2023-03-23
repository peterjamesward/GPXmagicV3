module AllTheStateOfAllTheTools exposing (..)

import Dict exposing (Dict)
import Tools.BendSmootherOptions as BendSmootherOptions
import Tools.BezierOptions as BezierOptions
import Tools.CentroidAverageOptions as CentroidAverageOptions
import Tools.CurveFormerOptions as CurveFormerOptions
import Tools.DeletePoints as DeletePoints
import Tools.DirectionChanges as DirectionChanges
import Tools.DisplaySettingsOptions as DisplaySettingsOptions
import Tools.Essentials as Essentials
import Tools.Flythrough as Flythrough
import Tools.GradientProblems as GradientProblems
import Tools.InterpolateOptions as InterpolateOptions
import Tools.Intersections as Intersections
import Tools.LandUse as LandUse
import Tools.MapMatchingRouterOptions as MapMatchingRouterOptions
import Tools.MoveAndStretchOptions as MoveAndStretchOptions
import Tools.MoveScaleRotateOptions as MoveScaleRotateOptions
import Tools.NamedSegmentOptions as NamedSegmentOptions
import Tools.NudgeOptions as NudgeOptions
import Tools.OutAndBackOptions as OutAndBackOptions
import Tools.ProfileSmoothOptions as ProfileSmoothOptions
import Tools.Simplify as Simplify
import Tools.SmartSmootherOptions as SmartSmootherOptions
import Tools.SplitAndJoinOptions as SplitAndJoinOptions
import Tools.StartFinishTypes as StartFinishTypes
import Tools.Straightener as Straightener
import Tools.StravaOptions as StravaOptions
import Tools.TimestampOptions as TimestampOptions
import Tools.TrackInfoBox as TrackInfoBox
import Tools.TracksOptions as Tracks


type AllState msg
    = DirectionChanges DirectionChanges.Options
    | DeletePoints DeletePoints.Options
    | Essentials Essentials.Options
    | Bezier BezierOptions.Options
    | CentroidAverage CentroidAverageOptions.Options
    | CurveFormer CurveFormerOptions.Options
    | BendSmoother BendSmootherOptions.Options
    | NudgeOptions NudgeOptions.Options
    | TrackInfoBox TrackInfoBox.Options
    | GradientProblems GradientProblems.Options
    | DisplaySettings DisplaySettingsOptions.Options
    | OutAndBack OutAndBackOptions.Options
    | Simplify Simplify.Options
    | Interpolate InterpolateOptions.Options
    | ProfileSmooth ProfileSmoothOptions.Options
    | MoveScaleRotate MoveScaleRotateOptions.Options
    | Flythrough Flythrough.Options
    | Strava StravaOptions.Options
    | MoveAndStretch MoveAndStretchOptions.Options
    | StartFinish StartFinishTypes.Options
    | SplitAndJoin SplitAndJoinOptions.Options
    | Intersections Intersections.Options
    | Straightener Straightener.Options
    | LandUse LandUse.Options
    | SmartSmoother SmartSmootherOptions.Options
    | NamedSegment NamedSegmentOptions.Options
    | Timestamp TimestampOptions.Options
    | MapMatchingRouter MapMatchingRouterOptions.Options
    | Tracks (Tracks.Options msg)


type alias BigToolStateDict msg =
    Dict String (AllState msg)
