module Tools.DeletePoints exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Angle exposing (Angle)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), asRecord, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette
import List.Extra
import Quantity
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showAngle)
import ViewPureStyles exposing (neatToolsBorder, sliderThumb, useIcon)


type alias Options =
    { dummy : Int
    }


defaultOptions =
    { dummy = 0
    }


type Msg
    = Delete Int Int


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            -- Make sure we have up to date breaches and preview is shown.
            ( options
            , [ ShowPreview
                    { tag = "delete"
                    , shape = PreviewCircle
                    , colour = colour
                    , points =
                        --TODO: list of points between markers
                        DomainModel.buildPreview
                            (List.map Tuple.first [])
                            theTrack.trackTree
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "delete" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe TrackLoaded
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case msg of
        Delete from to ->
            ( options, [] )


view : (Msg -> msg) -> Options -> Element msg
view msgWrapper options =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        column [ centerX, padding 4, spacing 4, height <| px 100 ]
            []
