module Tools.LandUse exposing (Mode(..), Msg(..), Options, defaultOptions, toolId, update, view)

import Actions exposing (ToolAction)
import Color
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import FlatColors.ChinesePalette
import LandUseDataTypes
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOPtions
import Tools.LandUseColours exposing (landUseColours)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (elmuiColour)
import ViewPureStyles exposing (contrastingColour)


toolId =
    "landuse"


type alias Options =
    { mode : Mode }


type Mode
    = Legend


defaultOptions : Options
defaultOptions =
    { mode = Legend
    }


type Msg
    = SetMode Mode


update :
    Msg
    -> (Msg -> msg)
    -> Options
    -> ( Options, List (ToolAction msg) )
update msg wrapper options =
    case msg of
        SetMode mode ->
            ( { options | mode = mode }, [] )


view : I18NOPtions.Location -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location wrap options maybeTrack =
    let
        i18n =
            I18N.text location toolId

        status =
            case maybeTrack of
                Just track ->
                    case track.landUseData.status of
                        LandUseDataTypes.LandUseError err ->
                            text err

                        LandUseDataTypes.LandUseNoTrack ->
                            i18n "notrack"

                        LandUseDataTypes.LandUseWaitingOSM ->
                            i18n "waiting"

                        LandUseDataTypes.LandUseWaitingMap ->
                            i18n "altitude"

                        LandUseDataTypes.LandUseOK ->
                            i18n "success"

                Nothing ->
                    i18n "notrack"
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        column [ padding 4, spacing 6, width fill ]
            [ none
            , paragraph [] [ status ]
            , legend
            ]


legend : Element msg
legend =
    let
        showLegendEntry : ( String, Color.Color ) -> Element msg
        showLegendEntry ( name, colour ) =
            el
                [ Background.color <| elmuiColour colour
                , Font.color <| contrastingColour <| elmuiColour colour
                , padding 5
                , Font.bold
                ]
            <|
                text name
    in
    wrappedRow
        [ spacingXY 6 6
        , alignTop
        , padding 6
        ]
    <|
        List.map showLegendEntry <|
            Dict.toList landUseColours
