module Tools.LandUse exposing (..)

import Actions exposing (ToolAction(..))
import Color
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import LandUseDataTypes
import SceneBuilder3D
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
    | Names


defaultOptions : Options
defaultOptions =
    { mode = Legend
    }


type Msg
    = SetMode Mode
    | CentreOnPlace String
    | DisplayInfo String String


update :
    Msg
    -> (Msg -> msg)
    -> Options
    -> ( Options, List (ToolAction msg) )
update msg wrapper options =
    case msg of
        SetMode mode ->
            ( { options | mode = mode }, [] )

        CentreOnPlace name ->
            ( options, [] )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )


view : I18NOPtions.Options -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
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
            , paragraph [] [  status ]
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


names : LandUseDataTypes.LandUseData -> Element msg
names landUse =
    let
        showNameEntry : ( String, EarthPoint ) -> Element msg
        showNameEntry ( name, position ) =
            el
                [ padding 5
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
        (landUse.places |> Dict.toList |> List.map showNameEntry)
