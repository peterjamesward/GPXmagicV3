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
import UtilsForViews exposing (elmuiColour)
import ViewPureStyles exposing (contrastingColour)


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


toolID : String
toolID =
    "landuse"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Land use" )
        , ( "info", """Displays the colour legend for land use data, and a list of named places.""" )
        ]
    )


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


view : (Msg -> msg) -> Options -> Element msg
view wrap options =
    let
        modeSelection =
            Input.radioRow [ centerX, spacing 5 ]
                { onChange = wrap << SetMode
                , options =
                    [ Input.option Legend (text "Legend")
                    , Input.option Names (text "Places")
                    ]
                , selected = Just options.mode
                , label = Input.labelHidden "Mode"
                }
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        column [ padding 4, spacing 6, width fill ]
            [ none

            --, modeSelection
            , case options.mode of
                Legend ->
                    legend

                Names ->
                    none
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
            Dict.toList SceneBuilder3D.landUseColours


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
