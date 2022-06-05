module Tools.SplitAndJoin exposing (..)

import Actions exposing (ToolAction)
import Delay
import Dict exposing (Dict)
import DomainModel exposing (GPXSource, PeteTree, indexFromDistance, skipCount, trueLength)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FlatColors.ChinesePalette
import GpxParser
import Length
import List.Extra
import Quantity
import String.Interpolate
import Task
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.OneClickQuickFix as OneClickQuickFix
import Tools.SplitAndJoinOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure, withLeadingZeros)
import ViewPureStyles exposing (..)
import WriteGPX


toolId =
    "split/join"


type Msg
    = SplitTrack
    | SetSplitLimit Length.Length
    | WriteSection (List ( Int, Float, Float ))
    | ToggleBuffers Bool
    | AppendFile
    | FileSelected File
    | FileLoaded String
    | ToggleAutofix Bool
    | DisplayInfo String String


defaultOptions : Options
defaultOptions =
    { splitLimit = Length.kilometers 100.0
    , addBuffers = False
    , applyAutofix = False
    }


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> ( Options, List (Actions.ToolAction msg) )
update msg settings mTrack wrap =
    case msg of
        SetSplitLimit n ->
            ( { settings | splitLimit = n }, [] )

        ToggleBuffers _ ->
            ( { settings | addBuffers = not settings.addBuffers }, [] )

        ToggleAutofix _ ->
            ( { settings | applyAutofix = not settings.applyAutofix }, [] )

        AppendFile ->
            ( settings, [ Actions.SelectGpxFile (wrap << FileSelected) ] )

        FileSelected file ->
            ( settings, [ Actions.LoadGpxFile (wrap << FileLoaded) file ] )

        --, ActionCommand <| Task.perform (msgWrapper << FileLoaded) (File.toString file)
        FileLoaded content ->
            ( settings, [ Actions.ParseAndAppend content, Actions.TrackHasChanged ] )

        SplitTrack ->
            let
                trackSplits =
                    calculateSections (trueLength mTrack.trackTree) settings
            in
            ( settings
            , [ Actions.WriteTrackSections trackSplits
              , Actions.DelayMessage 2000 <| wrap <| WriteSection <| List.drop 1 trackSplits
              ]
            )

        WriteSection moreSections ->
            case moreSections of
                section1 :: evenMoreSections ->
                    ( settings
                    , [ Actions.WriteTrackSections moreSections
                      , Actions.DelayMessage 2000 <| wrap <| WriteSection <| evenMoreSections
                      ]
                    )

                [] ->
                    ( settings, [] )

        DisplayInfo tool tag ->
            ( settings, [ Actions.DisplayInfo tool tag ] )


writeOneSection : List ( Int, Float, Float ) -> Options -> TrackLoaded msg -> Cmd msg
writeOneSection sections options track =
    case sections of
        ( index, start, end ) :: rest ->
            let
                ( metricStart, metricEnd ) =
                    if options.addBuffers then
                        ( Length.meters (start - 60.0)
                        , Length.meters (end + 140.0)
                        )

                    else
                        ( Length.meters start
                        , Length.meters end
                        )

                trackName =
                    track.trackName |> Maybe.withDefault "track"

                filename =
                    trackName
                        ++ "_"
                        ++ withLeadingZeros 2 (String.fromInt index)
                        ++ ".gpx"

                trackExtract =
                    -- This is a mini-track
                    let
                        ( startIndex, endIndex ) =
                            ( indexFromDistance metricStart track.trackTree
                            , indexFromDistance metricEnd track.trackTree
                            )
                    in
                    TrackLoaded.trackFromPoints trackName <|
                        List.map Tuple.second <|
                            DomainModel.extractPointsInRange
                                startIndex
                                (skipCount track.trackTree - endIndex)
                                track.trackTree

                processingFunction =
                    if options.applyAutofix then
                        \atrack ->
                            let
                                ( fixedTree, _ ) =
                                    OneClickQuickFix.apply atrack
                            in
                            { atrack
                                | trackTree =
                                    -- If fix fails, use original tree.
                                    Maybe.withDefault atrack.trackTree fixedTree
                            }

                    else
                        identity

                content =
                    case trackExtract of
                        Just subTrack ->
                            WriteGPX.writeGPX track.trackName (processingFunction subTrack) []

                        Nothing ->
                            "failed to make the track section"
            in
            Download.string filename "text/gpx" content

        _ ->
            Cmd.none


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, [] )

        _ ->
            ( options, [] )


calculateSections : Length.Length -> Options -> List ( Int, Float, Float )
calculateSections length options =
    -- This function works out where the splits are, then each section is
    -- written out using the runtime, which kicks off the next.
    let
        effectiveLength =
            if options.addBuffers then
                options.splitLimit |> Quantity.minus (Length.meters 200.0)

            else
                options.splitLimit

        splitCount =
            ceiling <| Quantity.ratio length effectiveLength

        splitLength =
            length |> Quantity.divideBy (toFloat splitCount)

        splitPoints =
            List.map
                (\n ->
                    splitLength
                        |> Quantity.multiplyBy (toFloat n)
                        |> Length.inMeters
                )
                (List.range 0 splitCount)
    in
    List.map3
        (\a b c -> ( a, b, c ))
        (List.range 1 splitCount)
        splitPoints
        (List.drop 1 splitPoints)


view : I18NOptions.Location -> Bool -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view location imperial options wrapper track =
    let
        i18n =
            I18N.text location toolId

        effectiveLength =
            if options.addBuffers then
                options.splitLimit |> Quantity.minus (Length.meters 200.0)

            else
                options.splitLimit

        splitCount =
            ceiling <| Quantity.ratio (trueLength track.trackTree) effectiveLength

        splitLength =
            trueLength track.trackTree |> Quantity.divideBy (toFloat splitCount)

        partsSlider =
            if imperial then
                Input.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetSplitLimit << Length.miles
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "max")
                                    [ showLongMeasure True options.splitLimit ]
                    , min = 12.0
                    , max = 65.0
                    , step = Just 1.0
                    , value = Length.inMiles options.splitLimit
                    , thumb = Input.defaultThumb
                    }

            else
                Input.slider
                    commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetSplitLimit << Length.kilometers
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "max")
                                    [ showLongMeasure True options.splitLimit ]
                    , min = 20.0
                    , max = 100.0
                    , step = Just 1.0
                    , value = Length.inKilometers options.splitLimit
                    , thumb = Input.defaultThumb
                    }

        endPenCheckbox =
            Input.checkbox []
                { onChange = wrapper << ToggleBuffers
                , icon = Input.defaultCheckbox
                , checked = options.addBuffers
                , label = Input.labelRight [ centerY ] (i18n "pens")
                }

        quickFixCheckbox =
            Input.checkbox []
                { onChange = wrapper << ToggleAutofix
                , icon = Input.defaultCheckbox
                , checked = options.applyAutofix
                , label = Input.labelRight [ centerY ] (i18n "1CQF")
                }

        splitButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| SplitTrack
                , label =
                    text <|
                        String.Interpolate.interpolate
                            (I18N.localisedString location toolId "split")
                            [ String.fromInt splitCount
                            , showLongMeasure imperial splitLength
                            ]
                }

        appendFileButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| AppendFile
                , label = i18n "append"
                }
    in
    column
        [ spacing 6
        , padding 6
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ el [ centerX ] partsSlider
        , endPenCheckbox
        , quickFixCheckbox
        , el [ centerX ] splitButton
        , el [ centerX ] <|
            paragraph []
                [ i18n "note" ]
        , el [ centerX ] appendFileButton
        ]


parseAndAppend : String -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
parseAndAppend content track =
    let
        track2 =
            --TODO: Segments support
            GpxParser.parseGPX content
                |> List.concatMap Tuple.second

        currentGpx =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        newTree =
            (currentGpx ++ track2) |> DomainModel.treeFromSourcePoints
    in
    ( newTree, currentGpx )
