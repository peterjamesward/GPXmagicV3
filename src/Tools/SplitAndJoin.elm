module Tools.SplitAndJoin exposing (..)

import Actions exposing (ToolAction)
import Delay
import DomainModel exposing (indexFromDistance, skipCount, trueLength)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import File exposing (File)
import File.Download
import File.Select as Select
import FlatColors.ChinesePalette
import GpxParser
import Length
import List.Extra
import Quantity
import Task
import Tools.OneClickQuickFix as OneClickQuickFix
import Tools.SplitAndJoinOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showLongMeasure, withLeadingZeros)
import ViewPureStyles exposing (..)
import WriteGPX


type Msg
    = SplitTrack
    | SetSplitLimit Length.Length
    | WriteSection (List ( Int, Float, Float ))
    | ToggleBuffers Bool
    | AppendFile
    | FileSelected File
    | FileLoaded String
    | ToggleAutofix Bool


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
    -> ( Options, List (Actions.ToolAction msg) )
update msg settings mTrack =
    case msg of
        SetSplitLimit n ->
            ( { settings
                | splitLimit = n
              }
            , []
            )

        ToggleBuffers state ->
            ( { settings
                | addBuffers = not settings.addBuffers
              }
            , []
            )

        ToggleAutofix _ ->
            ( { settings
                | applyAutofix = not settings.applyAutofix
              }
            , []
            )

        AppendFile ->
            ( settings, [] )

        --, ActionCommand <| Cmd.map msgWrapper <| Select.file [ "text/gpx" ] FileSelected
        FileSelected file ->
            ( settings, [] )

        --, ActionCommand <| Task.perform (msgWrapper << FileLoaded) (File.toString file)
        FileLoaded content ->
            -- You'd think we could just concatenate the track point lists.
            -- Tried that, but they have different bounding boxes and "Ghanians".
            -- Life might be easier just to spin up a new GPX string and start over!
            -- Let's see if we can fix this without that resort. We did, not elegantly.
            let
                track2 =
                    content
                        |> GpxParser.parseGPXPoints
                        |> TrackLoaded.trackFromPoints
                            (GpxParser.parseTrackName content |> Maybe.withDefault "track")
            in
            case track2 of
                Just isNewTrack ->
                    ( settings
                    , []
                      -- Append new track
                    )

                Nothing ->
                    ( settings, [] )

        SplitTrack ->
            ( settings, [] )

        --, ActionCommand <|
        --    Delay.after 100 <|
        --        msgWrapper <|
        --            WriteSection <|
        --                writeSections
        --                    mTrack
        --                    (trueLength mTrack.trackTree)
        --                    settings
        --)
        WriteSection sections ->
            case sections of
                ( index, start, end ) :: rest ->
                    let
                        ( metricStart, metricEnd ) =
                            if settings.addBuffers then
                                ( Length.meters (start - 60.0)
                                , Length.meters (end + 140.0)
                                )

                            else
                                ( Length.meters start
                                , Length.meters end
                                )

                        trackName =
                            mTrack.trackName |> Maybe.withDefault "track"

                        filename =
                            trackName
                                ++ "_"
                                ++ withLeadingZeros 2 (String.fromInt index)
                                ++ ".gpx"

                        trackExtract =
                            -- This is a mini-track
                            let
                                ( startIndex, endIndex ) =
                                    ( indexFromDistance metricStart mTrack.trackTree
                                    , indexFromDistance metricEnd mTrack.trackTree
                                    )
                            in
                            TrackLoaded.trackFromPoints trackName <|
                                List.map Tuple.second <|
                                    DomainModel.extractPointsInRange startIndex endIndex mTrack.trackTree

                        processingFunction =
                            if settings.applyAutofix then
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
                                    WriteGPX.writeGPX mTrack.trackName <| processingFunction subTrack

                                Nothing ->
                                    ""
                    in
                    ( settings, [] )

                --, ActionCommand <|
                --    Cmd.batch
                --        [ File.Download.string filename "text/xml" content
                --        , Delay.after 2000 <| msgWrapper <| WriteSection rest
                --        ]
                --)
                _ ->
                    ( settings, [] )


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


writeSections : TrackLoaded msg -> Length.Length -> Options -> List ( Int, Float, Float )
writeSections track length options =
    -- Doesn't *actually* split the track, just writes out the files.
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


view : Bool -> Options -> (Msg -> msg) -> TrackLoaded msg -> Element msg
view imperial options wrapper track =
    let
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
                    , label = Input.labelBelow [] <| text <| "Max: " ++ showLongMeasure True options.splitLimit
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
                    , label = Input.labelBelow [] <| text <| "Max : " ++ showLongMeasure imperial options.splitLimit
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
                , label = Input.labelRight [ centerY ] (text "Allow for start and end pens")
                }

        quickFixCheckbox =
            Input.checkbox []
                { onChange = wrapper << ToggleAutofix
                , icon = Input.defaultCheckbox
                , checked = options.applyAutofix
                , label = Input.labelRight [ centerY ] (text "Apply one-click-quick-fix to each section")
                }

        splitButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| SplitTrack
                , label =
                    text <|
                        "Split into "
                            ++ String.fromInt splitCount
                            ++ " files\n"
                            ++ "each "
                            ++ showLongMeasure imperial splitLength
                            ++ " long"
                }

        appendFileButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper <| AppendFile
                , label = text "Append file ..."
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
                [ text "Files will be written to Downloads folder at two second intervals." ]
        , el [ centerX ] appendFileButton
        ]
