module SceneBuilderProfile exposing
    ( gradientChart
    , profileChart
    , profileChartWithColours
    )

{- EXAMPLE AS BASIS
   d0 = [{x:1, y:1}, {x:2, y:11},{x:3, y:1},{x:4, y:11},{x:5, y:10}, {x:6, y:8},{x:7, y:15},{x:8, y:2}]

   d1= [{x:1, y:1}, {x:2, y:5}, {x:4, y:1}, {x:5, y:3}, {x:5.5, y:4},{x:7, y:5},{x:8, y:3}]

   const ctx = document.getElementById("chart").getContext("2d");
   const chart = new Chart(ctx, {
     type: "line",
     data: {
       datasets: [
         {
           backgroundColor: "rgba(50,200,50,0.6)",
           borderColor: "rgba(50,200,50,0.6)",
           data: d0,
           fill: "stack"
         },
         {
           backgroundColor: "rgba(200,50,50,0.6)",
           borderColor: "rgba(200,50,50,0.6)",
           data: d1,
           fill: "stack"
         }
       ]
     },
     options: {
       scales: {
         x: {
           type: "linear"
         },
         y: {
           stacked: true,
         }
       },
       elements: {
         line: {
            tension: 0.2
         },
         point: {
           radius: 0
         }
       },
       tooltips: {
         mode: "nearest",
         intersect: false,
         axis: "x"
       },
       hover: {
         mode: "nearest",
         intersect: false,
         axis: "x"
       }
     }
   });
-}

import ColourPalette exposing (gradientColourPastel)
import CommonToolStyles
import Dict exposing (Dict)
import DomainModel exposing (GPXSource, RoadSection)
import FlatColors.FlatUIPalette
import Json.Encode as E
import Length exposing (Meters)
import PreviewData exposing (PreviewData, PreviewShape(..))
import Quantity exposing (Quantity)
import Quantity.Interval as Interval
import SystemSettings exposing (SystemSettings)
import Tools.NamedSegmentOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (colourHexString, uiColourHexString)
import ViewProfileChartContext exposing (ProfileContext)


type alias CommonChartInfo =
    { options : E.Value
    , distanceFunction : Length.Length -> Float
    , altitudeFunction : Length.Length -> Float
    , firstPointIndex : Int
    , lastPointIndex : Int
    , firstPointDistance : Length.Length
    , lastPointDistance : Length.Length
    , focalPoint : Length.Length
    , startDistance : Length.Length
    , endDistance : Length.Length
    }


commonChartScales : SystemSettings -> ProfileContext -> TrackLoaded msg -> Bool -> CommonChartInfo
commonChartScales settings profile track isGradients =
    let
        ( distanceFunction, altitudeFunction ) =
            if settings.imperial then
                ( Length.inMiles, Length.inFeet )

            else
                ( Length.inKilometers, Length.inMeters )

        ( leftmostCentreDistance, rightmostCentreDistance ) =
            ( halfOfView
            , DomainModel.trueLength track.trackTree |> Quantity.minus halfOfView
            )

        orangeDistance =
            DomainModel.distanceFromIndex track.currentPosition track.trackTree

        focalPoint =
            if profile.followSelectedPoint then
                orangeDistance
                    |> Quantity.clamp
                        leftmostCentreDistance
                        rightmostCentreDistance

            else
                profile.focalPoint
                    |> Quantity.clamp
                        leftmostCentreDistance
                        rightmostCentreDistance

        halfOfView =
            -- Zoom level zero shows whole track.
            DomainModel.trueLength track.trackTree
                |> Quantity.multiplyBy (0.5 ^ profile.zoomLevel)
                |> Quantity.half

        --_ = Debug.log "PROFILE" profile
        ( startDistance, endDistance ) =
            ( focalPoint |> Quantity.minus halfOfView
            , focalPoint |> Quantity.plus halfOfView
            )

        ( firstPointIndex, lastPointIndex ) =
            ( DomainModel.indexFromDistanceRoundedDown startDistance track.trackTree
            , DomainModel.indexFromDistanceRoundedUp endDistance track.trackTree
            )

        ( firstPointDistance, lastPointDistance ) =
            ( DomainModel.distanceFromIndex firstPointIndex track.trackTree
            , DomainModel.distanceFromIndex lastPointIndex track.trackTree
            )

        ( distanceUnits, altitudeUnits ) =
            if settings.imperial then
                ( "Miles"
                , if isGradients then
                    "%"

                  else
                    "Feet"
                )

            else
                ( "Kilometers"
                , if isGradients then
                    "%"

                  else
                    "Meters"
                )

        options =
            E.object
                [ ( "animations", E.bool False )
                , ( "responsive", E.bool True )
                , ( "plugins"
                  , E.object
                        [ ( "legend"
                          , E.object [ ( "display", E.bool False ) ]
                          )
                        ]
                  )
                , ( "element"
                  , E.object
                        [ ( "point"
                          , E.object
                                [ ( "pointStyle", E.bool False ) ]
                          )
                        ]
                  )
                , ( "scales"
                  , E.object
                        [ ( "x"
                          , E.object
                                [ ( "type", E.string "linear" )
                                , ( "min", E.float <| distanceFunction startDistance )
                                , ( "max", E.float <| distanceFunction endDistance )
                                , ( "title"
                                  , E.object
                                        [ ( "text", E.string distanceUnits )
                                        , ( "display", E.bool True )
                                        ]
                                  )
                                , ( "grid"
                                  , E.object
                                        [ ( "color"
                                          , E.string <|
                                                uiColourHexString <|
                                                    FlatColors.FlatUIPalette.concrete
                                          )
                                        ]
                                  )
                                , ( "color"
                                  , E.string <|
                                        uiColourHexString <|
                                            FlatColors.FlatUIPalette.concrete
                                  )
                                ]
                          )
                        , ( "y"
                          , E.object
                                [ ( "type", E.string "linear" )
                                , ( "title"
                                  , E.object
                                        [ ( "text", E.string altitudeUnits )
                                        , ( "display", E.bool True )
                                        ]
                                  )
                                , ( "grid"
                                  , E.object
                                        [ ( "color"
                                          , E.string <|
                                                uiColourHexString <|
                                                    FlatColors.FlatUIPalette.concrete
                                          )
                                        ]
                                  )
                                , ( "color"
                                  , E.string <|
                                        uiColourHexString <|
                                            FlatColors.FlatUIPalette.concrete
                                  )
                                ]
                          )
                        ]
                  )
                ]
    in
    { options = options
    , distanceFunction = distanceFunction
    , altitudeFunction = altitudeFunction
    , firstPointIndex = firstPointIndex
    , lastPointIndex = lastPointIndex
    , firstPointDistance = firstPointDistance
    , lastPointDistance = lastPointDistance
    , focalPoint = focalPoint
    , startDistance = startDistance
    , endDistance = endDistance
    }


profileChart :
    ProfileContext
    -> SystemSettings
    -> TrackLoaded msg
    -> List Tools.NamedSegmentOptions.NamedSegment
    -> Dict String PreviewData
    -> E.Value
profileChart profile settings track segments previews =
    if profile.colouredChart then
        profileChartWithColours profile settings track

    else
        profileChartMonochrome profile settings track segments previews


profileChartMonochrome :
    ProfileContext
    -> SystemSettings
    -> TrackLoaded msg
    -> List Tools.NamedSegmentOptions.NamedSegment
    -> Dict String PreviewData
    -> E.Value
profileChartMonochrome profile settings track segments previews =
    -- Use JSON as per chart.js demands.
    -- Indeed, declare the entire chart here, not in JS.
    let
        commonInfo =
            commonChartScales settings profile track False

        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets"
                          , E.list identity <|
                                profileDataset
                                    :: previewDatasets
                                    ++ segmentDatasets
                                    ++ [ purpleDataset, orangeDataset ]
                          )
                        ]
                  )
                , ( "options", commonInfo.options )
                ]

        profileDataset =
            E.object
                [ ( "backgroundColor", E.string "rgba(182,198,237,0.6)" )
                , ( "borderColor", E.string "rgba(77,110,205,0.6" )
                , ( "pointStyle", E.bool False )
                , ( "data", E.list identity <| chartCoordinates track.trackTree )
                , ( "fill", E.string "stack" )
                , ( "label", E.string "altitude" )
                ]

        segmentDatasets =
            List.filterMap segmentDataset segments

        segmentDataset : Tools.NamedSegmentOptions.NamedSegment -> Maybe E.Value
        segmentDataset segment =
            let
                segmentCoordinates : List E.Value
                segmentCoordinates =
                    let
                        segmentStartIndex =
                            DomainModel.indexFromDistance
                                segment.startDistance
                                track.trackTree

                        segmentEndIndex =
                            DomainModel.indexFromDistance
                                segment.endDistance
                                track.trackTree

                        segmentStartPoint =
                            DomainModel.gpxPointFromIndex segmentStartIndex track.trackTree

                        ( _, points ) =
                            DomainModel.traverseTreeBetweenLimitsToDepth
                                segmentStartIndex
                                segmentEndIndex
                                (always Nothing)
                                0
                                track.trackTree
                                coordinateCollector
                                ( segment.startDistance
                                , [ makeProfilePoint segmentStartPoint segment.startDistance ]
                                )
                    in
                    List.reverse points

                segmentInterval =
                    Interval.from segment.startDistance segment.endDistance

                windowInterval =
                    Interval.from commonInfo.startDistance commonInfo.endDistance
            in
            if Interval.intersects segmentInterval windowInterval then
                Just <|
                    E.object
                        [ ( "backgroundColor", E.string "#ffc0cb" )
                        , ( "borderColor", E.string "#ffc0cb" )
                        , ( "pointStyle", E.bool False )
                        , ( "data", E.list identity segmentCoordinates )
                        , ( "fill", E.bool True )
                        , ( "label", E.string segment.name )
                        ]

            else
                Nothing

        previewDatasets =
            previews
                |> Dict.map previewDataset
                |> Dict.values
                |> List.filterMap identity

        previewDataset : String -> PreviewData -> Maybe E.Value
        previewDataset tag preview =
            case preview.shape of
                PreviewProfile tree ->
                    let
                        colour =
                            uiColourHexString preview.colour
                    in
                    Just <|
                        E.object
                            [ ( "backgroundColor", E.string colour )
                            , ( "borderColor", E.string colour )
                            , ( "pointStyle", E.bool False )
                            , ( "data", E.list identity <| chartCoordinates tree )
                            , ( "fill", E.bool False )
                            , ( "label", E.string tag )
                            ]

                _ ->
                    Nothing

        orangeDataset =
            E.object
                [ ( "backgroundColor", E.string "orange" )
                , ( "borderColor", E.string "rgba(255,0,0,1.0" )
                , ( "pointStyle", E.string "circle" )
                , ( "pointRadius", E.float 10 )
                , ( "data", E.list identity orangePoint )
                , ( "label", E.string "orange" )
                ]

        purpleDataset =
            case track.markerPosition of
                Just purple ->
                    E.object
                        [ ( "backgroundColor", E.string "purple" )
                        , ( "borderColor", E.string "rgba(255,0,0,1.0" )
                        , ( "pointStyle", E.string "circle" )
                        , ( "pointRadius", E.float 10 )
                        , ( "data", E.list identity <| [ profilePointFromIndex purple ] )
                        , ( "label", E.string "purple" )
                        ]

                Nothing ->
                    E.object
                        [ ( "data", E.null ) ]

        coordinateCollector :
            RoadSection
            -> ( Quantity Float Meters, List E.Value )
            -> ( Quantity Float Meters, List E.Value )
        coordinateCollector road ( lastDistance, outputs ) =
            let
                newDistance =
                    lastDistance |> Quantity.plus road.trueLength
            in
            ( newDistance
            , makeProfilePoint (Tuple.second road.sourceData) newDistance
                :: outputs
            )

        firstPoint =
            DomainModel.gpxPointFromIndex commonInfo.firstPointIndex track.trackTree

        profilePointFromIndex : Int -> E.Value
        profilePointFromIndex index =
            let
                asGPX =
                    DomainModel.gpxPointFromIndex index track.trackTree

                asDist =
                    DomainModel.distanceFromIndex index track.trackTree
            in
            makeProfilePoint asGPX asDist

        orangePoint : List E.Value
        orangePoint =
            [ profilePointFromIndex track.currentPosition ]

        chartCoordinates : DomainModel.PeteTree -> List E.Value
        chartCoordinates tree =
            let
                ( _, points ) =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        commonInfo.firstPointIndex
                        commonInfo.lastPointIndex
                        (always <| Just <| floor <| profile.zoomLevel + 8)
                        0
                        tree
                        coordinateCollector
                        ( commonInfo.firstPointDistance
                        , [ makeProfilePoint firstPoint commonInfo.firstPointDistance ]
                        )
            in
            List.reverse points

        makeProfilePoint : GPXSource -> Quantity Float Meters -> E.Value
        makeProfilePoint gpx distance =
            let
                altitude =
                    commonInfo.altitudeFunction gpx.altitude

                fDistance =
                    commonInfo.distanceFunction distance
            in
            E.object
                [ ( "x", E.float fDistance )
                , ( "y", E.float altitude )
                ]
    in
    chartStuff


type alias GradientBucketEntry =
    { bucketEndsAt : Length.Length
    , chartEntries : List E.Value
    }


profileChartWithColours : ProfileContext -> SystemSettings -> TrackLoaded msg -> E.Value
profileChartWithColours profile settings track =
    -- We cannot change the fill area dynamically under a line chart, so we create
    -- multiple charts with common axes, representing different gradient classes
    -- (steep up, up, flat, down, steep down, say).
    -- Question is whether clearer to use one fold or one per class.
    -- The subtlety is that we need start and end points for each region,
    -- so it's somewhat fussy.
    let
        bucketNumberFromGradient : Float -> Int
        bucketNumberFromGradient gradient =
            -- If this works with 40 buckets, we're good. (It didn't).
            gradient / 5 |> round |> clamp -4 4

        gradientFromBucketNumber : Int -> Float
        gradientFromBucketNumber bucket =
            bucket * 4 |> toFloat

        commonInfo =
            commonChartScales settings profile track False

        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets"
                          , E.list identity <|
                                roadSectionDatasets
                                    ++ [ purpleDataset
                                       , orangeDataset
                                       ]
                          )
                        ]
                  )
                , ( "options", commonInfo.options )
                ]

        roadSectionDatasets : List E.Value
        roadSectionDatasets =
            roadSectionCollections
                |> Dict.map datasetForGradientBucket
                |> Dict.values

        roadSectionCollections : Dict Int GradientBucketEntry
        roadSectionCollections =
            -- Partition road into buckets by gradient.
            let
                ( _, buckets ) =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        commonInfo.firstPointIndex
                        commonInfo.lastPointIndex
                        (always <| Just <| floor <| profile.zoomLevel + 8)
                        0
                        track.trackTree
                        roadSectionCollector
                        ( commonInfo.firstPointDistance, Dict.empty )
            in
            buckets

        roadSectionCollector :
            RoadSection
            -> ( Length.Length, Dict Int GradientBucketEntry )
            -> ( Length.Length, Dict Int GradientBucketEntry )
        roadSectionCollector road ( distanceAtStart, buckets ) =
            {-
               A few possibilities here:
               1. The start of the section is left of, at, or right of the left edge.
                   - This (partly) determines whether we may need a preceding null.
               2. The bucket is empty or not.
                   - If empty, we need a preceding null unless the start is at or left of the left edge.
               3. If not, the last bucket entry is contiguous or not.
                   - If contiguous, just add the road end,
                   - otherwise:
                       - a null between previous and new segments, road start & road end.
               4. The end of the section is left of, at, or right of the right edge.
                   - No, this doesn't matter.
            -}
            let
                distanceAtEnd =
                    distanceAtStart |> Quantity.plus road.trueLength

                ( startWithinView, endWithinView ) =
                    ( distanceAtStart |> Quantity.greaterThanOrEqualTo commonInfo.startDistance
                    , distanceAtEnd |> Quantity.lessThanOrEqualTo commonInfo.endDistance
                    )

                ( startOfThisSegment, endOfThisSegment ) =
                    ( makeProfilePoint (Tuple.first road.sourceData) distanceAtStart
                    , makeProfilePoint (Tuple.second road.sourceData) distanceAtEnd
                    )

                bucketKey =
                    bucketNumberFromGradient road.gradientAtStart

                newPoints =
                    case Dict.get bucketKey buckets of
                        Just bucket ->
                            -- Non-empty by presence. Check for contiguity.
                            if
                                Quantity.equalWithin Length.centimeter
                                    bucket.bucketEndsAt
                                    distanceAtStart
                            then
                                -- Contiguous, just add the end
                                endOfThisSegment :: bucket.chartEntries

                            else
                                -- Non-contiguous, insert null and start also
                                let
                                    interveningNull =
                                        makeNullPoint <|
                                            Quantity.half <|
                                                Quantity.plus
                                                    bucket.bucketEndsAt
                                                    distanceAtStart
                                in
                                endOfThisSegment
                                    :: startOfThisSegment
                                    :: interveningNull
                                    :: bucket.chartEntries

                        Nothing ->
                            -- First entry in this bucket
                            [ endOfThisSegment, startOfThisSegment ]

                newBucket : GradientBucketEntry
                newBucket =
                    { bucketEndsAt = distanceAtEnd
                    , chartEntries = newPoints
                    }
            in
            ( distanceAtEnd
            , Dict.insert bucketKey newBucket buckets
            )

        datasetForGradientBucket : Int -> GradientBucketEntry -> E.Value
        datasetForGradientBucket bucketKey { bucketEndsAt, chartEntries } =
            -- Each road section bucket makes its own dataset.
            -- Will try putting null values for start and end into each collection
            let
                dummyStart =
                    makeNullPoint commonInfo.startDistance

                dummyEnd =
                    makeNullPoint commonInfo.endDistance

                paddedEntries =
                    dummyStart :: chartEntries ++ [ dummyEnd ]
            in
            E.object
                [ ( "backgroundColor"
                  , E.string <|
                        colourHexString <|
                            gradientColourPastel <|
                                gradientFromBucketNumber bucketKey
                  )
                , ( "borderColor", E.string "rgba(77,110,205,0.6" )
                , ( "pointStyle", E.bool False )
                , ( "data", E.list identity <| List.reverse chartEntries )
                , ( "fill", E.string "stack" )
                , ( "spanGaps", E.bool False )
                ]

        orangeDataset =
            E.object
                [ ( "backgroundColor", E.string "orange" )
                , ( "borderColor", E.string "rgba(255,0,0,1.0" )
                , ( "pointStyle", E.string "circle" )
                , ( "pointRadius", E.float 10 )
                , ( "data", E.list identity orangePoint )
                , ( "label", E.string "orange" )
                ]

        purpleDataset =
            case track.markerPosition of
                Just purple ->
                    E.object
                        [ ( "backgroundColor", E.string "purple" )
                        , ( "borderColor", E.string "rgba(255,0,0,1.0" )
                        , ( "pointStyle", E.string "circle" )
                        , ( "pointRadius", E.float 10 )
                        , ( "data", E.list identity <| [ profilePointFromIndex purple ] )
                        , ( "label", E.string "purple" )
                        ]

                Nothing ->
                    E.object
                        [ ( "data", E.null ) ]

        firstPoint =
            DomainModel.gpxPointFromIndex commonInfo.firstPointIndex track.trackTree

        profilePointFromIndex : Int -> E.Value
        profilePointFromIndex index =
            let
                asGPX =
                    DomainModel.gpxPointFromIndex index track.trackTree

                asDist =
                    DomainModel.distanceFromIndex index track.trackTree
            in
            makeProfilePoint asGPX asDist

        orangePoint : List E.Value
        orangePoint =
            [ profilePointFromIndex track.currentPosition ]

        makeProfilePoint : GPXSource -> Quantity Float Meters -> E.Value
        makeProfilePoint gpx distance =
            let
                altitude =
                    commonInfo.altitudeFunction gpx.altitude

                fDistance =
                    commonInfo.distanceFunction distance
            in
            E.object
                [ ( "x", E.float fDistance )
                , ( "y", E.float altitude )
                ]

        makeNullPoint : Quantity Float Meters -> E.Value
        makeNullPoint distance =
            let
                fDistance =
                    commonInfo.distanceFunction distance
            in
            E.object
                [ ( "x", E.float fDistance )
                , ( "y", E.null )
                ]
    in
    chartStuff


gradientChart : ProfileContext -> SystemSettings -> TrackLoaded msg -> E.Value
gradientChart profile settings track =
    -- Use JSON as per chart.js demands.
    -- Indeed, declare the entire chart here, not in JS.
    let
        commonInfo =
            commonChartScales settings profile track True

        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets"
                          , E.list identity
                                [ gradientDataset
                                , purpleDataset
                                , orangeDataset
                                ]
                          )
                        ]
                  )
                , ( "options", commonInfo.options )
                ]

        gradientDataset =
            E.object
                [ ( "backgroundColor", E.string "rgba(182,198,237,0.6)" )
                , ( "borderColor", E.string "rgba(77,110,205,0.6" )
                , ( "pointStyle", E.bool False )
                , ( "data", E.list identity coordinates )
                , ( "fill", E.bool True )
                , ( "label", E.string "gradient" )
                , ( "stepped", E.bool True )
                ]

        orangeDataset =
            E.object
                [ ( "backgroundColor", E.string "orange" )
                , ( "borderColor", E.string "rgba(255,0,0,1.0" )
                , ( "pointStyle", E.string "circle" )
                , ( "pointRadius", E.float 10 )
                , ( "data", E.list identity orangePoint )
                , ( "label", E.string "orange" )
                ]

        purpleDataset =
            case track.markerPosition of
                Just purple ->
                    E.object
                        [ ( "backgroundColor", E.string "purple" )
                        , ( "borderColor", E.string "rgba(255,0,0,1.0" )
                        , ( "pointStyle", E.string "circle" )
                        , ( "pointRadius", E.float 10 )
                        , ( "data", E.list identity <| [ gradientPointFromIndex purple ] )
                        , ( "label", E.string "purple" )
                        ]

                Nothing ->
                    E.object
                        [ ( "data", E.null ) ]

        orangeDistance =
            DomainModel.distanceFromIndex track.currentPosition track.trackTree

        coordinateCollector :
            RoadSection
            -> ( Quantity Float Meters, List E.Value )
            -> ( Quantity Float Meters, List E.Value )
        coordinateCollector road ( lastDistance, outputs ) =
            let
                newDistance =
                    lastDistance |> Quantity.plus road.trueLength
            in
            ( newDistance
            , makeGradientPoint
                (Tuple.second road.sourceData)
                lastDistance
                road.gradientAtStart
                :: outputs
            )

        gradientPointFromIndex : Int -> E.Value
        gradientPointFromIndex index =
            let
                asGPX =
                    DomainModel.gpxPointFromIndex index track.trackTree

                asDist =
                    DomainModel.distanceFromIndex index track.trackTree

                gradient =
                    DomainModel.leafFromIndex index track.trackTree
                        |> DomainModel.asRecord
                        |> .gradientAtStart
            in
            makeGradientPoint asGPX asDist gradient

        orangePoint : List E.Value
        orangePoint =
            [ gradientPointFromIndex track.currentPosition ]

        coordinates : List E.Value
        coordinates =
            let
                ( _, points ) =
                    DomainModel.traverseTreeBetweenLimitsToDepth
                        commonInfo.firstPointIndex
                        commonInfo.lastPointIndex
                        (always <| Just <| floor <| profile.zoomLevel + 8)
                        0
                        track.trackTree
                        coordinateCollector
                        ( commonInfo.firstPointDistance
                        , []
                        )
            in
            List.reverse points

        makeGradientPoint : GPXSource -> Quantity Float Meters -> Float -> E.Value
        makeGradientPoint gpx distance gradient =
            let
                altitude =
                    commonInfo.altitudeFunction gpx.altitude

                fDistance =
                    commonInfo.distanceFunction distance
            in
            E.object
                [ ( "x", E.float fDistance )
                , ( "y", E.float gradient )
                ]
    in
    chartStuff
