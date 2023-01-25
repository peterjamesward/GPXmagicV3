module SceneBuilderProfile exposing
    ( gradientChart
    , profileChart
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
import DomainModel exposing (GPXSource, RoadSection)
import Json.Encode as E
import Length exposing (Meters)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (colourHexString)
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


commonChartScales : ProfileContext -> Bool -> TrackLoaded msg -> Bool -> CommonChartInfo
commonChartScales profile imperial track isGradients =
    let
        ( distanceFunction, altitudeFunction ) =
            if imperial then
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
            if imperial then
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


profileChart : ProfileContext -> Bool -> TrackLoaded msg -> E.Value
profileChart profile imperial track =
    -- Use JSON as per chart.js demands.
    -- Indeed, declare the entire chart here, not in JS.
    let
        commonInfo =
            commonChartScales profile imperial track False

        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets"
                          , E.list identity
                                [ profileDataset
                                , purpleDataset
                                , orangeDataset
                                ]
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
                , ( "data", E.list identity coordinates )
                , ( "fill", E.string "stack" )
                , ( "label", E.string "altitude" )
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
                        [ ( "data", E.list identity [] ) ]

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


gradientChart : ProfileContext -> Bool -> TrackLoaded msg -> E.Value
gradientChart profile imperial track =
    -- Use JSON as per chart.js demands.
    -- Indeed, declare the entire chart here, not in JS.
    let
        commonInfo =
            commonChartScales profile imperial track True

        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets"
                          , E.list identity
                                [ gradientDataset

                                --, purpleDataset
                                --, orangeDataset
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
                , ( "step", E.bool True )
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
                        [ ( "data", E.list identity [] ) ]

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
                newDistance
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
            in
            makeGradientPoint asGPX asDist 0

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
                , ( "colour", E.string <| colourHexString <| gradientColourPastel gradient )
                ]
    in
    chartStuff
