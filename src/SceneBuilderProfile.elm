module SceneBuilderProfile exposing (profileChart)

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

import DomainModel
import Json.Encode as E
import Length
import TrackLoaded exposing (TrackLoaded)


profileChart : Bool -> TrackLoaded msg -> E.Value
profileChart imperial track =
    -- Use JSON as per chart.js demands.
    -- Indeed, declare the entire chart here, not in JS.
    let
        ( distanceFunction, altitudeFunction ) =
            if imperial then
                ( Length.inMiles, Length.inFeet )

            else
                ( Length.inKilometers, Length.inMeters )

        ( distanceUnits, altitudeUnits ) =
            if imperial then
                ( "Miles", "Feet" )

            else
                ( "Kilometers", "Meters" )

        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets", E.list identity [ profileDataset ] )
                        ]
                  )
                , ( "options", options )
                ]

        trackLength =
            DomainModel.trueLength track.trackTree
                |> distanceFunction
                |> ceiling

        options =
            E.object
                [ ( "animations", E.bool False )
                , ( "plugins"
                  , E.object
                        [ ( "legend"
                          , E.object
                                [ ( "display", E.bool False )
                                ]
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
                                , ( "max", E.int trackLength )
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

        profileDataset =
            E.object
                [ ( "backgroundColor", E.string "rgba(182,198,237,0.6)" )
                , ( "borderColor", E.string "rgba(77,110,205,0.6" )
                , ( "data", E.list identity coordinates )
                , ( "fill", E.string "stack" )
                ]

        coordinates : List E.Value
        coordinates =
            -- Simple, safe, slow.
            List.map makeProfilePoint (List.range 0 (DomainModel.skipCount track.trackTree))

        makeProfilePoint : Int -> E.Value
        makeProfilePoint sequence =
            let
                gpx =
                    DomainModel.gpxPointFromIndex sequence track.trackTree

                altitude =
                    altitudeFunction gpx.altitude

                distance =
                    DomainModel.distanceFromIndex sequence track.trackTree
                        |> distanceFunction
            in
            E.object
                [ ( "x", E.float distance )
                , ( "y", E.float altitude )
                ]
    in
    chartStuff
