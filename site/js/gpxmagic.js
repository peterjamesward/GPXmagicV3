// Tell elm about prior OAuth state, and pass window size.
const app = Elm.Main.init({
    node: document.getElementById("myapp"),
    flags: rememberedBytes()

});

// OAuth integration ...
/* Fetch back generated bytes from the local storage */
function rememberedBytes() {
    const bytes = localStorage.getItem("bytes");
    return bytes ? bytes.split(",").map(x => parseInt(x,10)) : null;
}

function isUnset(x) {
    if (typeof(x) === 'undefined' || x === null) {
        return true;
    } else {
        return false;
    }
}

function isSet(x) {
    if (typeof(x) !== 'undefined' && x !== null) {
        return true;
    } else {
        return false;
    }
}

/* Generate high entropy random bytes using the Web Crypto API and
remember them so that they are preserved between redirections. This
allows to protect for XSS & authorization code attacks */
app.ports.genRandomBytes.subscribe(n => {
    const buffer = new Uint8Array(n);
    crypto.getRandomValues(buffer);
    const bytes = Array.from(buffer);
    localStorage.setItem("bytes", bytes);
    app.ports.randomBytes.send(bytes);
});

app.ports.mapCommands.subscribe(mapMessageHandler);
app.ports.storageCommands.subscribe(storageMessageHandler);

// We use port messages to synchronise our state here with Elm.
var map;
var isMapCreated = false;
var dragPointStart;
var dragging = false;
var clickToDrag = false;

var canvas;
// Make dummy feature just for making drag point feedback.
var drag = {
    'type': 'FeatureCollection',
    'features': [
        {
            'type': 'Feature',
            'geometry': {
                'type': 'Point',
                'coordinates': [0, 0]
            }
        }
    ]
};

function startDraggingPoint(e) {
//    console.log(e);
    canvas.style.cursor = 'grab';

    var coords = e.lngLat;
    drag.features[0].geometry.coordinates = [coords.lng, coords.lat];

    safelyRemoveLayer('drag');
    safelyRemoveSource('drag');

    if (isUnset(map.getSource('drag'))) {
        console.log('add source drag');
        map.addSource('drag', {
            'type': 'geojson',
            'data': drag
            });

     //   console.log('adding drag layer');
        map.addLayer({
            'id': 'drag',
            'type': 'circle',
            'source': 'drag',
            'paint': {
                'circle-radius': 5,
                'circle-color': '#000000'
            }
        });
    };


    dragging = true;
    dragPointStart = e.lngLat;
    map.on('mousemove', onMove);
    map.once('mouseup', onUp);

};

// Copied from mapbox point dragging example
function onMove(e) {
    var coords = e.lngLat;

    //console.log(e);

    // Set a UI indicator for dragging.
    canvas.style.cursor = 'grabbing';

    // Note we don't tell Elm until the point is dropped.
    // Update the Point feature in `geojson` coordinates
    // and call setData to the source layer `point` on it.
    drag.features[0].geometry.coordinates = [coords.lng, coords.lat];
    map.getSource('drag').setData(drag);

};

function onUp(e) {
    //console.log(e);
    var coords = e.lngLat;

    canvas.style.cursor = '';
    safelyRemoveLayer('drag');
    dragging = false;

    map.off('mousemove', onMove);
    map.off('touchmove', onMove);

    app.ports.mapResponses.send
        ( { 'msg' : 'drag'
          , 'start' : dragPointStart
          , 'end' : coords
      } );
};

function mapMessageHandler(msg) {
//    console.log('behold, a message from the land of Elm');
    //console.log(msg);

    switch (msg.Cmd) {
        case 'Init':
            if (!isMapCreated) {
                makeTheMap(msg);
            }
            break;

        case 'Repaint':
            if (isMapCreated) {
                map.resize();
                setTimeout(function() { map.resize(); }, 50);

            }
            break;

        case 'Track':
            if (isMapCreated) {
                addLineToMap(msg.data, msg.points);
                setClickMode(false, msg.points);
            }
            break;

        case 'Mark':
            if (isMapCreated) {
                //console.log('adding marker');
                addOptionals(msg);
            }
            break;

        case 'ShowPreview':
            if (isMapCreated) {
                showPreview(msg);
            }
            break;

        case 'HidePreview':
            if (isMapCreated) {
                hidePreview(msg.label);
            }
            break;

        case 'Drag':
            if (isMapCreated) {
                setClickMode(msg.Enable, msg.points);
            }
            break;

        case 'Centre':
            if (isMapCreated) {
                centreMap(msg.lon, msg.lat);
            }
            break;

        case 'Bounds':
            if (isMapCreated) {
                //console.log(msg.bbox);
                map.fitBounds(msg.bbox, { animate : true });
            }
            break;

        case 'Zoom':
            if (isMapCreated) {
                map.setZoom(msg.zoom);
            }
            break;

        case 'Profile':
            profileAsChart(msg.container, msg.chart);
            break;

        case 'Gradient':
            gradientChart(msg.container, msg.chart);
            break;

        case 'Style':
            if (isMapCreated) {
                map.setStyle(msg.style);
            }
            break;

        case 'Elev':
            console.log('Elm asked for route elevations')
            if (isMapCreated) {
                const source = map.getSource('route');
                if (isSet(source)) {
                    const elevations =
                        source._data.geometry.coordinates.map(
                            v => map.queryTerrainElevation(v)
                        );
                    console.log(elevations);

                    app.ports.mapResponses.send(
                      { 'msg' : 'elevations'
                      , 'elevations' : elevations
                      }
                    );
                };
            }
            break;

        case 'LandUse':
          // Ask Map for altitude data corresponding to land use nodes.
            console.log('Elm asked for specific elevations')
            if (isMapCreated) {
                const elevations =
                    msg.data.map(
                        v => map.queryTerrainElevation(v)
                    );
                console.log(elevations);

                app.ports.mapResponses.send(
                  { 'msg' : 'landuse'
                  , 'elevations' : elevations
                  }
                );
            };
            break;

        case 'Planning':
            if (isMapCreated) {
                showPlanningTools();
            }
            break;

        case 'StopPlanning':
            if (isMapCreated) {
                removePlanningTools();
            }
            break;

        case 'GetPoints':
            if (isMapCreated) {
                updateRoute();
            }
            break;
    };
};

var draw;

function showPlanningTools() {
    draw = new MapboxDraw({
      // Instead of showing all the draw tools, show only the line string and delete tools.
      displayControlsDefault: false,
      controls: {
        line_string: true,
        trash: true
      },
      // Set the draw mode to draw LineStrings by default.
      defaultMode: 'draw_line_string',
      styles: [
        // Set the line style for the user-input coordinates.
        {
          id: 'gl-draw-line',
          type: 'line',
          filter: ['all', ['==', '$type', 'LineString'], ['!=', 'mode', 'static']],
          layout: {
            'line-cap': 'round',
            'line-join': 'round'
          },
          paint: {
            'line-color': '#438EE4',
            'line-dasharray': [0.2, 2],
            'line-width': 4,
            'line-opacity': 0.7
          }
        },
        // Style the vertex point halos.
        {
          id: 'gl-draw-polygon-and-line-vertex-halo-active',
          type: 'circle',
          filter: [
            'all',
            ['==', 'meta', 'vertex'],
            ['==', '$type', 'Point'],
            ['!=', 'mode', 'static']
          ],
          paint: {
            'circle-radius': 12,
            'circle-color': '#FFF'
          }
        },
        // Style the vertex points.
        {
          id: 'gl-draw-polygon-and-line-vertex-active',
          type: 'circle',
          filter: [
            'all',
            ['==', 'meta', 'vertex'],
            ['==', '$type', 'Point'],
            ['!=', 'mode', 'static']
          ],
          paint: {
            'circle-radius': 8,
            'circle-color': '#438EE4'
          }
        }
      ]
    });

    map.addControl(draw, 'top-left');
}

function removePlanningTools() {
//    console.log('removing');
    draw.deleteAll();
    document.getElementsByClassName('mapboxgl-ctrl-group')[0].style.display = 'none' // WORK-AROUND!
//    map.removeControl(draw); //BUGGY!
}

// Use the coordinates you drew to make the Map Matching API request
function updateRoute() {

    const profile = 'driving';

    const data = draw.getAll();
    const lastFeature = data.features.length - 1;
    const coords = data.features[lastFeature].geometry.coordinates;

    // We can now send the coords back to Elm
    //console.log('COORDS', coords);
    app.ports.mapResponses.send(
      { 'msg' : 'waypoints'
      , 'waypoints' : coords
      });
}


function storageMessageHandler(msg) {
//    console.log('behold, a message from the land of Elm');
    //console.log(msg);

    switch (msg.Cmd) {

        case 'storage.set':
            //console.log(msg.key); console.log(msg.value);
            localStorage.setItem(msg.key, JSON.stringify( msg.value ) );
            break;

        case 'storage.get':
            var val = null;
            try {
              val = JSON.parse(localStorage.getItem(msg.key))
            } catch (e) {
            };
            app.ports.storageResponses.send({ 'msg' : 'storage.got', 'key' : msg.key, 'value' : val });
            break;

        case 'storage.list':
            var keys = [];
            var cnt = localStorage.length;
            for (var i = 0; i < cnt; i++) {
                var key = localStorage.key(i);
                keys.push(key);
            };
            //console.log(keys);
            app.ports.storageResponses.send({ 'msg' : 'storage.keys', 'keys' : keys });
            break;

        case 'storage.clear':
            localStorage.clear();
            break;

        case 'session.set':
            //console.log(msg.key); console.log(msg.value);
            sessionStorage.setItem(msg.key, JSON.stringify( msg.value ) );
            break;

        case 'session.get':
            var val = null;
            try {
              val = JSON.parse(sessionStorage.getItem(msg.key))
            } catch (e) {
            };
            app.ports.storageResponses.send({ 'msg' : 'session.got', 'key' : msg.key, 'value' : val });
            break;

        case 'session.list':
            var keys = [];
            var cnt = sessionStorage.length;
            for (var i = 0; i < cnt; i++) {
                var key = sessionStorage.key(i);
                keys.push(key);
            };
            //console.log(keys);
            app.ports.storageResponses.send({ 'msg' : 'session.keys', 'keys' : keys });
            break;

        case 'session.clear':
            sessionStorage.clear();
            break;

        case 'memory':
            app.ports.storageResponses.send({ 'msg' : 'memory', 'memory' : performance.memory });
            break;
    }
};

function makeTheMap(msg) {
    console.log('create the map');

    mapboxgl.accessToken = msg.token;
    var element = document.getElementById("map");
//    if (isSet(element)) // Check for the container's presence.
    {
        console.log('making the map now');

        map = new mapboxgl.Map({
            container: 'map',
            //style: 'mapbox://styles/mapbox/streets-v11',
            //style: 'mapbox://styles/mapbox/satellite-v9',
            //style: 'mapbox://styles/mapbox/satellite-streets-v11',
            //style: 'mapbox://styles/mapbox/outdoors-v11',
            style: msg.style,
            center: [msg.lon, msg.lat],
            zoom: msg.zoom
        });

        map.on('load', function (m) {
            //console.log('tell Elm the map is ready');
            isMapCreated = true;
            canvas  = map.getCanvasContainer();
            // Add zoom and rotation controls to the map.
            map.addControl(new mapboxgl.NavigationControl());
            map.setProjection('globe');

            app.ports.mapResponses.send({ 'msg' : 'map ready' });

            if (element.style.visibility === true) map.resize();
        });


//    } else {
//        //No 'map' node, we have to keep checking.
//        console.log('no map node');
//        app.ports.mapResponses.send({ 'msg' : 'no node' });
    };
};


function addDecorations() {

    console.log('ADD DECORATIONS', map.getSource('mapbox-dem'));

   if (isUnset(map.getSource('mapbox-dem'))) {
      console.log('add source terrain');
      map.addSource('mapbox-dem', {
          'type': 'raster-dem',
          'url': 'mapbox://mapbox.mapbox-terrain-dem-v1',
          'tileSize': 512,
          'maxzoom': 14
      });
      // add the DEM source as a terrain layer with exaggerated height
      map.setTerrain({ 'source': 'mapbox-dem', 'exaggeration': 1.2 });
   };

   if ( isUnset(map.getLayer('sky'))) {
      // add a sky layer that will show when the map is highly pitched
      map.addLayer({
        'id': 'sky',
        'type': 'sky',
        'paint': {
        'sky-type': 'atmosphere',
        'sky-atmosphere-sun': [0.0, 0.0],
        'sky-atmosphere-sun-intensity': 15
        }
      });
   };
};

function centreMap(lon, lat) {
    map.setCenter([lon, lat]);
};

function safelyRemoveSource(source) {
    if (isSet(map.getSource(source)))
    {   console.log("Removing source", map.getSource(source));
        try {
            map.removeSource(source);
        }
        catch (e) {
            console.log(e);
        }
    } else {
        console.log("Map says there is no source ", source);
    }
}

function safelyRemoveLayer(layer) {
    if (isSet(map.getLayer(layer)))
    {   console.log("Removing layer", map.getLayer(layer));
        try {
            map.removeLayer(layer);
        }
        catch (e) {
            console.log(e);
        }
    } else {
        console.log("Map says there is no layer ", layer);
    }
}

function addLineToMap(data, points) {

    // Attempt idempotency.
    safelyRemoveLayer('route');
    safelyRemoveSource('route');

    console.log('adding geojson data');
    console.log('add source route');
    map.addSource('route', {
        'type': 'geojson',
        'data': data
        });

    //console.log('adding route layer');
    map.addLayer({
        'id': 'route',
        'type': 'line',
        'source': 'route',
        'layout': {
        'line-join': 'round',
        'line-cap': 'round'
        },
        'paint': {
            'line-color': '#888',
            'line-width': 8
        }
    });

    map.on('click', function(e) {
      app.ports.mapResponses.send
        ( { 'msg' : 'click'
          , 'lon' : e.lngLat.lng
          , 'lat' : e.lngLat.lat
          }
        );
        e;
    });

    addDecorations();

};

function setClickMode(newMode, points) {

    clickToDrag = newMode;

    safelyRemoveLayer('points');
    safelyRemoveSource('points');

    console.log('setClickMode: add source points');
    map.addSource('points', {
        'type': 'geojson',
        'data': points
        });

    map.addLayer({
        'id': 'points',
        'type': 'circle',
        'source': 'points',
        'paint': {
            'circle-radius': 5,
            'circle-color': '#ff8f00'
        }
    });

    if (clickToDrag) {

        // When the cursor enters a feature in the point layer, prepare for dragging.
        map.on('mouseenter', 'points', function (e) {
            canvas.style.cursor = 'move';
            e;
        });

        map.on('mouseleave', 'points', function (e) {
            canvas.style.cursor = '';
            e;
        });

        map.on('mousedown', 'points', function (e) {
            // Prevent the default map drag behavior.
            e.preventDefault();
            startDraggingPoint(e);
            e;
        });

        map.on('click', function(e) {
          //console.log("click", e);
          app.ports.mapResponses.send
              ( { 'msg' : 'click'
                , 'lon' : e.lngLat.lng
                , 'lat' : e.lngLat.lat
            } );
            e;
        });

    } else {
        map.off('mouseenter','points')
           .off('mouseleave','points')
           .off('mousedown','points')
           .off('click','points')
    }

};

var orangeMarker = new mapboxgl.Marker({ color: "#FFA500" });
var purpleMarker = new mapboxgl.Marker({ color: "#800080", scale: 0.8 });
var whiteMarker = new mapboxgl.Marker({ color: "#FFFFFF", scale: 0.8 });

function addOptionals(msg) {

    if (isSet(msg.orange)) {
        orangeMarker
            .remove()
            .setLngLat([msg.orange.lon, msg.orange.lat])
            .addTo(map);
    }

    purpleMarker.remove();
    if (isSet(msg.purple)) {
        purpleMarker
            .setLngLat([msg.purple.lon, msg.purple.lat])
            .addTo(map);
    };

    setTimeout(function() { map.resize(); }, 50);
};

    //    E.object
    //        [ ( "Cmd", E.string "ShowPreview" )
    //        , ( "token", E.string mapboxKey )
    //        , ( "label", E.string tag )
    //        , ( "shape", E.string shape )
    //        , ( "colour", E.string colour )
    //        , ( "data", geoJson )
    //        ]

function showPreview(msg) {

    //console.log ( msg );
    safelyRemoveLayer(msg.label);
    safelyRemoveSource(msg.label);


    console.log('add source ', msg.label);
    map.addSource(msg.label, {
        'type': 'geojson',
        'data': msg.data
        });

    if (msg.shape === 'line') {
        map.addLayer({
            'id': msg.label,
            'type': 'line',
            'source': msg.label,
            'layout': {
                'line-join': 'round',
                'line-cap': 'round'
            },
            'paint': {
                'line-color': msg.colour,
                'line-width': 6
            }
        });
    }
    else
    {
        map.addLayer({
            'id': msg.label,
            'type': 'circle',
            'source': msg.label,
            'paint': {
                'circle-radius': 8,
                'circle-color': msg.colour
            }
        });
    };
};

function hidePreview(label) {

    //console.log("Hide", label);
    safelyRemoveLayer(label);
    safelyRemoveSource(label);

}

function addData(chart, label, data) {
    chart.data.labels.push(label);
    chart.data.datasets.forEach((dataset) => {
        dataset.data.push(data);
    });
    chart.update();
}

function removeData(chart) {
    chart.data.labels.pop();
    chart.data.datasets.forEach((dataset) => {
        dataset.data.pop();
    });
    chart.update();
}

const eventPlugin = {
    id: 'clicks',
    afterEvent: (chart, args, options) => {

        if (args.event.type === 'click') {
            const canvasPosition = Chart.helpers.getRelativePosition(args.event, chart);
            const dataX = chart.scales.x.getValueForPixel(canvasPosition.x);
            //console.log("CLICKED AT " + dataX + " IN " + chart.canvas.parentElement.id);

            app.ports.mapResponses.send (
                { 'msg' : 'profileClick'
                , 'container' : chart.canvas.parentElement.id
                , 'x' : dataX
                }
            );
        }
    }
}

Chart.register(eventPlugin);

function profileAsChart(canvasContainerDiv, chartInfo) {

    //console.log(chartInfo);

    var profileDiv = document.getElementById(canvasContainerDiv);
    var canvasId = canvasContainerDiv + '.profile.';
    var chart = Chart.getChart(canvasId);

    if ( isUnset(profileDiv) ) {
        console.log('No profile container ' + canvasContainerDiv);
        return;
    }

    // If the canvas is there, just swap the data in.
    if (isSet(chart)) {

        //console.log('Updating chart data');
        chart.data.datasets = chartInfo.data.datasets; // Profile
        chart.options.scales = chartInfo.options.scales;
        chart.update('none'); //update the chart

    } else {

        //console.log('Adding new canvas');
        var canvas = document.createElement('canvas');
        canvas.id     = canvasId;
        canvas.width  = profileDiv.width;
        canvas.height = profileDiv.height;
        canvas.style.zIndex   = 8;
        canvas.style.position = "absolute";
        profileDiv.appendChild(canvas);

        //console.log('Making chart');
        chart = new Chart(
            document.getElementById(canvasId),
            chartInfo
        );
    };
}

function gradientChart(canvasContainerDiv, chartInfo) {

    var profileDiv = document.getElementById(canvasContainerDiv);
    var canvasId = canvasContainerDiv + '.gradient.';
    var chart = Chart.getChart(canvasId);

    if ( isUnset(profileDiv) ) {
        console.log('No gradient container ' + canvasContainerDiv);
        return;
    }

    // If the canvas is there, just swap the data in.
    if (isSet(chart) ) {

        //console.log('Updating chart data');
        chart.data.datasets = chartInfo.data.datasets; // ALL
//        chart.data.datasets[0] = chartInfo.data.datasets[0]; // Gradient
//        chart.data.datasets[1] = chartInfo.data.datasets[1]; // Orange
//        chart.data.datasets[2] = chartInfo.data.datasets[2]; // Purple
        chart.options.scales = chartInfo.options.scales;

    } else {

        console.log('Adding gradient canvas');
        var canvas = document.createElement('canvas');
        canvas.id     = canvasId;
        canvas.width  = profileDiv.width;
        canvas.height = profileDiv.height;
        canvas.style.zIndex   = 8;
        canvas.style.position = "absolute";
        profileDiv.appendChild(canvas);

        console.log('Making gradient chart');
        chart = new Chart(
            document.getElementById(canvasId),
            chartInfo
        );
    };

    chart.update('none'); //update the chart

}


