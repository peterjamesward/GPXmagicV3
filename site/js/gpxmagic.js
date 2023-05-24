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


var canvas;

function elmMessageHandler(msg) {
//    console.log('behold, a message from the land of Elm');
    //console.log(msg);
    // Repurposed from map to canvsa profile commands.

    switch (msg.Cmd) {

        case 'Profile':
            profileAsChart(msg.container, msg.chart);
            break;

        case 'Gradient':
            gradientChart(msg.container, msg.chart);
            break;

    };
};

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

        //console.log('Adding gradient canvas');
        var canvas = document.createElement('canvas');
        canvas.id     = canvasId;
        canvas.width  = profileDiv.width;
        canvas.height = profileDiv.height;
        canvas.style.zIndex   = 8;
        canvas.style.position = "absolute";
        profileDiv.appendChild(canvas);

        //console.log('Making gradient chart');
        chart = new Chart(
            document.getElementById(canvasId),
            chartInfo
        );
    };

    chart.update('none'); //update the chart

}


