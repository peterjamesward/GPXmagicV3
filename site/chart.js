/// <reference path="../../packages/d3fc/index.d.ts" />

const data = [{"distance":0,"altitude":12.467755997081404,"gradient":0.18978762567034546},
{"distance":26.36773919966308,"altitude":12.517798703251394,"gradient":0.18978784659748643},
{"distance":27.793261863628437,"altitude":12.520504172018093,"gradient":0.18978791154357338},
{"distance":29.218784039774018,"altitude":12.52320964078479,"gradient":0.18978803749248432},
{"distance":30.644305269902187,"altitude":12.52591510955149,"gradient":2.18978822408369428},
{"distance":32.06982509852133,"altitude":12.528620578318186,"gradient":0.1897884712266169},
{"distance":33.495343070826664,"altitude":12.531326047084885,"gradient":0.18978877860236976},
{"distance":34.92085873440838,"altitude":12.534031515851582,"gradient":0.18978914581778442},
{"distance":36.346371639817875,"altitude":12.536736984618281,"gradient":0.18978957230930815},
{"distance":37.77188134184183,"altitude":12.53944245338498,"gradient":0.18979005728712342},
{"distance":39.197387401205226,"altitude":12.542147922151678,"gradient":0.18979060049617877}]

const xExtent = fc.extentLinear()
  .accessors([d => d.distance])
  .include([0, 40]);

const yExtent = fc.extentLinear()
  .accessors([d => d.altitude])
  .include([12.2, 12.7]);

const xAxis = fc.axisBottom(xExtent)
  .tickArguments([5])
  .tickCenterLabel(true);

const yAxis = fc.axisBottom(yExtent)
  .tickArguments([5])
  .tickCenterLabel(true);

// gridlines (from d3fc-annotation)
// n.b. the gridlines are rendered using SVG
var gridlines = fc.annotationSvgGridline();

var line = fc.seriesCanvasLine()
    .crossValue(d => d.distance)
    .mainValue(d => d.altitude)
    .decorate((context, datum, index) => {
        context.fillStyle = '#4444ff';
      });

const color = d3.scaleOrdinal(d3.schemeCategory10);

var area = fc.seriesCanvasArea()
    .crossValue(d => d.distance)
    .mainValue(d => d.altitude)
    .decorate((context, datum, index) => {
        context.fillStyle = '#eeeeee';
    });

// combine into a single series
var multi = fc.seriesCanvasMulti()
  .series([area, line]);

// the Cartesian component, which uses d3fc-element for layout
// of the standard features of a chart (axes, labels, plot area)
var chart = fc.chartCartesian(
    d3.scaleLinear(),
    d3.scaleLinear()
  )
  .xLabel('Distance')
  .yLabel('Altitude')
  .yDomain(yExtent(data))
  .xDomain(xExtent(data))
  .yOrient('left')
  .svgPlotArea(gridlines)
  .canvasPlotArea(multi);

// render
d3.select('#chart')
  .datum(data)
  .call(chart);

