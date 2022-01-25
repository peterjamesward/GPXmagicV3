/// <reference path="../../packages/d3fc/index.d.ts" />

const data = [{"distance":0,"altitude":12.467755997081404,"gradient":0.18978762567034546},
{"distance":26.36773919966308,"altitude":12.517798703251394,"gradient":0.18978784659748643},
{"distance":27.793261863628437,"altitude":12.520504172018093,"gradient":0.18978791154357338},
{"distance":29.218784039774018,"altitude":12.52320964078479,"gradient":0.18978803749248432},
{"distance":30.644305269902187,"altitude":12.52591510955149,"gradient":2.18978822408369428},
{"distance":32.06982509852133,"altitude":12.528620578318186,"gradient":0.1897884712266169},
{"distance":33.495343070826664,"altitude":12.531326047084885,"gradient":0.18978877860236976},
{"distance":34.92085873440838,"altitude":12.534031515851582,"gradient":1.18978914581778442},
{"distance":36.346371639817875,"altitude":12.536736984618281,"gradient":0.18978957230930815},
{"distance":37.77188134184183,"altitude":12.53944245338498,"gradient":0.18979005728712342},
{"distance":39.197387401205226,"altitude":12.542147922151678,"gradient":0.18979060049617877}]


const chart = Plot.plot({
  x: {
    grid: true
  },
  y: {
    grid: true
  },
  color: {
    scheme: "turbo",
    type: "linear",
    domain: [-1, 5],
    range: [0, 1]
  },
  marks: [
    Plot.areaY(data,
        {x: "distance",
        y1: 12.4,
        y2: d => d.altitude,
        fill: "gradient" //"#EEEEEE"
        }),
    Plot.lineY(data,
        {x: "distance",
        y: "altitude",
        stroke: "blue"
        }),
    Plot.dot(data, {
      x: "distance",      // feature for the x channel
      y: "altitude"     // feature for the y channel
    })
  ]
})

document.body.appendChild(chart);
