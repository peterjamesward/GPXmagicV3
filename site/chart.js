/// <reference path="../../packages/d3fc/index.d.ts" />

const data =
[{"distance":0,"altitude":88.86,"gradient":-4.129949389785558},{"distance":15.496557938043683,"altitude":88.22,"gradient":-10.22763866692309},{"distance":39.94012777284023,"altitude":85.72,"gradient":-12.171675628968433},{"distance":51.935189459541874,"altitude":84.26,"gradient":-9.177084881485616},{"distance":106.8545899564033,"altitude":79.22,"gradient":-0.5148873245255288},{"distance":122.3919699207904,"altitude":79.14,"gradient":-8.894202736142717},{"distance":136.33363548414775,"altitude":77.9,"gradient":-1.8187384286824086},{"distance":146.87665788780987,"altitude":77.70825,"gradient":3.7996013347855424},{"distance":148.79709620674484,"altitude":77.781219,"gradient":9.135044843920396},{"distance":166.40818654606258,"altitude":79.39,"gradient":10.10627035338811},{"distance":254.76917123915686,"altitude":88.32,"gradient":10.688498307336777},{"distance":341.6850291349586,"altitude":97.61,"gradient":10.360499856417801},{"distance":428.64994516088535,"altitude":106.62,"gradient":9.732305632778631},{"distance":447.5560509638394,"altitude":108.46,"gradient":8.204286531053267},{"distance":462.67010134843537,"altitude":109.7,"gradient":8.562623402537914},{"distance":537.4135496951543,"altitude":116.1,"gradient":6.393001562909683},{"distance":571.0440748693334,"altitude":118.25,"gradient":7.157335134076706},{"distance":599.8257367785566,"altitude":120.31,"gradient":7.78663602635521},{"distance":626.538170658346,"altitude":122.39,"gradient":0},{"distance":640.4528417739567,"altitude":122.39,"gradient":5.426597369831853},{"distance":705.1342574159793,"altitude":125.9,"gradient":2.3549995551329546},{"distance":734.8582545383681,"altitude":126.6,"gradient":4.8016516885467855},{"distance":765.4727200462753,"altitude":128.07,"gradient":4.836369100782325},{"distance":844.8711265777133,"altitude":131.91,"gradient":6.498884399126895},{"distance":924.2693691544428,"altitude":137.07,"gradient":8.614165197074309},{"distance":976.6249938356572,"altitude":141.58,"gradient":7.773769886790636},{"distance":1028.9805440934006,"altitude":145.65,"gradient":0},{"distance":1050.5426334114088,"altitude":145.65,"gradient":6.623173069048985},{"distance":1073.1903882618703,"altitude":147.15,"gradient":7.095752828654646},{"distance":1074.176893875726,"altitude":147.22,"gradient":5.763229954346532},{"distance":1118.4229159883234,"altitude":149.77,"gradient":5.992256410511822},{"distance":1135.4448845110767,"altitude":150.79,"gradient":4.4300479385150435},{"distance":1201.1326612662742,"altitude":153.7,"gradient":13.652652804229374},{"distance":1213.2914704281523,"altitude":155.36,"gradient":5.216571511885088},{"distance":1243.5795628352096,"altitude":156.94,"gradient":2.1973253002612503},{"distance":1261.3284141297195,"altitude":157.33,"gradient":0.25270363665006007},{"distance":1281.114437260531,"altitude":157.38,"gradient":-3.517912944335244},{"distance":1298.1700042826244,"altitude":156.78,"gradient":-2.8637713148492665},{"distance":1335.1841329773172,"altitude":155.72,"gradient":0.07211467221805885},{"distance":1390.6513475810816,"altitude":155.76,"gradient":7.860507154986058},{"distance":1446.1185065573875,"altitude":160.12,"gradient":12.157705175484221},{"distance":1484.1190990484454,"altitude":164.74,"gradient":9.755435460100365},{"distance":1553.5163087136543,"altitude":171.51,"gradient":9.665175589966188},{"distance":1629.5625215484254,"altitude":178.86,"gradient":4.106233869080311},{"distance":1650.9933515512253,"altitude":179.74,"gradient":5.448011949981665},{"distance":1684.6842468145178,"altitude":181.575484,"gradient":14.453296727250967},{"distance":1686.674083496261,"altitude":181.863081,"gradient":18.618461152963473},{"distance":1707.8943173681196,"altitude":185.813962,"gradient":12.49734943469594},{"distance":1709.9427517288432,"altitude":186.069962,"gradient":7.003308348829653},{"distance":1752.7801915582822,"altitude":189.07,"gradient":13.20662890460623},{"distance":1787.6869041894831,"altitude":193.68,"gradient":7.772723943272589},{"distance":1849.9559367104323,"altitude":198.52,"gradient":7.114300380331807},{"distance":1912.2248846486884,"altitude":202.95,"gradient":9.435547840439641},{"distance":1932.1495358909042,"altitude":204.83,"gradient":6.8570756938374275},{"distance":1944.691325399993,"altitude":205.69,"gradient":7.974075306404959},{"distance":1954.473023841861,"altitude":206.47,"gradient":-0.3224725595887816},{"distance":1973.0792581448711,"altitude":206.41,"gradient":-2.549094003026441},{"distance":2008.3859199992974,"altitude":205.51,"gradient":-0.6072505075421877},{"distance":2021.560054727283,"altitude":205.43,"gradient":0},{"distance":2031.625608563495,"altitude":205.43,"gradient":0},{"distance":2056.744436270366,"altitude":205.43,"gradient":3.0466394753443757},{"distance":2103.024936848486,"altitude":206.84,"gradient":5.364507270200931},{"distance":2117.1921280129322,"altitude":207.6,"gradient":8.112814033058044},{"distance":2193.2446539904604,"altitude":213.77,"gradient":0},{"distance":2209.313765976943,"altitude":213.77,"gradient":8.622770396534445},{"distance":2273.910174600309,"altitude":219.34,"gradient":3.2894926043967754},{"distance":2294.5820557010934,"altitude":220.02,"gradient":7.7351724514939875},{"distance":2313.0690384925856,"altitude":221.45,"gradient":12.692264829765492},{"distance":2339.8570077447434,"altitude":224.85,"gradient":8.378113121332376},{"distance":2371.009606930286,"altitude":227.46,"gradient":7.04964373452333},{"distance":2418.671589954271,"altitude":230.82,"gradient":4.57910225847015},{"distance":2439.4180146108242,"altitude":231.77,"gradient":4.590437450435889},{"distance":2457.2812359019044,"altitude":232.59,"gradient":0},{"distance":2470.9165047993065,"altitude":232.59,"gradient":0.8358191717565929},{"distance":2511.595160122014,"altitude":232.93,"gradient":-0.6646746218097492},{"distance":2552.216539536928,"altitude":232.66,"gradient":-3.431038288750081},{"distance":2598.8496536367097,"altitude":231.06,"gradient":-3.757759896760338},{"distance":2637.7025883667516,"altitude":229.6,"gradient":-1.6971170045237125},{"distance":2727.2662422543963,"altitude":228.08,"gradient":-0.5459908649768385},{"distance":2800.5275412354704,"altitude":227.68,"gradient":1.4741744978780822},{"distance":2873.788881840299,"altitude":228.76,"gradient":3.782411462266916},{"distance":2904.985910819605,"altitude":229.94,"gradient":12.200793997278623},{"distance":2932.1152927330963,"altitude":233.25,"gradient":6.146804754556001},{"distance":3017.362835305782,"altitude":238.49,"gradient":10.070208831273582},{"distance":3046.259952026568,"altitude":241.4,"gradient":4.922411171912426},{"distance":3066.372046799328,"altitude":242.39,"gradient":5.3832556349665035},{"distance":3085.5054498892678,"altitude":243.42,"gradient":5.467467641368019},{"distance":3107.087653523577,"altitude":244.6,"gradient":3.7619585455015625},{"distance":3122.770973604429,"altitude":245.19,"gradient":9.403537543123656},{"distance":3152.5470019039863,"altitude":247.99,"gradient":8.696013106977787},{"distance":3231.318732301949,"altitude":254.84,"gradient":6.845594486300853},{"distance":3310.2015818602486,"altitude":260.24,"gradient":5.395328532436185},{"distance":3388.9734300331525,"altitude":264.49,"gradient":-1.6960689078112139},{"distance":3435.551726254591,"altitude":263.7,"gradient":-7.138086263379058},{"distance":3463.010066314944,"altitude":261.74,"gradient":-6.354977299640205},{"distance":3491.491678674165,"altitude":259.93,"gradient":-7.374878249886946},{"distance":3571.7120538388763,"altitude":254.013845,"gradient":0.10662770541363949},{"distance":3573.793126475127,"altitude":254.016064,"gradient":7.6101788237864305},{"distance":3653.8692474324,"altitude":260.11,"gradient":1.8709962040716863},{"distance":3706.2477609679845,"altitude":261.09,"gradient":-1.4657593522844465},{"distance":3763.5559404239643,"altitude":260.25,"gradient":-8.225513977402704},{"distance":3835.6486998100772,"altitude":254.32,"gradient":-8.641642184331708},{"distance":3907.741478787847,"altitude":248.09,"gradient":-11.702745598008297},{"distance":3957.387948082957,"altitude":242.28,"gradient":-1.7624715922101795},{"distance":3989.728894882854,"altitude":241.71,"gradient":2.7248021082340643},{"distance":4031.199796442467,"altitude":242.84,"gradient":2.8266670365825197},{"distance":4049.94979398872,"altitude":243.37,"gradient":-4.346502980262038},{"distance":4144.658920352918,"altitude":239.253465,"gradient":-10.654960343606984},{"distance":4146.589983382388,"altitude":239.047711,"gradient":-16.21427071763986},{"distance":4176.857832522594,"altitude":234.14,"gradient":-7.663410467780075},{"distance":4248.2795581761,"altitude":228.66666,"gradient":-1.2976833752254142},{"distance":4250.296537086343,"altitude":228.640486,"gradient":5.052593621685097},{"distance":4301.94355632811,"altitude":231.25,"gradient":6.556195335560654},{"distance":4336.7198234053485,"altitude":233.53,"gradient":4.523248595321078},{"distance":4390.66335422353,"altitude":235.97,"gradient":2.8660814549271088},{"distance":4430.09000757483,"altitude":237.1,"gradient":2.352792041735344},{"distance":4464.94221658669,"altitude":237.92,"gradient":3.331185326292045},{"distance":4531.885354886421,"altitude":240.15,"gradient":8.104205196287143},{"distance":4596.789930629516,"altitude":245.41,"gradient":6.466948109826064},{"distance":4696.063960526946,"altitude":251.83,"gradient":5.209564016761459},{"distance":4794.30484946762,"altitude":256.947922,"gradient":0.1739709371839475},{"distance":4796.3218530297145,"altitude":256.951431,"gradient":-4.858341430085426},{"distance":4852.7491552160855,"altitude":254.21,"gradient":-3.5338804686146146},{"distance":4910.193090233951,"altitude":252.18,"gradient":0.369637311657615},{"distance":4964.300182156869,"altitude":252.38,"gradient":-0.9545390366853687},{"distance":4995.728964895991,"altitude":252.08,"gradient":3.701240282215093},{"distance":5015.722262914381,"altitude":252.82,"gradient":-1.0136007836576806},{"distance":5016.708844629907,"altitude":252.81,"gradient":5.389514264290602},{"distance":5095.751218229798,"altitude":257.07,"gradient":5.94313554121867},{"distance":5173.833536356577,"altitude":261.710538,"gradient":-0.18601348310747454},{"distance":5175.789308561468,"altitude":261.7069,"gradient":-6.312148793508021},{"distance":5223.2675936596,"altitude":258.71,"gradient":-3.6733962761343286},{"distance":5288.874454962789,"altitude":256.3,"gradient":5.365292484735103},{"distance":5354.481316285164,"altitude":259.82,"gradient":11.505963394027088},{"distance":5416.579123790001,"altitude":266.964951,"gradient":5.83856953707511},{"distance":5418.590335598893,"altitude":267.082377,"gradient":0.23772657872535607},{"distance":5480.688145945357,"altitude":267.23,"gradient":-9.56464733682896},{"distance":5554.083429145913,"altitude":260.21,"gradient":-3.252846197828336},{"distance":5598.65977588398,"altitude":258.76,"gradient":-2.33458941635781},{"distance":5633.355384213417,"altitude":257.95,"gradient":-1.0214519315411534},{"distance":5660.76734471336,"altitude":257.67,"gradient":0.841320850163213},{"distance":5684.53948823758,"altitude":257.87,"gradient":3.598521149992986},{"distance":5714.8297089856815,"altitude":258.96,"gradient":6.979338290239285},{"distance":5739.473879084235,"altitude":260.68,"gradient":11.687080505627874},{"distance":5761.490429742553,"altitude":263.253092,"gradient":6.029214026490512},{"distance":5763.429455285292,"altitude":263.37,"gradient":0},{"distance":5781.452149222951,"altitude":263.37,"gradient":8.052916705416358},{"distance":5852.730671154506,"altitude":269.11,"gradient":7.5197876886522295},{"distance":5924.009279300432,"altitude":274.47,"gradient":5.096967699015124},{"distance":5972.4694647647675,"altitude":276.94,"gradient":1.0135434041924178},{"distance":5973.456102333384,"altitude":276.95,"gradient":-2.678865423721984},{"distance":6069.3922392233535,"altitude":274.38,"gradient":-11.892215357831633},{"distance":6160.7124825357,"altitude":263.52,"gradient":-9.819456219036597},{"distance":6199.207485880914,"altitude":259.74,"gradient":-5.718714090194091},{"distance":6281.3937943039755,"altitude":255.04,"gradient":-11.94848143913271},{"distance":6363.57997043041,"altitude":245.22,"gradient":-7.5672451824284845},{"distance":6414.721434816767,"altitude":241.35,"gradient":-8.703781394618389},{"distance":6501.3504487837545,"altitude":233.81,"gradient":-7.415626715647507},{"distance":6519.420398248342,"altitude":232.47,"gradient":-14.656851447354942},{"distance":6542.140148202559,"altitude":229.14,"gradient":-7.716990335546034},{"distance":6565.076551165862,"altitude":227.37,"gradient":-7.912840324496187},{"distance":6591.236563438469,"altitude":225.3,"gradient":-8.144303896951484},{"distance":6620.827797146018,"altitude":222.89,"gradient":-17.679268545092054},{"distance":6665.626030623832,"altitude":214.97,"gradient":-8.418643983276587},{"distance":6745.518422003735,"altitude":208.244144,"gradient":-16.735345393210945},{"distance":6747.535488231343,"altitude":207.906581,"gradient":-25.237279411266936},{"distance":6758.325724197841,"altitude":205.183419,"gradient":-18.325330690431528},{"distance":6760.215294464536,"altitude":204.837149,"gradient":-9.273924880509801},{"distance":6812.373890337636,"altitude":200,"gradient":-16.900837965101548},{"distance":6827.111224694329,"altitude":197.509267,"gradient":-8.465914470331915},{"distance":6829.1106188601125,"altitude":197.34,"gradient":0},{"distance":6847.713765068972,"altitude":197.34,"gradient":-8.146043048252924},{"distance":6849.789680933869,"altitude":197.170895,"gradient":-17.01628666004476},{"distance":6864.962145124086,"altitude":194.589105,"gradient":-10.609399333267351},{"distance":6866.991949010578,"altitude":194.373755,"gradient":-4.626467989464836},{"distance":6893.443120893462,"altitude":193.15,"gradient":-7.315469513527719},{"distance":6915.177884425178,"altitude":191.56,"gradient":-6.70133002704251},{"distance":6939.501414133792,"altitude":189.93,"gradient":-10.708343586803371},{"distance":6998.147272375951,"altitude":183.65,"gradient":-5.062612603068086},{"distance":7068.269169474073,"altitude":180.1,"gradient":-12.080162363027817},{"distance":7162.390420979754,"altitude":168.73,"gradient":-6.248962943384517},{"distance":7185.274218066658,"altitude":167.3,"gradient":-14.951447842243109},{"distance":7200.744405465265,"altitude":164.986983,"gradient":-7.003803677745216},{"distance":7202.84302225641,"altitude":164.84,"gradient":-5.220696871432374},{"distance":7217.932348823341,"altitude":164.052232,"gradient":-11.437013387401977},{"distance":7219.9181119968825,"altitude":163.82512,"gradient":-17.54867645052918},{"distance":7231.809276586318,"altitude":161.738378,"gradient":-16.540533309651426},{"distance":7233.84777796767,"altitude":161.401199,"gradient":-9.889673614404009},{"distance":7268.239196052957,"altitude":158,"gradient":-8.544836765984021},{"distance":7313.295644823592,"altitude":154.15,"gradient":-8.459909528904317},{"distance":7328.071216544888,"altitude":152.9,"gradient":-9.133761841548},{"distance":7342.961028909094,"altitude":151.54,"gradient":-5.768022539162669},{"distance":7362.725168044471,"altitude":150.4,"gradient":-6.398797686577426},{"distance":7389.605217770187,"altitude":148.68,"gradient":-7.236128395452436},{"distance":7407.432430466008,"altitude":147.39,"gradient":-7.648504168280001},{"distance":7436.980691802815,"altitude":145.13,"gradient":-9.232462999745922},{"distance":7497.202974850719,"altitude":139.57,"gradient":-6.21843316212538},{"distance":7522.289680124027,"altitude":138.01,"gradient":-8.391495034485239},{"distance":7557.6826582261965,"altitude":135.04,"gradient":0},{"distance":7575.753918300417,"altitude":135.04,"gradient":-6.748295713802685},{"distance":7577.533330541562,"altitude":134.91992,"gradient":-12.014993825556214},{"distance":7603.499552790998,"altitude":131.80008,"gradient":-5.936420979889294},{"distance":7605.522320387952,"altitude":131.68,"gradient":0},{"distance":7613.854307878729,"altitude":131.68,"gradient":-6.4408343501259075},{"distance":7615.892801540617,"altitude":131.548704,"gradient":-13.167038171599309},{"distance":7627.426936857972,"altitude":130.03,"gradient":-6.940956798922321},{"distance":7660.1313617054575,"altitude":127.76,"gradient":-10.906852663193652},{"distance":7672.325530280671,"altitude":126.43,"gradient":-5.37097665272965},{"distance":7687.778958095856,"altitude":125.6,"gradient":0},{"distance":7701.628042464288,"altitude":125.6,"gradient":-5.973676116060914},{"distance":7703.650817011357,"altitude":125.479166,"gradient":-12.099403529274456},{"distance":7766.2847475325125,"altitude":117.900834,"gradient":-6.037887191058063},{"distance":7768.286010493817,"altitude":117.78,"gradient":0},{"distance":7778.152335198036,"altitude":117.78,"gradient":-5.956753322930517},{"distance":7780.128730620469,"altitude":117.662271,"gradient":-12.391271424709513},{"distance":7790.476910044922,"altitude":116.38,"gradient":-10.931092970187619},{"distance":7851.12958236307,"altitude":109.75,"gradient":-9.635733408983285},{"distance":7872.1969979280075,"altitude":107.72,"gradient":-8.313953387788871},{"distance":7889.51727785783,"altitude":106.28,"gradient":-9.183274129165149},{"distance":7908.791449303705,"altitude":104.51,"gradient":-9.46346219376519},{"distance":7929.0800071347385,"altitude":102.59,"gradient":-4.417322747461531},{"distance":7952.397298147534,"altitude":101.56,"gradient":-5.749920241724666},{"distance":7974.832391953514,"altitude":100.27,"gradient":-9.927738464120068},{"distance":7997.093252349738,"altitude":98.06,"gradient":-6.836433221879903},{"distance":8027.5184745744355,"altitude":95.98,"gradient":-14.901243828335243},{"distance":8050.275541293602,"altitude":92.588914,"gradient":-7.382755873707285},{"distance":8052.292592954897,"altitude":92.44,"gradient":0},{"distance":8067.958584418309,"altitude":92.44,"gradient":-7.740713598610664},{"distance":8070.034465381388,"altitude":92.279312,"gradient":-16.18422576986973},{"distance":8084.360909123397,"altitude":89.960688,"gradient":-10.071638063936138},{"distance":8086.497592338519,"altitude":89.745489,"gradient":-5.484601800285524},{"distance":8133.73854244478,"altitude":87.154511,"gradient":-11.030219948941173},{"distance":8135.693834370039,"altitude":86.938838,"gradient":-16.10226920194778},{"distance":8152.199228527023,"altitude":84.281095,"gradient":-10.7542504112621},{"distance":8154.21159565373,"altitude":84.06468,"gradient":-5.5363640327136165},{"distance":8191.7425634102265,"altitude":81.986829,"gradient":-8.276253338671305},{"distance":8207.170184433304,"altitude":80.71,"gradient":-3.6123279763776095},{"distance":8232.084865655093,"altitude":79.81,"gradient":-8.264687998046147},{"distance":8257.010186495843,"altitude":77.75,"gradient":-11.88634573278632},{"distance":8267.86297530916,"altitude":76.46,"gradient":-9.07222155228032},{"distance":8299.16732544693,"altitude":73.62,"gradient":-3.454686500804312},{"distance":8390.347813148683,"altitude":70.47,"gradient":-0.7731993195943926},{"distance":8427.854312786076,"altitude":70.18,"gradient":3.0706804040474336},{"distance":8449.673581216515,"altitude":70.85,"gradient":2.895895611679586},{"distance":8465.90344800123,"altitude":71.32,"gradient":6.342441817071175},{"distance":8499.171392128377,"altitude":73.43,"gradient":1.5935376884980017},{"distance":8546.86402001447,"altitude":74.19,"gradient":-3.448016867386636},{"distance":8635.900707422754,"altitude":71.12,"gradient":-1.8756364110510277},{"distance":8724.93715340773,"altitude":69.45,"gradient":0.8262349261852627},{"distance":8755.194891500969,"altitude":69.7,"gradient":1.0283050515058056},{"distance":8814.51580985168,"altitude":70.31,"gradient":-1.244266457503606},{"distance":8869.970169795157,"altitude":69.62,"gradient":-0.6672161512551172},{"distance":8925.42446287604,"altitude":69.25,"gradient":-1.1800261273122032},{"distance":8955.084823091725,"altitude":68.9,"gradient":-0.2896323967775804},{"distance":8982.706042572034,"altitude":68.82,"gradient":1.0061276110013098},{"distance":9031.40761826106,"altitude":69.31,"gradient":0.5310981778126894},{"distance":9082.245676322564,"altitude":69.58,"gradient":2.970216610512438},{"distance":9133.083719401835,"altitude":71.09,"gradient":5.381808797273062},{"distance":9156.681733523843,"altitude":72.36,"gradient":9.019489747313171},{"distance":9249.369903633358,"altitude":80.72,"gradient":7.304069944204701},{"distance":9342.05795360708,"altitude":87.49,"gradient":2.982456038416929},{"distance":9369.887366322935,"altitude":88.32,"gradient":1.3429282926305703},{"distance":9401.16230500021,"altitude":88.74,"gradient":-1.045290628795812},{"distance":9420.29573971113,"altitude":88.54,"gradient":-0.4813682745766676}]

const xExtent = fc.extentLinear()
  .accessors([d => d.distance])
  ;

const yExtent = fc.extentLinear()
  .accessors([d => d.altitude])
  ;

const yExtentGradient = fc.extentLinear()
  .accessors([d => d.gradient])
  ;

const xAxis = fc.axisBottom(xExtent)
  .tickArguments([5])
  .tickCenterLabel(true);

const yAxis = fc.axisBottom(yExtent)
  .tickArguments([5])
  .tickCenterLabel(true);

const yAxisGradient = fc.axisBottom(yExtentGradient)
  .tickArguments([5])
  .tickCenterLabel(true);

// gridlines (from d3fc-annotation)
// n.b. the gridlines are rendered using SVG
var gridlines = fc.annotationSvgGridline();

var line = fc.seriesCanvasLine()
    .crossValue(d => d.distance)
    .mainValue(d => d.altitude)
    ;

var gradient = fc.seriesCanvasBar()
    .crossValue(d => d.distance)
    .mainValue(d => d.gradient)
    ;

var area = fc.seriesCanvasArea()
    .crossValue(d => d.distance)
    .mainValue(d => d.altitude)
    .decorate((context, datum, index) => {
        context.fillStyle = '#eeeeee';
    })
    ;

// combine into a single series
var multi = fc.seriesCanvasMulti()
  .series([area, line]);

// the Cartesian component, which uses d3fc-element for layout
// of the standard features of a chart (axes, labels, plot area)
var chart = fc.chartCartesian(
    d3.scaleLinear(),
    d3.scaleLinear()
  )
  .yDomain(yExtent(data))
  .xDomain(xExtent(data))
  .yOrient('left')
  .svgPlotArea(gridlines)
  .canvasPlotArea(multi);

var gradientChart = fc.chartCartesian(
    d3.scaleLinear(),
    d3.scaleLinear()
  )
  .yDomain(yExtentGradient(data))
  .xDomain(xExtent(data))
  .yOrient('left')
  .svgPlotArea(gridlines)
  .canvasPlotArea(gradient);

// render
d3.select('#altitude')
  .datum(data)
  .call(chart);

d3.select('#gradient')
  .datum(data)
  .call(gradientChart);

