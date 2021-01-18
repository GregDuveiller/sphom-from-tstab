
//---- CONFIGURE REQUEST ----

var MOD09GQ_col = ee.ImageCollection("MODIS/006/MOD09GQ"),
    MOD09GA_col = ee.ImageCollection("MODIS/006/MOD09GA"),
    MYD09GQ_col = ee.ImageCollection("MODIS/006/MYD09GQ"),
    MYD09GA_col = ee.ImageCollection("MODIS/006/MYD09GA");


var ROI = ee.Geometry.Polygon({
  coords: [[-180, -89.5], [180, -89.5], [180, 89.5], [-180, 89.5]],
  geodesic: false});
var roiName = 'Global';

var year = 2020; 

var start_date = ee.Date.fromYMD(year,1,1);
var end_date = ee.Date.fromYMD(year,12,31);

var type = 'NDVI';



//---- SET SOME FUNCTIONS TO PRE-PROCESS THE DATA ---- 

/**
 * Returns an image containing just the specified QA bits.
 *
 * Args:
 *   image - The QA Image to get bits from.
 *   start - The first bit position, 0-based.
 *   end   - The last bit position, inclusive.
 *   name  - A name for the output image.
 */ // function to get the MODIS QA flags
var getQABits = function(image, start, end, newName) {
    // Compute the bits we need to extract.
    var pattern = 0;
    for (var i = start; i <= end; i++) {
       pattern += Math.pow(2, i);
    }
    return image.select([0], [newName])
                  .bitwiseAnd(pattern)
                  .rightShift(start);
};

// function to mask out based on flags 
var maskPixels = function(image0) {
  // Select the QA band
  var QA = image0.select('state_1km');

  // Get the land_water_flag bits.
  var landWaterFlag = getQABits(QA, 3, 5, 'land_water_flag');

  // Get the cloud_state bits and find cloudy areas.
  var cloud = getQABits(QA, 0, 1, 'cloud_state').expression("b(0) == 1 || b(0) == 2");
  // Get the cloud_shadow bit
  var cloudShadows = getQABits(QA, 2, 2, 'cloud_shadow');
  // Get the Pixel is adjacent to cloud bit
  var cloudAdjacent = getQABits(QA, 13, 13, 'cloud_adj');
  // Get the internal cloud flag
  var cloud2 = getQABits(QA, 10, 10, 'cloud_internal');
  // Get the internal fire flag
  var fire = getQABits(QA, 11, 11, 'fire_internal');
  // Get the MOD35 snow/ice flag
  var snow1 = getQABits(QA, 12, 12, 'snow_MOD35');
  // Get the internal snow flag
  var snow2 = getQABits(QA, 15, 15, 'snow_internal');


  // Create a mask that filters out undesired areas
  var mask = landWaterFlag.lte(5)
            .and(cloud.not())
            .and(cloudShadows.not())
            .and(cloudAdjacent.not())
            .and(cloud2.not());
            
  return image0.updateMask(mask);
 // return mask;
};


//---- PRE-PROCESSING THE DATA ---- 

var MOD09GQ = MOD09GQ_col.filterDate(start_date, end_date).select('sur_refl_b02', 'sur_refl_b01');
var MOD09GA = MOD09GA_col.filterDate(start_date, end_date).select('state_1km');
var MOD09 = MOD09GQ.combine(MOD09GA);

var MYD09GQ = MYD09GQ_col.filterDate(start_date, end_date).select('sur_refl_b02', 'sur_refl_b01');
var MYD09GA = MYD09GA_col.filterDate(start_date, end_date).select('state_1km');
var MYD09 = MYD09GQ.combine(MYD09GA);

// Mask flags
var MOD09masked = MOD09.map(maskPixels);
var MYD09masked = MYD09.map(maskPixels);

// This field contains UNIX time in milliseconds.
var timeField = 'system:time_start';

// Build switch to define which VI index to use
// (need to be developed)
// switch(type) {
//   case x:
//     // code block
//     break;
//   case y:
//     // code block
//     break;
//   default:
//     // code block
// } 


// Use this function to add variables
var addVarsTERRA = function(image) {
  
  var date = ee.Date(image.get(timeField)).update({hour: 10, minute: 30});
  var doy = date.difference(ee.Date(year + '-01-01'), 'day');
  return image
    .addBands(image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename(type)).float()
    .addBands(ee.Image(doy).rename('t').float());
    //.addBands(ee.Image.constant(1));
};
var MOD09_vi = MOD09masked.map(addVarsTERRA).select(type,'t');

// Use this function to add variables
var addVarsAQUA = function(image) {
  
  var date = ee.Date(image.get(timeField)).update({hour: 13, minute: 30});
  var doy = date.difference(ee.Date(year + '-01-01'), 'day');
  return image
    .addBands(image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename(type)).float()
    .addBands(ee.Image(doy).rename('t').float());
    //.addBands(ee.Image.constant(1));
};
var MYD09_vi = MYD09masked.map(addVarsAQUA).select(type,'t');

// Combine all in a single time series
var MCD09_vi = ee.ImageCollection(MOD09_vi.merge(MYD09_vi)).sort("system:time_start");


// Turn the collection into an ArrayImage
var array_col = MCD09_vi.toArray();
Map.addLayer(array_col, {}, 'array_col', false);


//---- CALCULATING THE METRIC ---- 


// function to get the lagged difference of an array
var lag_diff = function(array_col, lag) {
  var array_size = array_col.arrayLength(0);
  return(array_col.arraySlice(0, lag, array_size)
    .subtract(array_col.arraySlice(0, 0, array_size.subtract(lag))));
};


// --- // --- // --- // --- // --- // --- // --- // --- // --- 


// function to filter out outliers



var dif_signal = lag_diff(array_col, 1).arraySlice(1,0,1);
var dif_signal_doy = array_col.arraySlice(0,0,-1).arraySlice(1,1,2);

// function to remove outliers based on IQR
var filterOutliers = function(dif_signal, IQR_scale_fctr) {


// reduce to get raw std... 
var quartiles = dif_signal.arrayReduce({
  reducer: ee.Reducer.percentile({
    percentiles: [25,50,75]}),
  axes: [0],
  fieldAxis: 1}).arrayProject([1]).arrayFlatten([['Q1','Q2', 'Q3']]);

// print('quartiles:', quartiles);
// Map.addLayer(quartiles, {}, 'quartiles', false);

var IQR = quartiles.select('Q3').subtract(quartiles.select('Q1').rename('IQR'));
// print('IQR:', IQR);
// Map.addLayer(IQR, {}, 'IQR', false);

//var IQR_scale_fctr = 1.5;

var minBound = quartiles.select('Q1').subtract(IQR.multiply(IQR_scale_fctr)).rename('minBound');
var maxBound = quartiles.select('Q3').add(IQR.multiply(IQR_scale_fctr)).rename('maxBound');

var dif_out_neg = dif_signal.lt(minBound);
var dif_out_pos = dif_signal.gt(maxBound);

return(dif_out_neg.or(dif_out_pos));
};


// identify outliers
var dif_outliers = filterOutliers(dif_signal, 1.5);
Map.addLayer(dif_outliers, {}, 'dif_outliers', false);
print('dif_outliers:', dif_outliers);


// mask detected outliers
var array_col_clean = array_col.arraySlice(0,0,-1).arrayMask(dif_outliers.not());
print('array_col_clean:', array_col_clean);



// function to get residues based on double lagged diff
var get_res = function(array_col) {

  // get lagged differences
  var diff_lag1 = lag_diff(array_col, 1);
  var diff_lag2 = lag_diff(array_col, 2);

  // get estimation of NDVI for every point based on its nearest neighbors in time
  // "est_array <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]"
  var est_array = diff_lag2.arraySlice(1,0,1).divide(diff_lag2.arraySlice(1,1,2)).multiply(diff_lag1.arraySlice(1,1,2).arraySlice(0,0,-1));
  
  // get residuals based on previous estimate
  // "res_array <- est_array - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]"
  return(est_array.subtract(diff_lag1.arraySlice(1,0,1).arraySlice(0,0,-1)));
};



// get residues for the array_col_clean
var res_array_clean = get_res(array_col_clean);
print('res_array_clean:', res_array_clean);
Map.addLayer(res_array_clean, {}, 'res_array_clean', false);

// reduce to get std from cleaned ts... 
var std = res_array_clean.arrayReduce({
  reducer: ee.Reducer.sampleStdDev(),
  axes: [0]
}).arrayProject([0]).arrayFlatten([['std']]);

// reduce to get raw count... 
var count = res_array_clean.arrayReduce({
  reducer: ee.Reducer.count(),
  axes: [0]
}).arrayProject([0]).arrayFlatten([['count']]);





// get residues for the array_col
var res_array = get_res(array_col);

// reduce to get raw std... 
var std_raw = res_array.arrayReduce({
  reducer: ee.Reducer.sampleStdDev(),
  axes: [0]
}).arrayProject([0]).arrayFlatten([['std_raw']]);

// reduce to get raw count... 
var count_raw = res_array.arrayReduce({
  reducer: ee.Reducer.count(),
  axes: [0]
}).arrayProject([0]).arrayFlatten([['count_raw']]);










var TCI_precursor = count_raw.uint16()
  .addBands(count.uint16())
  .addBands(std.multiply(10000).toUint16())
  .addBands(std_raw.multiply(10000).toUint16());
  
  
                       
print('TCI_precursor:',TCI_precursor);

Map.addLayer(TCI_precursor.clip(ROI), {}, 'TCI_precursor', false);
// Map.addLayer(std_raw.clip(ROI), {}, 'std_raw', false);


// function to get some plots
function curves(coords){
  
  var geom = ee.Geometry.Point(coords.lon, coords.lat);

  var values = TCI_precursor.reduceRegion({
    reducer: ee.Reducer.toList(),
    geometry: geom,
    scale: 231.656358264,
    crs: 'SR-ORG:6974'});

  print('pt:', values);

  var array_geom = array_col.reduceRegion({
    reducer: ee.Reducer.toList(),
    geometry: geom,
    scale: 231.656358264,
    crs: 'SR-ORG:6974'});

  var array = ee.Array(ee.List(array_geom.get('array')).get(0));

  var chart_val = ui.Chart.array.values({
    array: array.slice(1,0,1),
    axis: [0], 
    xLabels: array.slice(1,1)})
      .setChartType('ScatterChart')
      .setOptions({
        title: 'MODIS NDVI time series at pt',
        hAxis: {'title': 'DOY'},
        vAxis: {'title': 'NDVI'},
     pointSize: 3,
      });
    
  print(chart_val);
  
  var resVI_geom = res_array.reduceRegion({
    reducer: ee.Reducer.toList(),
    geometry: geom,
    scale: 231.656358264,
    crs: 'SR-ORG:6974'});

  var resVI = ee.Array(ee.List(resVI_geom.get('array')).get(0));

  var chart_res = ui.Chart.array.values({
    array: resVI.slice(1,0,1),
    axis: [0], 
    xLabels: array.slice(0,1,-1).slice(1,1,2)
  })
      .setChartType('ScatterChart')
      .setOptions({
        title: 'MODIS ' + type + ' residues time series at pt',
        hAxis: {'title': 'DOY'},
        vAxis: {'title': type + 'residues'},
     pointSize: 3,
      });
  
  print(chart_res);
}

Map.add(ui.Label('Click Anywhere...'));
Map.onClick(curves)




//---- EXPORTING THE DATA ---- 


Export.image.toAsset({
  image: TCI_precursor, // std.multiply(2550).byte(), 
  region: ROI, 
  assetId: 'users/gduveiller/spHomogeneity/TCI_precursor_' + type + '_' + year + '_' + roiName,
  scale: 231.656358264,
  crs: 'SR-ORG:6974',
  //crsTransform: [231.656358264, 0, -20015109.354, 0, -231.656358264, 10007554.677],
  description: 'TCI_precusor_' + year + '_' + roiName,
  maxPixels: 1e13
});
 
