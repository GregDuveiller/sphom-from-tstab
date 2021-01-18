/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var ROI = 
    /* color: #ffc82d */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[105.05477776468128, 13.301182729323495],
          [105.05477776468128, 13.192571798232288],
          [105.1711641782555, 13.192571798232288],
          [105.1711641782555, 13.301182729323495]]], null, false);
/***** End of imports. If edited, may not auto-convert in the playground. *****/
// Script to extract a MODIS data block and S2 image from GEE to create a figure
// for a specific test zone "rondonia"
//
// Gregory Duveiller - Jan 2021
// -------------------------------------------------------------------------------//


//---- SET-UP ----



var MOD09GQ_col = ee.ImageCollection("MODIS/006/MOD09GQ"),
    MOD09GA_col = ee.ImageCollection("MODIS/006/MOD09GA"),
    MYD09GQ_col = ee.ImageCollection("MODIS/006/MYD09GQ"),
    MYD09GA_col = ee.ImageCollection("MODIS/006/MYD09GA");


// set the name of the ROI
var roiName = 'rovieng';
var year = 2019;
var start_date = ee.Date.fromYMD(year,1,1);
var end_date = ee.Date.fromYMD(year,12,31);

// This is selected manually by exploring them first in the S2 explorer
var S2_img = ee.Image('COPERNICUS/S2_SR/20191117T032021_20191117T033345_T48PWV');

// Visual check
Map.addLayer(S2_img.clip(ROI), {gamma: 1.3, min: 200, max: 2600, bands: ['B4', 'B3', 'B2']});
Map.centerObject(ROI);


//---- SET SOME FUNCTIONS TO PREPROCESS THE DATA ----

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
  var mask = landWaterFlag.eq(1)
            .and(cloud.not()).and(cloudShadows.not()).and(cloudAdjacent.not()).and(cloud2.not())
            .and(fire.not())
            .and(snow1.not()).and(snow2.not());

  return image0.updateMask(mask);
 // return mask;
}

// Use this function to add variables for NDVI
var addNDVI = function(image) {
  // Return the image with the added bands.
  return image.addBands(image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01']).rename('NDVI')).float();
  };


//---- PROCESSING THE DATA ----

var MOD09GQ = MOD09GQ_col.filterDate(start_date, end_date).select('sur_refl_b02', 'sur_refl_b01');
var MOD09GA = MOD09GA_col.filterDate(start_date, end_date).select('state_1km');
var MOD09 = MOD09GQ.combine(MOD09GA);

var MYD09GQ = MYD09GQ_col.filterDate(start_date, end_date).select('sur_refl_b02', 'sur_refl_b01');
var MYD09GA = MYD09GA_col.filterDate(start_date, end_date).select('state_1km');
var MYD09 = MYD09GQ.combine(MYD09GA);

var MOD09masked = MOD09.map(maskPixels);
var MYD09masked = MYD09.map(maskPixels);

// This field contains UNIX time in milliseconds.
var timeField = 'system:time_start';

// Mask flags
var MOD09ndvi = MOD09masked.map(addNDVI).select('NDVI');
var MYD09ndvi = MYD09masked.map(addNDVI).select('NDVI');


var MCD09ndvi = ee.ImageCollection(MOD09ndvi.merge(MYD09ndvi));

var filteredMODIS = MCD09ndvi.sort("system:time_start");


// Get info on projection
var img = MOD09.first().select('sur_refl_b01');
var proj = img.projection();
var scale = img.projection().nominalScale();
print(scale);
print(proj)
var crs1 = 'SR-ORG:6974';



// TERRA data

var MOD09ndvi_subset = MOD09ndvi.select('NDVI').getRegion(ROI, scale, crs1);
print('MOD09ndvi_subset is ',MOD09ndvi_subset);

var MOD09ndvi_subset_FC = ee.FeatureCollection(MOD09ndvi_subset.map(function(list) {
    var list = ee.List(list);
    var dict = {
       col1: list.get(0),
        col2: list.get(1),
        col3: list.get(2),
        col4: list.get(3),
        col5: list.get(4)
    }
    return ee.Feature(null, dict)
}))

Export.table.toDrive({
  collection: MOD09ndvi_subset_FC,
  description: 'MODIS_TERRA_NDVI_datablock' + '_' + roiName + '_' + year,
  folder: 'GEE_exported_data',
  });


// AQUA data
var MYD09ndvi_subset = MYD09ndvi.select('NDVI').getRegion(ROI, scale, crs1);
print('MYD09ndvi_subset is ',MYD09ndvi_subset);

var MYD09ndvi_subset_FC = ee.FeatureCollection(MYD09ndvi_subset.map(function(list) {
    var list = ee.List(list);
    var dict = {
       col1: list.get(0),
        col2: list.get(1),
        col3: list.get(2),
        col4: list.get(3),
        col5: list.get(4)
    }
    return ee.Feature(null, dict)
}))

Export.table.toDrive({
  collection: MYD09ndvi_subset_FC,
  description: 'MODIS_AQUA_NDVI_datablock' + '_' + roiName + '_' + year,
  folder: 'GEE_exported_data',
  });


// export S2 snapshot
Export.image.toDrive({
  image: S2_img.select('B2', 'B3', 'B4', 'B8').clip(ROI),
  description: 'S2_L2A_image_' + roiName + '_' + year,
  region: ROI,
  crs: 'EPSG:4326',     // <- we explicitely export in lat/lon to facilite figuremaking
  maxPixels: 10000000000000,
  folder: 'GEE_exported_data',
  scale:10,
  });


