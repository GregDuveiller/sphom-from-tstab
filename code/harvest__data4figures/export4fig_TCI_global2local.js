// Script to extract TCI from global to regional scale for illustrative purposes
// A global image is prepared at lower spatial res and in WGS84
// A subset at high spatial resolution is then prepared for a selected region
// Together, they are put together in a figure labelled "TCI_global2local"
//
// Gregory Duveiller - Jan 2021
// -------------------------------------------------------------------------------// 


// define global ROI
var ROI = ee.Geometry.Polygon({
  coords: [[-180, -89.5], [180, -89.5], [180, 89.5], [-180, 89.5]],
  geodesic: false});

// define regional ROI
var zone = 
    /* color: #d63000 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-71.04135089495598, -9.21465544400193],
          [-71.04135089495598, -23.547648561082667],
          [-52.66122394183098, -23.547648561082667],
          [-52.66122394183098, -9.21465544400193]]], null, false);


// settings to select the data layer to fetch
var roiName = 'Global';
var type = 'NDVI';
var year = 2019; 

// get assets
var img_name = "users/gduveiller/spHomogeneity/TCI_precursor_" + type + "_" + year + "_" + roiName;
var TCI_pre = ee.Image(img_name);

// calculate TCI
var TCI = TCI_pre.select('std').divide(10000)
  .log().add(1.4189).divide(ee.Number(0.01).log().add(1.4189))
  .rename('TCI');

// palettes
var palettes = require('users/gena/packages:palettes');
var pal_magma = palettes.matplotlib.magma[7];

// add to viewer for control
Map.addLayer(TCI, {min: 0, max: 1, palette: pal_magma}, 'TCI');
Map.centerObject(ROI, 3);

// export TCI for global snapshot at LR
Export.image.toDrive({
  image: TCI.multiply(10000).uint16(),
  description: 'TCI_' + type + '_' + year + '_Global_' + 'WGS84',
  folder: 'GEE_exported_data', 
  scale: 10000,
  crs: 'EPSG:4326', //'SR-ORG:6974', 
  region: ROI,
  maxPixels: 1e13
});


// export TCI over the regional zoom
Export.image.toDrive({
  image: TCI.multiply(10000).uint16(),
  description: 'TCI_' + type + '_' + year + '_' + 'BoliviaMatoGrosso_WGS84',
  folder: 'GEE_exported_data', 
  scale: 231.656358264,
  crs: 'EPSG:4326', //'SR-ORG:6974', 
  region: zone,
  maxPixels: 1e13
});
