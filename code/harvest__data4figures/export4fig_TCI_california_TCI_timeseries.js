// Script to extract TCI from a time series over California, which is to be matched
// with data from the Cropland Data Layer for the USDA showing which crop is where.
// Together, they are put together in a figure labelled "fig___california-TCI-ts"
//
// Gregory Duveiller - Jan 2021
// -------------------------------------------------------------------------------// 


// define zone of interest
var zone4clip = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-120.16259770132999, 36.34641572154046],
          [-120.16259770132999, 35.71449858142742],
          [-119.26034550406436, 35.71449858142742],
          [-119.26034550406436, 36.34641572154046]]], null, false);


// palettes
var palettes = require('users/gena/packages:palettes');
var pal_magma = palettes.matplotlib.magma[7];
var pal_plasm = palettes.matplotlib.plasma[7];
var pal_virid = palettes.matplotlib.viridis[7];
var pal_RdBu7 = palettes.colorbrewer.RdBu[7];

// settings 
var roiName = 'Global';
var type = 'NDVI';
var year0 = 2007; 
var img_name = "users/gduveiller/spHomogeneity/TCI_precursor_" + type + "_" + year0 + "_" + roiName;
var TCI_pre = ee.Image(img_name);



// calc TCI
var TCI_stack = TCI_pre.select('std').divide(10000)
  //.clip(zone4clip)
  .log().add(1.4189).divide(ee.Number(0.01).log().add(1.4189))
  .rename('TCI_' + year0);

// Loop the years...
for(var year=year0+1; year<2020; year++) {

var img_name = "users/gduveiller/spHomogeneity/TCI_precursor_" + type + "_" + year + "_" + roiName;
var TCI_pre = ee.Image(img_name);

var TCI_nextyear = TCI_pre.select('std').divide(10000)
  //.clip(zone4clip)
  .log().add(1.4189).divide(ee.Number(0.01).log().add(1.4189))
  .rename('TCI_' + year);
  
var TCI_stack = TCI_stack.addBands(TCI_nextyear);
// Map.addLayer(TCI, {min: 0, max: 1, palette: pal_magma}, 'TCI'+year);
}

// visualize for control
Map.centerObject(zone4clip, 8);
Map.addLayer(TCI_stack);


Export.image.toDrive({
  image: TCI_stack.multiply(10000).uint16(),
  description: 'TCI_ts_' + type + '_' + 'California_WGS84',
  folder: 'GEE_exported_data', 
  scale: 231.656358264,
  crs: 'EPSG:4326', //'SR-ORG:6974', 
  region: zone4clip,
  maxPixels: 1e13
});


