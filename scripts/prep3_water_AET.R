# exploring water availability aet data
# Reprojecting and mosaic the water availability rasters.

# Note: Very large files, not working well in QGIS for some reason

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra,
  gdalUtilities
)

### Data in --------------------------------------------------------------------

#binary treemap raster, mosaic from regions 1-6
folder_tmr <- file.path("data", "region_treemap")
tmr1 <- terra::rast(file.path(folder_tmr, "mosaic_r1-r6_treemap_mask.tif"))

folder_wa <- file.path("data", "water_availability", "post_treatment_AET")
folder_w_out <- file.path("data", "water_availability", "AET_processed")
dir.create(folder_w_out)

# currently split between Interior and Coast
#  will need to mosaic/merge at some point (towards end, probably)

# #check gdal info
# gdalUtilities::gdalinfo(file.path(
#   folder_wa,
#   "Mgmt_Immediate_AET_BFTreatment_Undisturbed_WestCoast-007.tif"
# ))
# #float32. compression packbits.
# # 'Unknown based on IAU 1976 ellipsoid'. 9001?
# # after read in: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=IAU76 +units=m +no_defs

#baseline is undisturbed post-treatment
blc <- terra::rast(file.path(
  folder_wa,
  "Mgmt_Immediate_AET_BFTreatment_Undisturbed_WestCoast-007.tif"
))
bli <- terra::rast(file.path(
  folder_wa,
  "Mgmt_Immediate_AET_BFTreatment_Undisturbed_WestInterior-028.tif"
))

burnc <- terra::rast(file.path(
  folder_wa,
  "Mgmt_Immediate_AET_BFTreatment_PrescribedFirePublic_WestCoast-003.tif"
))
burni <- terra::rast(file.path(
  folder_wa,
  "Mgmt_Immediate_AET_BFTreatment_PrescribedFirePublic_WestInterior-026.tif"
))

thinc <- terra::rast(file.path(
  folder_wa,
  "Mgmt_Immediate_AET_BFTreatment_MultiTreat_MediumThinMastRXburn_WestCoast-035.tif"
))
thini <- terra::rast(file.path(
  folder_wa,
  "Mgmt_Immediate_AET_BFTreatment_MultiTreat_MediumThinMastRXburn_WestInterior-034.tif"
))


### Project & resave -----------------------------------------------------------

# Use mosaicked FVS treemap as guide, so pixels aligned
#  values don't matter.
# Takes a while (hours)
blc_tm <- terra::project(blc, tmr1, method = "bilinear")
bli_tm <- terra::project(bli, tmr1, method = "bilinear")
burnc_tm <- terra::project(burnc, tmr1, method = "bilinear")
burni_tm <- terra::project(burni, tmr1, method = "bilinear")
thinc_tm <- terra::project(thinc, tmr1, method = "bilinear")
thini_tm <- terra::project(thini, tmr1, method = "bilinear")

terra::writeRaster(
  blc_tm,
  file.path(folder_w_out, "undisturbed_westcoast.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  bli_tm,
  file.path(folder_w_out, "undisturbed_westinterior.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  burnc_tm,
  file.path(folder_w_out, "rxfire_westcoast.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  burni_tm,
  file.path(folder_w_out, "rxfire_westinterior.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  thinc_tm,
  file.path(folder_w_out, "thin_westcoast.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  thini_tm,
  file.path(folder_w_out, "thin_westinterior.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)

### Mosaic & save --------------------------------------------------------------

#No overlap, can use faster merge() as long as use algo 2 (no resampling)
blc <- terra::merge(blc_tm, bli_tm, algo = 2)
burn <- terra::merge(burnc_tm, burni_tm, algo = 2)
thin <- terra::merge(thinc_tm, thini_tm, algo = 2)

terra::writeRaster(
  blc,
  file.path(folder_w_out, "undisturbed_west.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  burn,
  file.path(folder_w_out, "rxfire_west.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
terra::writeRaster(
  thin,
  file.path(folder_w_out, "thin_west.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
