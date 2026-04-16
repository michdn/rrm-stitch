# Stitching rule raster

# Stitch only pixels included in TreeMap
# Pixels in PAD 1 or 2 = Burn only
# All other TreeMap pixels = Thinburn

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

folder_out <- file.path("data", "output", "water_availability")
dir.create(folder_out, recursive = TRUE)


### Data in --------------------------------------------------------------------

# Rasterized protected areas, status 1 or 2
# Pixel aligned to treemap/FVS
pad12_r <- terra::rast(file.path("data", "protected", "protected_status12.tif"))

# Processed water availabily rasters
# Pixel-aligned & mosaicked
folder_water <- file.path("data", "water_availability", "AET_processed")
blc <- terra::rast(file.path(folder_water, "undisturbed_west.tif"))
burn <- terra::rast(file.path(folder_water, "rxfire_west.tif"))
thin <- terra::rast(file.path(folder_water, "thin_west.tif"))

### 'Stitching' ----------------------------------------------------------------

# Very easy rules for creating melded/stitched treatment scenario raster
# 1. Pixels in PAD 1 or 2 = Burn only
# 2. All other TreeMap pixels = Thinburn
# (3. Stitch only pixels included in TreeMap) -- handled later
# https://docs.google.com/document/d/1lZ2hI0b4OIg3ZvOv_HpNFt-vAHgfXCfJ-wAojONx4-A/edit?usp=sharing

treat1 <- terra::ifel(pad12_r %in% c(1, 2), burn, thin)

varnames(treat1) <- "AET_treatmax"
names(treat1) <- "AET_treatmax"

terra::writeRaster(
  treat1,
  file.path(folder_out, "AET_legalmax_allpixels.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)

#dev load
#treat1 <- terra::rast(file.path(folder_out, "AET_legalmax_allpixels.tif"))

#difference raster (treated versus untreated/baseline)
