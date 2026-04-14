#Create mosaicked treemap pixels of all regions
# Used for template to reproject water data.
# Used for template to rasterize protected areas.

# NOT used for masking water availability pixels.
# A new mask will be generated for that.
# This mask is the maximum possible (all FVS stands), however
#  not all stands have results.
# A mask for that will be generated from FVS results directly.

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

# Treemap pixels in region
folder_tmr <- file.path("data", "region_treemap")
files_tmr <- list.files(folder_tmr, pattern = "*\\.tif$", full.names = TRUE)

### Mosaic ---------------------------------------------------------------------

tmr_sprc <- terra::sprc(files_tmr)

#R5 has very large extent to the west (Hawaii?) but no values
# (and we are only doing CONUS), so will need to trim

#Values do not matter, so do not need to worry about overlaps if they even exist,
# can just fast merge algo=2. They have same origin, resolution, CRS, etc.
tmr_raw <- terra::merge(tmr_sprc, algo = 2)
tmr <- terra::trim(tmr_raw, padding = 1, value = NA)

#make binary version
tmr1 <- terra::ifel(!is.na(tmr), 1, NA)

#save out both (though mostly interested in binary tmr1)
terra::writeRaster(
  tmr1,
  file.path(folder_tmr, "mosaic_r1-r6_treemap_mask.tif")
)
terra::writeRaster(
  tmr,
  file.path(folder_tmr, "mosaic_r1-r6_treemap_rawvalues.tif")
)
