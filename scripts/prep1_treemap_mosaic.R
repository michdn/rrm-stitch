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
files_tmr <- list.files(
  folder_tmr,
  pattern = "PC612_R[0-9]?_treemap_5070\\.tif$",
  full.names = TRUE
)

### Mosaic ---------------------------------------------------------------------

# Region 5 has very large extent, (covering Hawaii?),
#  want to trim off NAs in that direction, but cannot use trim(trim_sprc)
#  because region 3 rasters have NA rows at bottom, so need to keep that
#  to prevent extent from being too small and not matching later in processing.
# sprc are not subsettable, so must handle before sprc()

r5_raw <- files_tmr %>% grep(pattern = "R5", value = TRUE) %>% terra::rast()
r5 <- trim(r5_raw, padding = 1, value = NA)

# spatrastercollection is very difficult to manipulate
#  choices were to do two merges (?) or handle individual rasters
r1 <- files_tmr %>% grep(pattern = "R1", value = TRUE) %>% terra::rast()
r2 <- files_tmr %>% grep(pattern = "R2", value = TRUE) %>% terra::rast()
r3 <- files_tmr %>% grep(pattern = "R3", value = TRUE) %>% terra::rast()
r4 <- files_tmr %>% grep(pattern = "R4", value = TRUE) %>% terra::rast()
r6 <- files_tmr %>% grep(pattern = "R6", value = TRUE) %>% terra::rast()

#tmr_sprc <- terra::sprc(files_tmr)
tmr_sprc <- terra::sprc(r1, r2, r3, r4, r5, r6)

#Values do not matter, so do not need to worry about overlaps if they even exist,
# can just fast merge algo=2. They have same origin, resolution, CRS, etc.
tmr_raw <- terra::merge(tmr_sprc, algo = 2)

#make binary version
tmr1 <- terra::ifel(!is.na(tmr_raw), 1, NA)

names(tmr1) <- "treemap_coverage"
varnames(tmr1) <- "treemap_coverage"

#save out
terra::writeRaster(
  tmr1,
  file.path(folder_tmr, "mosaic_r1-r6_treemap_mask.tif")
)
