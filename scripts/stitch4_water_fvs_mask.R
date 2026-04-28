# Masks water availability with FVS-results pixels
# i.e. only reporting water results where we are reporting FVS results

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

#all pixel water availability
folder_wa <- file.path(
  "data",
  "output",
  "water_availability"
)
aet <- terra::rast(file.path(
  folder_wa,
  "AET_legalmax_difference_allpixels.tif"
))

#Where the final FVS difference (treated & baseline) rasters live
folder_mos <- file.path("data", "output", "fvs_mosaic")
# FVS final result to use
fvs_file <- "Difference_2026_aboveground_total_live.tif"

fvs <- terra::rast(file.path(folder_mos, fvs_file))

### Extents --------------------------------------------------------------------

# ext(aet)
# #SpatExtent : -2356110, 121860, 991800, 3165900 (xmin, xmax, ymin, ymax)
# ext(fvsbin)
# #SpatExtent : -2356110, 121830, 991710, 3165870 (xmin, xmax, ymin, ymax)
# aet_trim <- trim(aet)
# fvsbin_trim <- trim(fvsbin)
# ext(aet_trim)
# #SpatExtent : -2356110, -15570, 991800, 3165900 (xmin, xmax, ymin, ymax)
# ext(fvsbin_trim)
# #SpatExtent : -2356080, -35520, 993720, 3165690 (xmin, xmax, ymin, ymax)

# folder_tmr <- file.path("data", "region_treemap")
# tmr1 <- terra::rast(file.path(folder_tmr, "mosaic_r1-r6_treemap_mask.tif"))
# ext(tmr1)
# #SpatExtent : -2356110, 121860, 991800, 3165900 (xmin, xmax, ymin, ymax)

# AET data extends further west (ocean?), east, and north
#  - AET will to cropped to FVS results
# AET data is within the southern edge of FVS data, but FVS raster has
#  more NA/empty rows at bottom.
#  - AET will be expanded to match FVS result

aet_1crop <- terra::crop(aet, fvs)
aet_2exd <- terra::extend(aet_1crop, fvs)

### Masking ----------------------------------------------------------------

# Create binary FVS-result mask
fvsbin <- terra::ifel(!is.na(fvs), 1, NA)

# Apply mask
aet_masked <- terra::mask(aet_2exd, fvsbin, maskvalues = NA)

# Save final out
varnames(aet_masked) <- "AET_legalmax_difference"
terra::writeRaster(
  aet_masked,
  file.path(
    folder_wa,
    paste0("AET_legalmax_difference_fvsmasked.tif")
  ),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
