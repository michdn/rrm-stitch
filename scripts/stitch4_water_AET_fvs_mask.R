# Masks water availability with FVS-results pixels
# i.e. only reporting water results where we are reporting FVS results

# Edited to add legalmax and baseline rasters (in addition to difference) by request

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

#all pixel water AET
folder_wa <- file.path(
  "data",
  "output",
  "water_availability"
)
aet_diff <- terra::rast(file.path(
  folder_wa,
  "AET_legalmax_difference_allpixels.tif"
))
aet_lm <- terra::rast(file.path(
  folder_wa,
  "AET_legalmax_allpixels.tif"
))

# Processed water AET rasters
# Pixel-aligned & mosaicked
folder_aet <- file.path("data", "water_availability", "AET_processed")
aet_bl <- terra::rast(file.path(folder_aet, "undisturbed_west.tif"))

#Where final treated & baseline FVS rasters live
# To use as a mask
folder_mos <- file.path("data", "output", "fvs_mosaic")
# A final FVS final result to use
fvs_file <- "Baseline_2026_aboveground_total_live.tif"
fvs <- terra::rast(file.path(folder_mos, "baseline", fvs_file))

# Create binary FVS-result mask
fvsbin <- terra::ifel(!is.na(fvs), 1, NA)

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

### Extents & Masking ----------------------------------------------------------

crop_extend_mask_aet <- function(this_aet, fvs_eg) {
  # must be crop and then extend in this order
  # to match up extents
  step1 <- terra::crop(this_aet, fvs_eg)
  step2 <- terra::extend(step1, fvs_eg)
  #apply fvs mask
  step3 <- terra::mask(step2, fvs_eg, maskvalues = NA)
}

aet_diff_ce <- crop_extend_mask_aet(this_aet = aet_diff, fvs_eg = fvsbin)
aet_lm_ce <- crop_extend_mask_aet(aet_lm, fvsbin)
aet_bl_ce <- crop_extend_mask_aet(aet_bl, fvsbin)

#save masked raster
save_aet <- function(this_aet, this_varname) {
  varnames(this_aet) <- this_varname
  this_filename <- paste0(this_varname, "_fvsmasked.tif")
  terra::writeRaster(
    this_aet,
    file.path(
      folder_wa,
      this_filename
    ),
    gdal = c("COMPRESS = DEFLATE"),
    overwrite = TRUE
  )
}

save_aet(aet_diff_ce, "AET_legalmax_difference")
save_aet(aet_lm_ce, "AET_legalmax")
save_aet(aet_bl_ce, "AET_baseline")
