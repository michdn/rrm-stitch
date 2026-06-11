# Recreating stitching logic to create a treatment guide
#  Ideally would have been done as part of stitch4 script,
#   but was requested after stitching.
#  If you are coming back to this, ENSURE that the logic still MATCHES!

# Plan:
#  Use full size protected raster, create full size all pixel key.
#  Then mask to data pixels (use one of the final results as in the water mask)

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

# Rasterized protected areas, status 1 or 2
# Pixel aligned to treemap/FVS
pad12_r <- terra::rast(file.path("data", "protected", "protected_status12.tif"))

# Need thin guide? Important WHICH thin treatment?

# For masking:
# Where final treated & baseline rasters live
folder_mos <- file.path("data", "output", "fvs_mosaic")
# A final FVS final result to use
fvs_file <- "Baseline_2026_aboveground_total_live.tif"
fvs <- terra::rast(file.path(folder_mos, "baseline", fvs_file))

### Legalmax logic -------------------------------------------------------------

# logic copy
#       this_legalmax <- terra::ifel(this_pad %in% c(1, 2), this_burn, this_thin)

# mask, and pixel count to confirm.
