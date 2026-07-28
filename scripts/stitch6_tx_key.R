# Creating a treatment key raster

# Ideally would have been created during stitching script (stitch4),
#  so double-check that logic remains the same if coming back to this or stitching.

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

#prescribed fire
burn_value <- 1
#thin + pile burn treatment
thin_value <- 2

### Data in --------------------------------------------------------------------

# Rasterized protected areas, status 1 or 2
# Pixel aligned to treemap/FVS
pad12_r <- terra::rast(file.path(
  "data",
  "protected",
  "protected_status12_20260727.tif"
))

# Final mask
# Where final treated & baseline rasters live
folder_mos <- file.path("data", "output", "fvs_mosaic")
# A final FVS final result to use
fvs_file <- "Baseline_2026_aboveground_total_live.tif"
fvs <- terra::rast(file.path(folder_mos, "baseline", fvs_file))

# Create binary FVS-result mask
fvsbin <- terra::ifel(not.na(fvs), 1, NA)

### Tx raster key --------------------------------------------------------------

# Crop and then extend in this order
# to match up extents
pad12_r1 <- terra::crop(pad12_r, fvsbin)
pad12_r2 <- terra::extend(pad12_r1, fvsbin)
all.equal(ext(pad12_r2), ext(fvsbin))

# copied logic
#       this_legalmax <- terra::ifel(this_pad %in% c(1, 2), this_burn, this_thin)

# protected = rx burn, otherwise thin
tx_pad <- terra::ifel(pad12_r2 %in% c(1, 2), burn_value, thin_value)
tx_pad
plot(pad12_r2)
plot(tx_pad)

# limit to final FVS results
tx_key <- terra::mask(tx_pad, fvsbin, maskvalues = NA)
plot(tx_key)
tx_key

names(tx_key) <- "tx_key"
varnames(tx_key) <- "tx_key"

terra::writeRaster(
  tx_key,
  file.path(
    "data",
    "output",
    paste0("tx_key_rxfire", burn_value, "_thinburn", thin_value, ".tif")
  ),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
