# Creating binary FVS-result-treatment mask
#  successful FVS result = 1 (forest, basically, but could be missing pixels)
#  no FVS result = 0 (non-forest)

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

# Where final treated & baseline rasters live
folder_mos <- file.path("data", "output", "fvs_mosaic")
# A final FVS final result to use
fvs_file <- "Baseline_2026_aboveground_total_live.tif"
fvs <- terra::rast(file.path(folder_mos, "baseline", fvs_file))

### Binary ---------------------------------------------------------------------

fvs_bin <- terra::ifel(not.na(fvs), 1, 0)
names(fvs_bin) <- "FVS_result_forest_binary"
varnames(fvs_bin) <- "FVS_result_forest_binary"

terra::writeRaster(
  fvs_bin,
  file.path(
    "data",
    "output",
    "FVS_forest_binary.tif"
  ),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)

#check. for 20260729 runs, should be 887732435 (value 1)
bin_freq <- terra::freq(fvs_bin)

#   layer value      count
# 1     1     0 5098309821
# 2     1     1  887732435
