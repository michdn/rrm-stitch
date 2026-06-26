# Calculate non-merchantable cubic ft vol
# Total(tcuft) minus merchantable(mcuft)

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

### Data in --------------------------------------------------------------------

folder_mosbl <- file.path("data", "output", "fvs_mosaic", "baseline")

tcuft <- terra::rast(file.path(folder_mosbl, "Baseline_2026_tcuft.tif"))
mcuft <- terra::rast(file.path(folder_mosbl, "Baseline_2026_mcuft.tif"))

### Calculate ------------------------------------------------------------------

nonmerch <- tcuft - mcuft

# #some negative values...
# nm_neg <- terra::ifel(nonmerch < 0, nonmerch, NA)
# terra::writeRaster(
#   nm_neg,
#   file.path(
#     "data",
#     "qa",
#     "nonmerch_negative.tif"
#   ),
#   gdal = c("COMPRESS = DEFLATE"),
#   overwrite = TRUE
# )
# terra::global(nm_neg, c("notNA"))
# #                     notNA
# # Baseline_2026_tcuft 92209
# terra::global(nonmerch, c("notNA"))
# #                         notNA
# # Baseline_2026_tcuft 887704721
# 92209 / 887704721 * 100
# # [1] 0.01038735

# Solution for MVP: Clamp negative values to 0
nm_clamp <- terra::clamp(nonmerch, lower = 0, upper = Inf, values = TRUE)

### Save out -------------------------------------------------------------------

name_out <- "Baseline_2026_nonmerch_cuft"
names(nm_clamp) <- name_out
varnames(nm_clamp) <- name_out

terra::writeRaster(
  nm_clamp,
  file.path(
    folder_mosbl,
    paste0(name_out, ".tif")
  ),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
