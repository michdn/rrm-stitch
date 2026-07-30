# Generating a percent change of water availability (inverse AET)
#  per request

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

# masked AETs
folder_wa <- file.path(
  "data",
  "output",
  "water_availability"
)
# Difference: Treated minus baseline
# where AET difference is NEGATIVE means INCREASE in water availability
aet_diff <- terra::rast(file.path(
  folder_wa,
  "AET_legalmax_difference_fvsmasked.tif"
))
aet_bl <- terra::rast(file.path(
  folder_wa,
  "AET_baseline_fvsmasked.tif"
))

### Calculate ------------------------------------------------------------------

# percent change is difference / baseline * 100
# inverting (positive is an INCREASE in water availability) by multiply by -1

pc_water <- (-1 * aet_diff) / aet_bl * 100

#freq table, rounded to tens digit
pcw_freq <- terra::freq(pc_water, digits = -1)
pcw_freq %>% tibble::as_tibble() %>% dplyr::select(-layer)
# # A tibble: 17 × 2
#    value     count
#    <dbl>     <dbl>
#  1  -240         1
#  2   -40         1
#  3   -30         3
#  4   -20         3
#  5   -10         4
#  6     0  42852965
#  7    10  71300289
#  8    20 409391200
#  9    30 361622077
# 10    40   2122839
# 11    50     24601
# 12    60        68
# 13    70         1
# 14    80         1
# 15   120         1
# 16   130         1
# 17   190         1

# ORIGINAL ProtectStatus2only bug:
# # A tibble: 16 x 2
#    value     count
#    <dbl>     <dbl>
#  1  -240         1
#  2   -40         1
#  3   -30         2
#  4   -20         3
#  5   -10         4
#  6     0   8909967
#  7    10  32882480
#  8    20 439882006
#  9    30 403375219
# 10    40   2215860
# 11    50     20769
# 12    60        31
# 13    70         1
# 14    80         1
# 15   120         1
# 16   190         1

save_r <- function(this_r, this_varname) {
  varnames(this_r) <- this_varname
  names(this_r) <- this_varname #forgot this step in initial run
  this_filename <- paste0(this_varname, "_fvsmasked.tif")
  terra::writeRaster(
    this_r,
    file.path(
      folder_wa,
      this_filename
    ),
    gdal = c("COMPRESS = DEFLATE"),
    overwrite = TRUE
  )
}

save_r(pc_water, "water_avail_perc_change_legalmax")
