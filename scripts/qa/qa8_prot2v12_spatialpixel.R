# QA
# Test to see if new protect1&2 run has the same pixels as
# # the original protect2-only bug run

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

stamp <- format(Sys.time(), "%Y%m%d")

folder_out <- file.path(
  "data",
  "output",
  "qa"
)
dir.create(folder_out, recursive = TRUE)


### Spatial non-overlap check --------------------------------------------------

# latest run output
folder_mos <- file.path("data", "output", "fvs_mosaic")

# new 20260729 run
new_file <- "Baseline_2026_aboveground_total_live.tif"
new <- terra::rast(file.path(folder_mos, "baseline", new_file))

old_file <- "Baseline_2026_aboveground_total_live_ORIGINALBAD.tif"
old <- terra::rast(file.path("data", "qa", old_file))

# Check counts
terra::global(new, "notNA")
terra::global(old, "notNA")
# new: 887732435. old: 887704721

# Create binary 1/0
new_bin <- terra::ifel(not.na(new), 1, 0)
old_bin <- terra::ifel(not.na(old), 1, 0)
# 1 where old but not new, -1 where new but not old
diff_bin <- new_bin - old_bin

diff_bin
# 1 -- pixels where old existed but not new.
# 27,714 fewer pixels.

freq(diff_bin) %>% tibble::as_tibble()
#   layer value      count
#   <dbl> <dbl>      <dbl>
# 1     1     0 5986014542
# 2     1     1      27714
