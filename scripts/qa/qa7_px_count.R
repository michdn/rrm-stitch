# data pixel count

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

file_pattern <- ("Legalmax|Baseline")

stamp <- format(Sys.time(), "%Y%m%d")

folder_out <- file.path(
  "data",
  "qa"
)
dir.create(folder_out, recursive = TRUE)

### Data in --------------------------------------------------------------------

folder_mos <- file.path("data", "output", "fvs_mosaic")

target_files <- list.files(
  path = c(
    file.path(folder_mos, "baseline"),
    file.path(folder_mos, "legalmax")
  ),
  #path = folder_mos,
  pattern = paste0(file_pattern, ".*\\.tif$"),
  recursive = TRUE,
  full.names = TRUE
)

#Next time remove subfolder 'legalmax_extrapixels'. -- Done with new paths

### Target & loop for stats ----------------------------------------------------

targets <- target_files %>%
  tibble::as_tibble() %>%
  dplyr::rename(fullpath = value) %>%
  dplyr::mutate(
    filenm = tools::file_path_sans_ext(basename(fullpath)),
    scenario = stringr::str_split_i(filenm, "_", 1),
    year = stringr::str_split_i(filenm, "_", 2),
    fvsvar = stringr::str_split_i(filenm, "_[0-9]{4}_", 2)
  ) %>%
  dplyr::select(-filenm)

stat_collector <- vector(length = nrow(targets), "list")

for (i in 1:nrow(targets)) {
  print(paste0("Starting ", i, " of ", nrow(targets), " at ", Sys.time()))

  this_row <- targets[i, ]
  this_rast <- terra::rast(this_row[["fullpath"]])

  #global min, mean, max, notNA.
  this_stats <- terra::global(
    this_rast,
    c("notNA")
  ) %>%
    tibble::as_tibble()

  #all data for output
  stat_collector[[i]] <- cbind(this_row, this_stats)
}

(all_stats <- dplyr::bind_rows(stat_collector) %>%
  dplyr::arrange(notNA, scenario, fvsvar, year))

readr::write_csv(
  all_stats,
  file.path(
    folder_out,
    paste0("notna_stats", stamp, ".csv")
  )
)

# Tt does seem that Legalmax has more nonNA pixels than Baseline.
# The masking against each other in the stitching was intended to have
#  prevented this issue.
# However, it was saving out the previous step. Fixed now.
# The AET, hardwood/softwood layers were all masked with baseline, so that
#  should be all fine as well.

### Spatial non-overlap check --------------------------------------------------

# final FVS final result to use
bl_file <- "Baseline_2026_aboveground_total_live.tif"
bl <- terra::rast(file.path(folder_mos, "baseline", bl_file))
lm_file <- "Legalmax_2026_aboveground_total_live.tif"
lm <- terra::rast(file.path(folder_mos, "legalmax", lm_file))

# Check counts
terra::global(bl, "notNA")
terra::global(lm, "notNA")
# post fix:  887704721 and 887704721!

# Create binary 1/0
bl_bin <- terra::ifel(not.na(bl), 1, 0)
lm_bin <- terra::ifel(not.na(lm), 1, 0)
# 1 where legalmax but not baseline, -1 where baseline but not legalmax
diff_bin <- lm_bin - bl_bin
# ONLY 1! Legalmax has 'extra' pixels. From original run.

freq_diff <- terra::freq(diff_bin) %>%
  tibble::as_tibble() %>%
  dplyr::select(-layer)
#   value      count
#   <dbl>      <dbl>
#      0 5985222429
#      1     819827

819827 / 888524548 * 100
819827 / 887704721 * 100
# 0.09% extra pixels

#post-fix
freq_diff
# # A tibble: 1 x 2
#   value      count
#   <dbl>      <dbl>
# 1     0 5986042256

### Spot check -----------------------------------------------------------------

# region result

blr_file <- "PC612_R6_baseline_2026_cc.tif"
blr <- terra::rast(file.path(
  "data",
  "output",
  "fvs_region",
  "fvs_baseline",
  "PC612_R6",
  blr_file
))
lmr_file <- "PC612_R6_legalmax_2026_cc.tif"
lmr <- terra::rast(file.path(
  "data",
  "output",
  "fvs_region",
  "fvs_legalmax",
  "PC612_R6",
  lmr_file
))

terra::global(blr, "notNA")
terra::global(lmr, "notNA")

# fix check
#                               notNA
# PC612_R6_baseline_2026_cc 207525152
#                               notNA
# PC612_R6_legalmax_2026_cc 207525152
# Good!
