# qa
# creating percent diff rasters for 3 fvs vars

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra,
  viridis
)

### Settings -------------------------------------------------------------------

# FVS variables to run
vars_to_run <- c(
  "aboveground_total_live",
  "pot_smoke_sev",
  "tot_flame_sev"
)

# years to run
years_to_run <- c(2026, 2031, 2036, 2041, 2046)

# output folder
folder_out <- file.path("data", "output", "fvs_mosaic", "perc_change")
dir.create(folder_out)

### Data in --------------------------------------------------------------------

folder_base <- file.path("data", "output", "fvs_mosaic", "baseline")
folder_legalmax <- file.path("data", "output", "fvs_mosaic", "legalmax")

files_bl <- list.files(
  path = folder_base,
  pattern = "Baseline_.*\\.tif$",
  recursive = FALSE,
  full.names = TRUE
)

files_lm <- list.files(
  path = folder_lm,
  pattern = "Legalmax_.*\\.tif$",
  recursive = FALSE,
  full.names = TRUE
)


### Targets --------------------------------------------------------------------

make_target_tbl <- function(file_list) {
  file_list %>%
    tibble::as_tibble() %>%
    dplyr::rename(fullpath = value) %>%
    dplyr::mutate(
      filenm = tools::file_path_sans_ext(basename(fullpath)),
      year = stringr::str_split_i(filenm, "_", 2),
      fvsvar = stringr::str_split_i(filenm, "_[0-9]{4}_", 2)
    ) %>%
    dplyr::select(-filenm) %>%
    #only fvs variables, year requested
    dplyr::filter(
      fvsvar %in% vars_to_run,
      year %in% years_to_run
    )
}

target_bl <- make_target_tbl(files_bl) %>%
  dplyr::rename(fullpath_bl = fullpath)
target_lm <- make_target_tbl(files_lm) %>%
  dplyr::rename(fullpath_lm = fullpath)

if (!nrow(target_bl) == nrow(target_lm)) {
  stop("Mismatched number of rasters between baseline and legalmax")
}

targets <- dplyr::inner_join(target_lm, target_bl, by = join_by(year, fvsvar))

if (!nrow(targets) == nrow(target_bl)) {
  stop("Mismatched fvs variable & years between baseline and legalmax")
}

for (i in 1:nrow(targets)) {
  this_row <- targets[i, ]
  this_bl <- terra::rast(this_row[["fullpath_bl"]])
  this_lm <- terra::rast(this_row[["fullpath_lm"]])

  #percent change
  this_percchange <- (this_lm - this_bl) / this_bl * 100

  this_out_name <- paste0(
    this_projreg_name,
    "_percchange_",
    this_year,
    "_",
    this_var
  )
  names(this_percchange) <- this_out_name
  varnames(this_percchange) <- this_out_name
  terra::writeRaster(
    this_percchange,
    file.path(
      folder_out,
      paste0(this_out_name, ".tif")
    ),
    gdal = c("COMPRESS = DEFLATE"),
    overwrite = TRUE
  )
}
