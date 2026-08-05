# Zonal extracts using WCS regions
# 2026-07-31 data & which data
# Requires fvs perc change to be run first
# probably followed by graphing wcs stats

# PART 1
# FVS results (ATL, pot_smoke, tot flame) percent change
# Mean, min, max, 25th percentile, 50th, 75th
# 5 years

# PART 2
# FVS results merchantable total biomass
# nonmerch cuft, merch bdft
# Min, max, 25th, 50th, 75th
# Only 2026

# PART 3
# Water results
# acres and % of area with increase (positive water avail percent change)
# single raster, single 'year'

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra,
  exactextractr
)

### Settings -------------------------------------------------------------------

#preventing scientific notation
options(scipen = 999)

# output folder
folder_out <- file.path("data", "qa", "qa_zonal")
dir.create(folder_out, recursive = TRUE)

stamp <- format(Sys.time(), "%Y%m%d")

## Part 1 settings ----
# FVS variables to run
vars_to_run_p1 <- c(
  "aboveground_total_live",
  "pot_smoke_sev",
  "tot_flame_sev"
)
# FVS years to run
years_to_run_p1 <- c(2026, 2031, 2036, 2041, 2046)

## Part 2 settings ----
vars_to_run_p2 <- c(
  "nonmerch_cuft",
  "bdft"
)
years_to_run_p2 <- c(2026)

### Functions ------------------------------------------------------------------

calc_acres_from_pixels <- function(pxs, size_m = 30) {
  #size_m: pixel size in meters (e.g. 30 m)
  pxs *
    size_m *
    size_m %>%
      units::set_units("m^2") %>%
      units::set_units("acres") %>%
      units::drop_units()
}

### Data in (all parts) --------------------------------------------------------

#WCS priority landscapes, aka our test regions
lands <- sf::read_sf(file.path(
  "data",
  "qa",
  "wcs_landscapes",
  "WCS Boundaries(Klamath Split)",
  "WCS Boundaries(Klamath Split).shp"
))

#lands (get total area)
lands_area <- lands %>%
  dplyr::select(NAME_SHORT) %>%
  dplyr::mutate(
    area_total = sf::st_area(geometry),
    acres_total = units::set_units(area_total, "acres") %>%
      units::drop_units()
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-area_total)


### Part 1 ---------------------------------------------------------------------

## Data in ---
folder_pc <- file.path("data", "output", "fvs_mosaic", "perc_change")

files_pc <- list.files(
  path = folder_pc,
  #all files for the moment, will filter later
  pattern = "PercentChange_.*\\.tif$",
  full.names = TRUE
)

# Create and filter target rasters for zonal extraction
targets_p1 <- files_pc %>%
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
    fvsvar %in% vars_to_run_p1,
    year %in% years_to_run_p1
  )

collector_p1 <- vector(mode = "list", length = nrow(targets_p1))

(time_start <- Sys.time())
for (i in 1:nrow(targets_p1)) {
  print(paste(i, "of", nrow(targets_p1), "at", Sys.time()))

  this_target <- targets_p1[i, ]

  this_rast <- terra::rast(this_target[["fullpath"]])

  #extract (exact) the statistics
  this_extract <- exactextractr::exact_extract(
    this_rast,
    lands,
    #count gets sum of fractions of raster cells with non-NA values
    #NOTE: exact_extract ignores NAs by default
    fun = c("quantile", "min", "max", "mean", "count"),
    quantiles = c(0.25, 0.50, 0.75),
    append_cols = c("NAME_SHORT")
  )

  #format results (wide by stat)
  this_result <- this_target %>%
    dplyr::select(fvsvar, year) %>%
    cbind(this_extract)

  #collect
  collector_p1[[i]] <- this_result
} #end loop
(time_end <- Sys.time())
(time_end - time_start)

#bind all results
stats_p1 <- dplyr::bind_rows(collector_p1) %>%
  dplyr::as_tibble() %>%
  #convert count to acres
  dplyr::mutate(acres_fvs = calc_acres_from_pixels(count, size_m = 30)) %>%
  dplyr::left_join(lands_area, by = join_by("NAME_SHORT")) %>%
  dplyr::mutate(fvs_perc_area = acres_fvs / acres_total * 100) %>%
  # arrange for sheet matrix
  dplyr::select(
    NAME_SHORT,
    fvsvar,
    year,
    mean,
    min,
    max,
    q25,
    q50,
    q75,
    fvs_perc_area,
    everything()
  ) %>%
  dplyr::arrange(NAME_SHORT, fvsvar, year)

#very strange issue with sci notation appearing in q50 and q75 despite scipens
# using write.csv
options(scipen = 999)
write.csv(
  stats_p1,
  file.path(
    folder_out,
    paste0("part1_fvs_percchange_stats_", stamp, ".csv")
  )
)


### Part 2 ---------------------------------------------------------------------

## Data in ---
folder_bl <- file.path("data", "output", "fvs_mosaic", "baseline")

files_bl <- list.files(
  path = folder_bl,
  #all files for the moment, will filter later
  pattern = "Baseline_.*\\.tif$",
  full.names = TRUE
)

# Create and filter target rasters for zonal extraction
targets_p2 <- files_bl %>%
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
    fvsvar %in% vars_to_run_p2,
    year %in% years_to_run_p2
  )

collector_p2 <- vector(mode = "list", length = nrow(targets_p2))

(time_start <- Sys.time())
for (i in 1:nrow(targets_p2)) {
  print(paste(i, "of", nrow(targets_p2), "at", Sys.time()))

  this_target <- targets_p2[i, ]

  this_rast <- terra::rast(this_target[["fullpath"]])

  #extract (exact) the statistics
  this_extract <- exactextractr::exact_extract(
    this_rast,
    lands,
    #NOTE: exact_extract ignores NAs by default
    fun = c("quantile", "min", "max", "mean"),
    quantiles = c(0.25, 0.50, 0.75),
    append_cols = c("NAME_SHORT")
  )

  #format results (wide by stat)
  this_result <- this_target %>%
    dplyr::select(fvsvar, year) %>%
    cbind(this_extract)

  #collect
  collector_p2[[i]] <- this_result
} #end loop
(time_end <- Sys.time())
(time_end - time_start)

#bind all results
stats_p2 <- dplyr::bind_rows(collector_p2) %>%
  dplyr::as_tibble() %>%
  dplyr::select(NAME_SHORT, fvsvar, year, min, max, q25, q50, q75) %>%
  dplyr::arrange(NAME_SHORT, fvsvar)

readr::write_csv(
  stats_p2,
  file.path(
    folder_out,
    paste0("part2_fvs_biomass_stats_", stamp, ".csv")
  )
)


### Part 3 ---------------------------------------------------------------------

## Data in ---
folder_wa <- file.path("data", "output", "water_availability")
wapc <- terra::rast(file.path(
  folder_wa,
  "water_avail_perc_change_legalmax_fvsmasked.tif"
))

# #extract (exact) the statistics
stats_p3_wapc <- exactextractr::exact_extract(
  wapc,
  lands,
  #NOTE: exact_extract ignores NAs by default
  fun = c("quantile", "min", "max", "mean"),
  quantiles = c(0.25, 0.50, 0.75),
  append_cols = c("NAME_SHORT")
) %>%
  tibble::as_tibble()


#Round percent change to nearest tens (not tenths, tens!)
wapc_rnd <- round(wapc, digits = -1)

stats_p3_wa <- exactextractr::exact_extract(
  wapc_rnd,
  lands,
  append_cols = c("NAME_SHORT"),
  fun = c("frac", "count")
) %>%
  tibble::as_tibble()

stats_p3_wa2 <- stats_p3_wa %>%
  # exact extract doesn't have a freq() function,
  #  but can multiply fraction & total count
  mutate(across(
    starts_with("frac"),
    function(x) x * count,
    .names = "freq_{.col}"
  )) %>%
  #clean up names from freq_frac_n to just freq_n
  dplyr::rename_with(function(n) sub("_frac_", "_", n)) %>%
  dplyr::select(c(NAME_SHORT, starts_with("freq_"))) %>%
  #convert pixel counts to area
  dplyr::mutate(across(
    .cols = starts_with("freq"),
    .fns = calc_acres_from_pixels,
    .names = "acres_{.col}"
  )) %>%
  dplyr::select(-c(starts_with("freq_"))) %>%
  dplyr::rename_with(function(n) sub("_freq", "", n)) %>%
  #and a sum of acres
  dplyr::rowwise() %>%
  dplyr::mutate(acres_inc = sum(c_across(starts_with("acres_")))) %>%
  dplyr::ungroup() %>%
  #total area and percentage
  dplyr::left_join(lands_area, by = join_by(NAME_SHORT)) %>%
  dplyr::mutate(
    perc_area_inc = acres_inc / acres_total * 100
  )

# on the quantiles and stats, rename for percent change (pc)
stats_p3_wapc2 <- stats_p3_wapc %>%
  dplyr::rename_with(~ paste0("pc_", .), -NAME_SHORT) %>%
  dplyr::select(NAME_SHORT, pc_mean, pc_min, pc_max, everything())

#all the statistics together to figure out what to show later
stats_p3 <- stats_p3_wa2 %>%
  dplyr::left_join(stats_p3_wapc2, by = join_by(NAME_SHORT))

readr::write_csv(
  stats_p3,
  file.path(
    folder_out,
    paste0("part3_waterpchange_stats_", stamp, ".csv")
  )
)
