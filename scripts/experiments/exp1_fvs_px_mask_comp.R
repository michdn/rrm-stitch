# FVS pixel mask

# TODO Also plan for comparison pixel counts to FVS results.
#  years within scenario and across.

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

#regions to check, currently R2, R3, R5, and R6 are done
regions_to_run <- c("R2", "R3", "R5", "R6")

### Data in prep ---------------------------------------------------------------

# Treemap pixels in region
folder_tmr <- file.path("data", "region_treemap")
#files_tmr <- list.files(folder_tmr, pattern = "*\\.tif$", full.names = TRUE)

#FVS project folders
proj_stem <- "PC612_"

### Loop -----------------------------------------------------------------------

# for each region
# 1. Do a pixel count
#  - of each variable (5)
#  - of each year (5)
#  - of each scenario (3)
# 2. Compare against treemap file and see if matches

(start_time <- Sys.time())
collector <- vector(mode = "list", length = length(regions_to_run))
for (i in seq_along(regions_to_run)) {
  this_region <- regions_to_run[[i]]
  print(paste0("Starting region ", this_region, " at ", Sys.time()))

  ## treemap mask file
  this_tmr_file <- list.files(
    folder_tmr,
    pattern = paste0("^", proj_stem, this_region, "_treemap_5070.tif$"),
    full.names = TRUE,
    recursive = FALSE
  )
  #count of notNA pixels, will verify against results to see if the same count
  # if so, then I can use these for masking the water availability results
  this_tmr <- terra::rast(this_tmr_file)
  this_reg_nonna <- terra::global(this_tmr, "notNA")

  ## FVS result files
  this_folder <- file.path("data", paste0(proj_stem, this_region))

  #get all files, which will be all scenarios, variables, years. 75 files.
  this_files <- list.files(
    this_folder,
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  )

  #create a tibble of file names with metadata
  this_targets <- this_files %>%
    tibble::as_tibble() %>%
    dplyr::rename(full_file = value) %>%
    #pull pieces to get scenario, year, etc.
    dplyr::mutate(
      fname = tools::file_path_sans_ext(basename(full_file)),
      year = stringr::str_split_i(fname, "_", 4),
      variable = stringr::str_split_i(fname, "_[0-9]{4}_", 2),
      #don't need to pull from filename as it was search term
      region = this_region,
      #must be pulled from folder (as two treatments are both 'project')
      scenario = basename(dirname(full_file)),
      scenario = stringr::str_remove(scenario, "spat_"),
      #adding the the template notNA count
      notNA_template = .env$this_reg_nonna[["notNA"]]
    )

  #for each target, calc the count of nonNAs and add to tibble
  for (j in 1:nrow(this_targets)) {
    this_row <- this_targets[j, ]
    this_r <- terra::rast(this_row[["full_file"]])
    this_nonna <- terra::global(this_r, "notNA")
    this_targets[[j, "notNA_count"]] <- this_nonna[["notNA"]]

    # #exp dev
    # this_r_bin <- terra::ifel(!is.na(this_r), 1, 0)
    # #extents do not match!
    # #c(this_r_bin, this_tmr)
    # this_r_bin2 <- extend(this_r_bin, this_tmr)
    # this_tmr2 <- extend(this_tmr, this_r_bin2)
    # this_stack <- c(this_r_bin2, this_tmr2)
    # this_ct <- terra::crosstab(this_stack) %>% tibble::as_tibble()
    # this_ct_fmt <- this_ct %>%
    #   rename(
    #     atl = "PC612_R2_Baseline_2026_aboveground_total_live",
    #     tmr = "imputation_vrt"
    #   ) %>%
    #   mutate(atl = as.numeric(atl), tmr = as.numeric(tmr)) %>%
    #   dplyr::filter(atl == 0) %>%
    #   arrange(desc(n))
    # readr::write_csv(
    #   this_ct_fmt,
    #   file.path("data", "qa", "R2_2026_atl_vs_treemap.csv")
    # )
  } #end j file targets

  collector[[i]] <- this_targets
} # end i region
(end_time <- Sys.time())
(end_time - start_time)
#1.6 hours

px_qa <- dplyr::bind_rows(collector) %>%
  #after collection, calc any differences in counts
  dplyr::mutate(diff = notNA_count - notNA_template)

readr::write_csv(
  px_qa,
  file.path("data", "qa", "pixels_regions_fvs_templates.csv")
)

px_qa %>%
  dplyr::filter(!diff == 0)
#dplyr::summarize(count = n(), .by = c(region, scenario, year)) %>%

px_qa %>%
  dplyr::select(region, diff) %>%
  dplyr::distinct()
