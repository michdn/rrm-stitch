# Special for bdft, as it will ONLY have baseline
#  so cannot use stitch2_fvs_withbaseline.R script
#  (While for tcuft and mcuft used stitch2 b/c we had txs
#   before we knew we didn't need them.)

# This script is basically stitch2 for only baseline
#  created for: bdft baseline (2026)
#  but modular to run for any var, year, region

# It is likely that stitch3 mosaic will be run after this script.

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

#vars to run, just in case we need to use this for any other special vars
vars_to_run <- c("bdft", "mcuft", "tcuft")
years_to_run <- c(2026)
regions_to_run <- c(1:6)

# var to use for masking
# Since we aren't creating legalmax, we need to pull some prior completed results
#  to mask to the same pixels. REGION-level results
eg_mask_var <- "aboveground_total_live"

#timestamp
stamp <- format(Sys.time(), "%Y%m%d%H%M")

#output folders
folder_out_base <- file.path("data", "output", "fvs_region")
folder_logs <- file.path(folder_out_base, "logs")
dir.create(folder_logs, recursive = TRUE)

#set up logging
log_setup <- tibble::tribble(
  ~var_name          , ~var_value                                 ,
  "regions_to_run"   , paste0(regions_to_run, "", collapse = ",") ,
  "variables_to_run" , paste0(vars_to_run, "", collapse = ",")    ,
  "years_to_run"     , paste0(years_to_run, "", collapse = ",")   ,
  "timestamp"        , stamp
)
readr::write_csv(
  log_setup,
  file = file.path(folder_logs, paste0("log_stitch2b_", stamp, "_setup.csv"))
)

### Data in --------------------------------------------------------------------

# folder where the FVS results have been copied into from rem share
folder_fvs <- file.path("data", "fvs_results")

# already completed results (that WERE masked against legalmax)
#  to pull rasters for masking data pixels
folder_regbl <- file.path("data", "output", "fvs_region", "fvs_baseline")

### Loop per region ------------------------------------------------------------

#total number of regions - variables - years that will be run
total_layer_count <- length(regions_to_run) *
  length(vars_to_run) *
  length(years_to_run)

#dedicated indexer for triple nested loop
idx <- 0 #start at 0, will be adding 1 before first write
log_collector <- vector("list", length = total_layer_count)

#triple loop
start_time <- Sys.time()
for (i in seq_along(regions_to_run)) {
  this_reg <- regions_to_run[[i]]
  #used in multiple places for finding folders and files
  this_projreg_name <- paste0("PC612_R", this_reg)

  #log and update to console
  start_time_region <- Sys.time()
  print(paste0("Starting region ", this_reg, " at ", start_time_region))

  this_folder_fvs <- file.path(folder_fvs, this_projreg_name)

  this_folder_out_bl <- file.path(
    folder_out_base,
    "fvs_baseline",
    this_projreg_name
  )
  dir.create(this_folder_out_bl, recursive = TRUE, showWarnings = FALSE)

  #first inner loop on variable
  for (j in seq_along(vars_to_run)) {
    this_var <- vars_to_run[[j]]

    #log and update to console
    start_time_variable <- Sys.time()
    print(paste0(
      "Starting ",
      this_var,
      " at ",
      start_time_variable,
      " for ",
      length(years_to_run),
      " years."
    ))

    #second inner loop on year
    for (y in seq_along(years_to_run)) {
      start_time_layer <- Sys.time()

      this_year <- years_to_run[[y]]

      # Get baseline
      this_baseline <- terra::rast(file.path(
        this_folder_fvs,
        "spat_baseline",
        paste0(
          this_projreg_name,
          "_Baseline_",
          this_year,
          "_",
          this_var,
          ".tif"
        )
      ))

      # Need make sure pixels are consistent between legalmax and baseline
      # Using previous run data for a baseline-only run here

      # eg raster
      px_tif <- paste0(
        this_projreg_name,
        "_baseline_",
        this_year,
        "_",
        eg_mask_var,
        ".tif"
      )
      this_pxtif <- terra::rast(file.path(
        folder_regbl,
        this_projreg_name,
        px_tif
      ))

      #just these pixels
      this_baseline <- terra::crop(this_baseline, this_pxtif, mask = TRUE)

      this_base_out_name <- paste0(
        this_projreg_name,
        "_baseline_",
        this_year,
        "_",
        this_var
      )
      names(this_baseline) <- this_base_out_name
      varnames(this_baseline) <- this_base_out_name
      terra::writeRaster(
        this_baseline,
        file.path(
          this_folder_out_bl,
          paste0(this_base_out_name, ".tif")
        ),
        gdal = c("COMPRESS = DEFLATE"),
        overwrite = TRUE
      )

      end_time_layer <- Sys.time()

      #logging
      idx <- idx + 1
      log_collector[[idx]] <- tibble::tibble(
        region = this_reg,
        variable = this_var,
        year = this_year,
        start_layer = start_time_layer %>% format(),
        end_layer = end_time_layer %>% format(),
        elapsed_layer_minutes = difftime(
          end_time_layer,
          start_time_layer,
          units = c("mins")
        )
      )
    } # end second inner loop on years
  } # end first inner loop on FVS variable
} # end outer loop on region

#write layer run log
log_per_rvy <- dplyr::bind_rows(log_collector)
readr::write_csv(
  log_per_rvy,
  file = file.path(
    folder_logs,
    paste0("log_stitch7_", stamp, "_region_var_year_runtime.csv")
  )
)

end_time <- Sys.time()
(end_time - start_time)
