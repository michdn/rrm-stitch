# Collapsing the various thin treatments into one
#
### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

# Regions 1 - 6 possible
# Note that R6 does not have a thin guide because there's only 1 thin treatment
#  will have exception code to just copy over existing to new folder
#  (Rather than trying to deal with multiple source folders during stitching)
regions_to_run <- c(1, 2, 3, 4, 5, 6)

# FVS variables to run
# variable are inner loop
vars_to_run <- c(
  "aboveground_total_live",
  "mcuft",
  "pot_smoke_sev",
  "tcuft",
  "tot_flame_sev"
)

# Years to run
# Secondary inner loop after variable
years_to_run <- c(2026, 2031, 2036, 2041, 2046)

#output folder for combined thin-treatment rasters
folder_out_base <- file.path("data", "fvs_thin123")
folder_logs <- file.path(folder_out_base, "logs")
dir.create(folder_logs, recursive = TRUE)

#timestamp
stamp <- format(Sys.time(), "%Y%m%d%H%M")

#some logging
log_setup <- tibble::tribble(
  ~var_name          , ~var_value                                 ,
  "regions_to_run"   , paste0(regions_to_run, "", collapse = ",") ,
  "variables_to_run" , paste0(vars_to_run, "", collapse = ",")    ,
  "years_to_run"     , paste0(years_to_run, "", collapse = ",")   ,
  "timestamp"        , stamp
)
readr::write_csv(
  log_setup,
  file = file.path(folder_logs, paste0("log_", stamp, "_setup.csv"))
)

### Data in --------------------------------------------------------------------

# thin guides from Dave
folder_guide <- file.path("data", "fvs_thin_key")

# Folder where the FVS results have been copied into from rem share
folder_fvs <- file.path("data", "fvs_results")

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

  #this region in and out folders
  this_folder_fvs <- file.path(folder_fvs, this_projreg_name)
  this_folder_out <- file.path(
    folder_out_base,
    this_projreg_name
  )
  dir.create(this_folder_out, recursive = TRUE, showWarnings = FALSE)

  # Read in appropriate thin guide for this region
  # (Note, can ignore the 2026 in thin key name - for all years)
  if (this_reg %in% c(1:5)) {
    this_guide <- terra::rast(file.path(
      folder_guide,
      paste0(this_projreg_name, "_Baseline_2026_thin_key.tif")
    ))
    # thin guide has values 0 through potentially 3.
    # (0 is no treatment touches the stand, 1-3 is thin1-thin3)
    # Not all regions have all thin treatments
    # However, selectRanges() indexing begins at 1
    #  Since a variable number of thin treatments, cannot move 0 to a new value
    #   without great difficulty
    # Choice is either 1) shift everything +1, or
    #  2) drop 0 values, and then cover() with either thin-1 or baseline at end
    # Chose option 2.
    this_guide <- terra::mask(
      this_guide,
      this_guide,
      maskvalues = 0,
      updatevalue = NA
    )
  } # end if reg 1-5

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

      #file name of target
      tx_tif <- paste0(
        this_projreg_name,
        "_Project_",
        this_year,
        "_",
        this_var,
        ".tif"
      )

      if (this_reg %in% c(1:5)) {
        # Read in appropriate thin results
        # (region, variable, year, thin-n treatment)
        # as (variable-length, dep on region) stack
        this_thin_paths <- c(
          #subfolder names for the various thinning treatments
          # note, not all regions will have all three
          #R1: (2), R2: (3), R3: (3), R4: (3), R5: (2), R6: (1)
          #list.files() will ignore paths that don't exist
          file.path(this_folder_fvs, "spat_thin-1"),
          file.path(this_folder_fvs, "spat_thin-2"),
          file.path(this_folder_fvs, "spat_thin-3")
        )
        this_thin_files <- list.files(
          path = this_thin_paths,
          pattern = paste0(tx_tif, "$"),
          full.names = TRUE,
          recursive = TRUE
        )
        # Conveniently, want to stack in numerical order 1, 2, 3 (default file order)
        this_thin_stack <- terra::rast(this_thin_files)
        #log and save out this_thing_stack file list
        thin_log <- tibble::tibble(files = this_thin_files) %>%
          dplyr::mutate(id = dplyr::row_number())
        readr::write_csv(
          thin_log,
          file.path(
            folder_logs,
            paste0(tools::file_path_sans_ext(tx_tif), "_", stamp, ".csv")
          )
        )

        # Using thin guide per pixel, collapse thin treatments to one raster
        # Stack thins in order, and use terra::selectRange() (index starts at 1)
        thin_combined <- terra::selectRange(this_thin_stack, this_guide)

        # Now to handle stands/pixels where no treatment actually touched it
        #  (value of 0 in original thin guide)
        # Cover() with thin-1 to get remaining values
        # (thin-1 will equal baseline here, and there is always a thin-1 layer)
        thin_filled <- terra::cover(thin_combined, this_thin_stack[[1]])
        #names() as in thin tx raster
        names(thin_filled) <- names(this_thin_stack[[1]])
        #Save out
        terra::writeRaster(
          thin_combined,
          file.path(this_folder_out, tx_tif),
          gdal = c("COMPRESS = DEFLATE"),
          overwrite = TRUE
        )
      } else if (this_reg == 6) {
        #copy thin-1 over to new folder
        fs::file_copy(
          path = file.path(this_folder_fvs, "spat_thin-1", tx_tif),
          new_path = file.path(this_folder_out, tx_tif),
          overwrite = TRUE
        )
      } # end region if-elses

      end_time_layer <- Sys.time()
      #logging
      idx <- idx + 1
      log_collector[[idx]] <-
        tibble::tibble(
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
    paste0("log_", stamp, "_region_var_year_runtime.csv")
  )
)

end_time <- Sys.time()
(end_time - start_time)
#9 hours
