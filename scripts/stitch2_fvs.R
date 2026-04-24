# Treatment stitching of FVS results

# Results are coming in batches by region,
# so by region, here, to be mosaic in other script

# Uses combined thin results from prep4_fvs_thin_combine.R

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

# Regions 1 - 6 possible
# Regions drive the outer loop
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

#output folders
folder_out_base <- file.path("data", "output", "fvs_region")
folder_logs <- file.path(folder_out_base, "logs")
dir.create(folder_logs, recursive = TRUE)

#timestamp
stamp <- format(Sys.time(), "%Y%m%d%H%M")

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
  file = file.path(folder_logs, paste0("log_", stamp, "_setup.csv"))
)

### Data in --------------------------------------------------------------------

# Rasterized protected areas, status 1 or 2
# Pixel aligned to treemap/FVS
pad12_r <- terra::rast(file.path("data", "protected", "protected_status12.tif"))

# Folder where the FVS results have been copied into from rem share
# for prescription burn (and baseline) (and raw thin treatments)
folder_fvs <- file.path("data", "fvs_results")
# for already-combined thin treatments (see prep4_fvs_thin_combine.R script)
folder_thin <- file.path("data", "fvs_thin123")

# NOTE: file names are duplicated between different treatments,
# region folder data/fvs_results/PC612_R6/
# /spat_baseline/ e.g. PC612_R6_Baseline_2026_aboveground_total_live.tif
# /spat_rxb/ e.g. PC612_R6_Project_2026_aboveground_total_live.tif
# /spat_thin-1/ e.g. PC612_R6_Project_2026_aboveground_total_live.tif
# Note: no longer as relevant as we are now using combined thin rasters
#  from a different folder

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
  this_folder_thin <- file.path(folder_thin, this_projreg_name)

  this_folder_out_lm <- file.path(
    folder_out_base,
    "fvs_legalmax",
    this_projreg_name
  )
  dir.create(this_folder_out_lm, recursive = TRUE, showWarnings = FALSE)
  this_folder_out_diff <- file.path(
    folder_out_base,
    "fvs_treatdiff",
    this_projreg_name
  )
  dir.create(this_folder_out_diff, recursive = TRUE, showWarnings = FALSE)

  # Pull an example raster to crop pad12_r to this region
  # Use first variable and year requested
  #(Cannot use baseline in R3 because it had to be buffered to run in FVS)
  # Not doing inside inner loops, since that would be very inefficient and add a lot of time
  eg_var <- vars_to_run[[1]]
  eg_year <- years_to_run[[1]]
  eg_reg_raster <- terra::rast(file.path(
    this_folder_fvs,
    "spat_rxb",
    paste0(
      this_projreg_name,
      "_Project_",
      eg_year,
      "_",
      eg_var,
      ".tif"
    )
  ))
  this_pad <- terra::crop(pad12_r, eg_reg_raster)

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

      #get the two treatment rasters
      tx_tif <- paste0(
        this_projreg_name,
        "_Project_",
        this_year,
        "_",
        this_var,
        ".tif"
      )
      this_burn <- terra::rast(file.path(
        this_folder_fvs,
        "spat_rxb",
        tx_tif
      ))
      this_thin <- terra::rast(file.path(
        this_folder_thin,
        tx_tif
      ))

      # Treatment stitching rules
      this_legalmax <- terra::ifel(this_pad %in% c(1, 2), this_burn, this_thin)

      #Saving out
      this_out_name <- paste0(
        this_projreg_name,
        "_legalmax_",
        this_year,
        "_",
        this_var
      )
      #FIXME keep FVS var name??
      names(this_legalmax) <- this_out_name
      varnames(this_legalmax) <- this_out_name
      terra::writeRaster(
        this_legalmax,
        file.path(
          this_folder_out_lm,
          paste0(this_out_name, ".tif")
        ),
        gdal = c("COMPRESS = DEFLATE"),
        overwrite = TRUE
      )

      # Subtract baseline
      # get baseline
      this_baseline <- terra::rast(file.path(
        this_folder_fvs,
        "spat_baseline",
        paste0(
          this_projreg_name,
          "_Baseline_",
          eg_year,
          "_",
          eg_var,
          ".tif"
        )
      ))
      # IF REGION 3
      #baseline was run with a buffer, so need to strip out
      if (this_reg == 3) {
        #mask baseline with legalmax
        #FIXME test me and remove "2"
        this_baseline2 <- terra::crop(this_baseline, this_legalmax, mask = TRUE)
      }

      #difference raster (treated versus untreated/baseline)
      # if treated was 50, and baseline 100, then diff is 50-100 = -50
      this_diff <- this_legalmax - this_baseline

      this_diff_out_name <- paste0(
        this_projreg_name,
        "_difference_",
        this_year,
        "_",
        this_var
      )
      #FIXME keep FVS layer name??
      #names(this_diff) <- this_diff_out_name
      #varnames(this_diff) <- this_diff_out_name
      terra::writeRaster(
        this_diff,
        file.path(
          this_folder_out_diff,
          paste0(this_diff_out_name, ".tif")
        ),
        gdal = c("COMPRESS = DEFLATE"),
        overwrite = TRUE
      )

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
