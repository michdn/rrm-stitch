# after stitching fvs region results
# for each variable-year combination:
# mosaic diff together
# (mosaic legalmax together if wanted/QA)

# 20 hours for 2 mosaics, 5 years, 5 variables. (~22min per raster)

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

# Mosaic (merge) which of the region results?
# "legalmax" is the stitched treatment results
# "diff" is the subtracted layer: legalmax - baseline. DEPRECATED.
# "baseline" is the baseline layers (from pixel matched/consistent intermediates)
# c("legalmax", "baseline")
which_mosaic <- c("baseline", "legalmax")

# FVS variables to run
vars_to_run <- c(
  #"aboveground_total_live",
  "mcuft",
  #"pot_smoke_sev",
  "tcuft" #,
  #"tot_flame_sev"
)

# Years to run
#c(2026, 2031, 2036, 2041, 2046)
years_to_run <- c(2026)

#output folders
folder_out <- file.path("data", "output", "fvs_mosaic")
folder_logs <- file.path(folder_out, "logs")
dir.create(folder_logs, recursive = TRUE)

#timestamp
stamp <- format(Sys.time(), "%Y%m%d%H%M")

#set up logging
log_setup <- tibble::tribble(
  ~var_name          , ~var_value                               ,
  "variables_to_run" , paste0(vars_to_run, "", collapse = ",")  ,
  "years_to_run"     , paste0(years_to_run, "", collapse = ",") ,
  "timestamp"        , stamp
)
readr::write_csv(
  log_setup,
  file = file.path(folder_logs, paste0("log_", stamp, "_setup.csv"))
)

### Data in --------------------------------------------------------------------

# if ("diff" %in% which_mosaic) {
#   folder_diff <- file.path("data", "output", "fvs_region", "fvs_treatdiff")
#   folder_out_diff <- file.path(folder_out, "difference")
#   dir.create(folder_out_diff, recursive = TRUE)
# }

if ("legalmax" %in% which_mosaic) {
  folder_lm <- file.path("data", "output", "fvs_region", "fvs_legalmax")
  folder_out_lm <- file.path(folder_out, "legalmax")
  dir.create(folder_out_lm, recursive = TRUE)
}

if ("baseline" %in% which_mosaic) {
  folder_bl <- file.path("data", "output", "fvs_region", "fvs_baseline")
  folder_out_bl <- file.path(folder_out, "baseline")
  dir.create(folder_out_bl, recursive = TRUE)
}


### Loop -----------------------------------------------------------------------

#total number of variables - years that will be run
# NOTE: not accurately number of "layers", since not looking at how many mosaics
#  (legalmax and/or difference and/or baseline layers)
total_layer_count <- length(vars_to_run) * length(years_to_run)
#dedicated indexer (could have done fancy math, but this is easier)
idx <- 0 #start at 0, will be adding 1 before first write
log_collector <- vector("list", length = total_layer_count)

(start_time <- Sys.time())
for (v in seq_along(vars_to_run)) {
  this_var <- vars_to_run[[v]]

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

  for (y in seq_along(years_to_run)) {
    start_time_layer <- Sys.time()

    this_year <- years_to_run[[y]]

    #log and update to console
    start_time_year <- Sys.time()
    print(paste0(
      "Starting ",
      this_year,
      " at ",
      start_time_layer,
      " for ",
      length(which_mosaic),
      " mosiac(s)."
    ))

    # # get all six region difference layers
    # # for this variable and year combination
    # if ("diff" %in% which_mosaic) {
    #   this_diff_partname <- paste0(
    #     "_difference_",
    #     this_year,
    #     "_",
    #     this_var
    #   )
    #   this_diffs_files <- list.files(
    #     path = folder_diff,
    #     pattern = paste0("PC612_R[0-9]?", this_diff_partname, "\\.tif$"),
    #     full.names = TRUE,
    #     #search all region subfolders
    #     recursive = TRUE
    #   )
    #   #need all 6 regions
    #   if (!length(this_diffs_files) == 6) {
    #     stop(paste0(
    #       "Incorrect number of region files found: ",
    #       length(this_diffs_files),
    #       " instead of 6."
    #     ))
    #   }
    #   #create sprc (different extents okay) for merging
    #   this_sprc <- terra::sprc(this_diffs_files)
    #   #mosaic via faster merge (no overlap) with algo 2
    #   #18.5 min per layer (w/Chrome running on puffin)
    #   this_mosaic <- terra::merge(this_sprc, algo = 2)

    #   #save
    #   this_diff_mos_name <- paste0(
    #     "Difference_",
    #     this_year,
    #     "_",
    #     this_var
    #   )
    #   names(this_mosaic) <- this_diff_mos_name
    #   varnames(this_mosaic) <- this_diff_mos_name
    #   terra::writeRaster(
    #     this_mosaic,
    #     file.path(
    #       folder_out_diff,
    #       paste0(this_diff_mos_name, ".tif")
    #     ),
    #     gdal = c("COMPRESS = DEFLATE"),
    #     overwrite = TRUE
    #   )
    # } # end if difference mosaic

    if ("legalmax" %in% which_mosaic) {
      this_lm_partname <- paste0(
        "_legalmax_",
        this_year,
        "_",
        this_var
      )
      this_lms_files <- list.files(
        path = folder_lm,
        pattern = paste0("PC612_R[0-9]?", this_lm_partname, "\\.tif$"),
        full.names = TRUE,
        #search all region subfolders
        recursive = TRUE
      )
      #need all 6 regions
      if (!length(this_lms_files) == 6) {
        stop(paste0(
          "Incorrect number of region files found: ",
          length(this_lms_files),
          " instead of 6."
        ))
      }
      #create sprc (different extents okay) for merging
      this_sprc_lm <- terra::sprc(this_lms_files)
      #mosaic via faster merge (no overlap) with algo 2
      this_mosaic_lm <- terra::merge(this_sprc_lm, algo = 2)

      #save
      this_lm_mos_name <- paste0(
        "Legalmax_",
        this_year,
        "_",
        this_var
      )
      names(this_mosaic_lm) <- this_lm_mos_name
      varnames(this_mosaic_lm) <- this_lm_mos_name
      terra::writeRaster(
        this_mosaic_lm,
        file.path(
          folder_out_lm,
          paste0(this_lm_mos_name, ".tif")
        ),
        gdal = c("COMPRESS = DEFLATE"),
        overwrite = TRUE
      )
    } # end if legalmax

    #should have made this a function
    if ("baseline" %in% which_mosaic) {
      this_bl_partname <- paste0(
        "_baseline_",
        this_year,
        "_",
        this_var
      )
      this_bls_files <- list.files(
        path = folder_bl,
        pattern = paste0("PC612_R[0-9]?", this_bl_partname, "\\.tif$"),
        full.names = TRUE,
        #search all region subfolders
        recursive = TRUE
      )
      #need all 6 regions
      if (!length(this_bls_files) == 6) {
        stop(paste0(
          "Incorrect number of region files found: ",
          length(this_bls_files),
          " instead of 6."
        ))
      }
      #create sprc (different extents okay) for merging
      this_sprc_bl <- terra::sprc(this_bls_files)
      #mosaic via faster merge (no overlap) with algo 2
      this_mosaic_bl <- terra::merge(this_sprc_bl, algo = 2)

      #save
      this_bl_mos_name <- paste0(
        "Baseline_",
        this_year,
        "_",
        this_var
      )
      names(this_mosaic_bl) <- this_bl_mos_name
      varnames(this_mosaic_bl) <- this_bl_mos_name
      terra::writeRaster(
        this_mosaic_bl,
        file.path(
          folder_out_bl,
          paste0(this_bl_mos_name, ".tif")
        ),
        gdal = c("COMPRESS = DEFLATE"),
        overwrite = TRUE
      )
    } # end if baseline
    end_time_layer <- Sys.time()

    #logging
    idx <- idx + 1
    log_collector[[idx]] <-
      tibble::tibble(
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
  } # end years loop
} # end variables loop

#write layer run log
log_per_rvy <- dplyr::bind_rows(log_collector)
readr::write_csv(
  log_per_rvy,
  file = file.path(
    folder_logs,
    paste0("log_", stamp, "_var_year_runtime.csv")
  )
)

end_time <- Sys.time()
(end_time - start_time)
