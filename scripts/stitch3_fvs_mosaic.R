# after stitching fvs region results
# for each variable-year combination:
# mosaic diff together
# (mosaic legalmax together if wanted/QA?)

#FIXME handle region 3 weirdness with buffered baseline!

# CREATE BINARY FVS MASK FOR WATER
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

# FVS variables to run
vars_to_run <- c(
  "aboveground_total_live",
  "mcuft",
  "pot_smoke_sev",
  "tcuft",
  "tot_flame_sev"
)

# Years to run
years_to_run <- c(2026, 2031, 2036, 2041, 2046)

#output folders
folder_out <- file.path("data", "output", "fvs_mosaic")
dir.create(folder_out, recursive = TRUE)


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

    #TODO
    # get all six region difference layers
    # for this variable and year combination
    #TODO
    # Mosaic via faster merge (no overlap) with algo 2
    #TODO
    # Save out
  }
}
