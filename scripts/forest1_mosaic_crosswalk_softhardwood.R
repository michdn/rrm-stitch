# Creating a forest type (1 softwood / 2 hardwood / 3 mixed) raster
#   for the biomass metric
# Based on FVS fortyp, manually crosswalks to hard- or softwood

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

# forest type folder (in & out)
folder_ft <- file.path("data", "forest_type")

# crosswalk of hardwood and softwood values
#  fortyp values from Appendix B of Essential FVS
#  with manually added softwood / hardwood / mixed designations
ft_xwalk_raw <- readr::read_csv(
  file.path(folder_ft, "ForestType_hardwood_softwood_20260514.csv")
)

# FVS per region fortyp files
files_ft <- list.files(
  path = folder_ft,
  pattern = ".*Baseline_2026_forest_type\\.tif$",
  full.names = TRUE
)
if (!length(files_ft) == 6) {
  #stop("Wrong number of region forest type rasters.")
  print(paste0(length(files_ft), " of 6 region files found."))
}

#Where final treated & baseline FVS rasters live
# To use as a mask
folder_mos <- file.path("data", "output", "fvs_mosaic")
# # A final FVS final result to use
# fvs_file <- "Baseline_2026_aboveground_total_live.tif"
# fvs <- terra::rast(file.path(folder_mos, "baseline", fvs_file))
# # Create binary FVS-result mask
# fvsbin <- terra::ifel(!is.na(fvs), 1, NA)
#save this for next time
# terra::writeRaster(
#   fvsbin,
#   file.path(folder_mos, "fvs_results_mask_bl2026atl.tif"),
#   gdal = c("COMPRESS = DEFLATE"),
#   #integer result 2S or 2U probably fine
#   datatype = "INT2S",
#   overwrite = TRUE
# )
fvsbin <- terra::rast(file.path(folder_mos, "fvs_results_mask_bl2026atl.tif"))

### Crosswalk & rcl prep -------------------------------------------------------

ft_xwalk <- ft_xwalk_raw %>%
  #R friendly col names, only ones needed
  dplyr::rename(fvs_code = `Type Code`, forest_type = `Hardwood/Softwood`) %>%
  dplyr::select(fvs_code, forest_type) %>%
  # parse
  tidyr::separate_wider_delim(
    forest_type,
    delim = "_",
    names = c("ft_code", "ft_desc")
  ) %>%
  dplyr::mutate(ft_code = as.numeric(ft_code))

ft_rcl <- ft_xwalk %>%
  dplyr::select(fvs_code, ft_code) %>%
  as.matrix()

### region 3 -------------------------------------------------------------------

# # region 3 baseline has buffer
# # want to check if buffer values are same as in the overlapping regions
# # plan: mask reg 3 by other regions, then subtract by that region, see if non-0
# # R2 border NE, R4 border NW, R5 to the west
# r3 <- terra::rast(files_ft %>% grep(pattern = "R3", value = TRUE))

# r2 <- terra::rast(files_ft %>% grep(pattern = "R2", value = TRUE))
# r4 <- terra::rast(files_ft %>% grep(pattern = "R4", value = TRUE))
# r5 <- terra::rast(files_ft %>% grep(pattern = "R5", value = TRUE))

# r34 <- terra::crop(r3, r4)
# r34e <- terra::extend(r34, r4)
# r34m <- terra::crop(r34e, r4, mask = TRUE)
# r4diff <- r4 - r34m
# #min -60 max 778
# r4d_freq <- terra::freq(r4diff) %>% tibble::as_tibble()
# # almost all 0s, but there are 38 non-zeros.

# # Cannot simple merge!

### Mosaic & reclassify --------------------------------------------------------

# Prep for partial mosaic & cover
r3 <- terra::rast(files_ft %>% grep(pattern = "R3", value = TRUE))
# create sprc (different extents okay) for merging
# all EXCEPT R3 (mismatches in overlap)
ft_sprc <- terra::sprc(
  files_ft %>% grep(pattern = "R3", value = TRUE, invert = TRUE)
)
# mosaic non-overlapping regions via faster merge with algo 2
ft_mosaic <- terra::merge(ft_sprc, algo = 2)
# use cover to pick up R3 pixels that do not overlap
r3e <- terra::extend(r3, ft_mosaic)
ft_mosaic_e <- terra::extend(ft_mosaic, r3e)
ft_full <- terra::cover(ft_mosaic_e, r3e)

# Remove any R3 buffer outside of R3 non-buffered (treated) results
# by masking with the same as AET mask to FVS results
ft_mask <- terra::mask(ft_full, fvsbin, maskvalues = NA)

# reclassify with ft_rcl
ft_reclass <- terra::classify(ft_mask, ft_rcl)

# add categories (not strictly necessary, they probably don't use the .aux.xmls)
levels(ft_reclass) <- ft_xwalk %>%
  dplyr::select(ft_code, ft_desc) %>%
  dplyr::filter(!is.na(ft_code)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(ft_code)

# varnames and save
varnames(ft_reclass) <- "softwood_hardwood_mixed"
names("softwood_hardwood_mixed")

terra::writeRaster(
  ft_reclass,
  file.path(
    folder_ft,
    "softwood_hardwood_mixed.tif"
  ),
  gdal = c("COMPRESS = DEFLATE"),
  #integer result 2S or 2U probably fine
  datatype = "INT2S",
  overwrite = TRUE
)
