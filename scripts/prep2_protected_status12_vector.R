# Protected area
# Status 1 or 2 gets burn only

#https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
#https://www.sciencebase.gov/catalog/item/6759b69fd34edfeb8710a3ea
# PAD-US 4.1 Vector Analysis.
#  Version that has already removed overlaps (prioritizing status 1>2>etc)
# Publication Date 2024-04-25

# For this project, interested in
# GAP_Sts = 1 or 2

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  sf,
  terra
)

### Data in --------------------------------------------------------------------

#binary treemap raster, mosaic from regions 1-6
folder_tmr <- file.path("data", "region_treemap")
tmr1 <- terra::rast(file.path(folder_tmr, "mosaic_r1-r6_treemap_mask.tif"))

#check layers from original protected geodatabase
sf::st_layers(file.path(
  "data",
  "protected",
  "PADUS4_1VectorAnalysis_PADUS_Only",
  "PADUS4_1VectorAnalysis_PADUS_Only.gdb"
))
#layer name = "PADUS4_1VectorAnalysis_PADUS_Only_Simp_SingP"

pad <- sf::read_sf(
  dsn = file.path(
    "data",
    "protected",
    "PADUS4_1VectorAnalysis_PADUS_Only",
    "PADUS4_1VectorAnalysis_PADUS_Only.gdb"
  ),
  layer = "PADUS4_1VectorAnalysis_PADUS_Only_Simp_SingP"
)


### Filter, project protected --------------------------------------------------

# IMPORTANT! Must make field numeric before rasterization, otherwise it
# will treat it as a categorical,
#  and the e.g. 1 & 2 values become 0 & 1 values when you save.
pad12 <- pad %>%
  # values "4" "3" "2" "1"
  dplyr::mutate(GAP_Sts = as.numeric(GAP_Sts)) %>%
  dplyr::filter(GAP_Sts %in% c(1, 2))

#has 292176 features
pad12

#CRS 5070
pad12_5070 <- sf::st_transform(pad12, crs = "EPSG:5070")

#save intermediate
sf::write_sf(
  pad12_5070,
  file.path("data", "protected", "protected_status12_5070_20260727.gpkg")
)

### Rasterize ------------------------------------------------------------------

# Rasterized to treemap pixel mask raster
#  (this was used to reproject water results, so everything is pixel-aligned)
# Keeping protected status 1 or 2 (instead of collapsing to single value)
#  May be wanted in future, but not part of original rules.

#rasterize
pad12_r <- terra::rasterize(
  pad12_5070,
  tmr1,
  field = "GAP_Sts",
  #in case of any overlap, take highest priority value
  fun = "min",
  #if ANY part of pixel is protected, then protected
  touches = TRUE
)

varnames(pad12_r) <- names(pad12_r) #GAP_Sts
terra::writeRaster(
  pad12_r,
  file.path("data", "protected", "protected_status12_20260727.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
