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
  sf
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

pad <- sf::st_read(
  dsn = file.path(
    "data",
    "protected",
    "PADUS4_1VectorAnalysis_PADUS_Only",
    "PADUS4_1VectorAnalysis_PADUS_Only.gdb"
  ),
  layer = "PADUS4_1VectorAnalysis_PADUS_Only_Simp_SingP"
)


### Filter, project protected --------------------------------------------------

pad12 <- pad %>%
  dplyr::filter(GAP_Sts %in% c(1, 2))

#has 292176 features
pad12

#CRS 5070
pad12_5070 <- sf::st_transform(pad12, crs = "EPSG:5070")

#save intermediate
sf::write_sf(
  pad12_5070,
  file.path("data", "protected", "protected_status12_5070.gpkg")
)

### Rasterize ------------------------------------------------------------------

# Rasterized to treemap pixel mask raster
#  (this was used to reproject water results, so everything is pixel-aligned)
# Keeping protected status 1 or 2 (instead of collapsing to single value)
#  May be wanted in future, but not part of original rules.
pad12_r <- terra::rasterize(
  pad12_5070,
  tmr1,
  field = "GAP_Sts",
  #in case of any overlap, which there should not be, take highest priority value
  fun = "min",
  #if ANY part of pixel is protected, then protected
  touches = TRUE
)

terra::writeRaster(
  pad12_r,
  file.path("data", "protected", "protected_status12.tif"),
  gdal = c("COMPRESS = DEFLATE"),
  overwrite = TRUE
)
