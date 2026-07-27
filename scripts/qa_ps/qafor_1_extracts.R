# Creating similar data to Funding Opportunity Report

# PS uses pixels with all_touched = false,
# so pixels with centers within polygon are included

# However, "the treatment area calculations are with all_touched = true and
#  calculating the actual percentage of overlap, adjusting the area to it"

# My original rasters are in EPSG 5070
#    "planscape stores everything in 3857. vectors are stored as 4269.
#     Calculations happen in 4326, but using geodesic algorithms"

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra,
  sf
)

### Settings -------------------------------------------------------------------

#resave output (RDS, graphs)?
resave <- TRUE
#(re)save QA output?
qa_save <- TRUE

# qa ps folder
folder_qaps <- file.path("data", "qa_ps")

# in "for_gpkgs" subfolder
#gpkg_name <- "geopackage-5677_pilotgv_scenqa1.gpkg"
#gpkg_name <- "geopackage-5686_placerville_200a5.gpkg"
gpkg_name <- "geopackage-5690_wa_prot_v2.gpkg"

# per gpkg subfolder results
folder_out <- file.path(folder_qaps, tools::file_path_sans_ext(gpkg_name))
dir.create(folder_out)

### Data in --------------------------------------------------------------------
#preventing scientific notation
options(scipen = 999)

# funding opportunity report geopackage
# these are the numbers we are double-checking!
# in EPSG 4326

# Note: project areas CAN go beyond planning area, so will need to buffer
#  planning area before cropping
plan <- sf::read_sf(
  dsn = file.path(
    folder_qaps,
    "for_gpkgs",
    gpkg_name
  ),
  layer = "planning_area"
)
proj <- sf::read_sf(
  dsn = file.path(
    folder_qaps,
    "for_gpkgs",
    gpkg_name
  ),
  layer = "project_areas"
)

# water base folder
folder_wa <- file.path("data", "output", "water_availability")

# fvs mosaic base output folder
folder_mos <- file.path("data", "output", "fvs_mosaic")
# baseline, legalmax subfolders
# eg Baseline_2046_pot_smoke_sev.tif, Legalmax_2026_pot_smoke_sev.tif
# merch and nonmerch
# (note wood is brought in separately below for special processing)
# "Baseline_2026_nonmerch_cuft.tif", "Baseline_2026_bdft.tif"
var_list <- c(
  "aboveground_total_live",
  "pot_smoke_sev",
  "tot_flame_sev",
  "nonmerch_cuft",
  "bdft"
)
var_pattern <- paste(paste0(var_list, "\\.tif$"), collapse = "|")

fvs_files <- list.files(
  c(file.path(folder_mos, "baseline"), file.path(folder_mos, "legalmax")),
  pattern = var_pattern,
  full.names = TRUE,
  recursive = TRUE
)
targets <- fvs_files %>%
  tibble::as_tibble() %>%
  dplyr::rename(fullpath = value) %>%
  dplyr::mutate(
    filenm = tools::file_path_sans_ext(basename(fullpath)),
    scenario = stringr::str_split_i(filenm, "_", 1),
    year = stringr::str_split_i(filenm, "_", 2),
    var = stringr::str_split_i(filenm, "_[0-9]{4}_", 2)
  ) %>%
  dplyr::select(-filenm)

#Adding specials (at beginning)
targets <- dplyr::bind_rows(
  # tx key CATEGORICAL
  tibble(
    fullpath = file.path(
      "data",
      "output",
      "tx_key_rxfire1_thinburn2.tif"
    ),
    scenario = "TxKey",
    year = "9999", #just to have something, it's all years
    var = "tx_key"
  ),
  #water - extracted in project areas, compared to planning acres
  tibble(
    fullpath = file.path(
      folder_wa,
      "water_avail_perc_change_legalmax_fvsmasked.tif"
    ),
    scenario = "PercentChange",
    year = "2026",
    var = "water_pchange"
  ),
  targets
)

# biomass
# SUM of PROJECT areas, but special processing with wood type!
# wood type CATEGORICAL!
# 1: softwood, 2: hardwood, 3: mixed
wood <- terra::rast(file.path(
  "data",
  "forest_type",
  "softwood_hardwood_mixed.tif"
))

# water
# (for plan area extraction/info)
water <- terra::rast(file.path(
  folder_wa,
  "water_avail_perc_change_legalmax_fvsmasked.tif"
))
# forgot to rename water layer before saving originally
names(water) <- "water_pchange"


### GIS prep -------------------------------------------------------------------

# buffer planning area for rough cropping
#  distance was rough measurement from QGIS on diameter or 10 acre stand size
# this will be done in 5070 BEFORE projecting raster
plan_5070 <- sf::st_transform(plan, crs = "EPSG:5070")
plan_buff <- sf::st_buffer(plan_5070, dist = 250)

# note: will be doing raster reprojection on the fly inside loop on CROPPED rasters

#pivot all planscape project data long, for later matching up
proj_long <- proj %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-c(id, name)) %>%
  tidyr::pivot_longer(-proj_id, names_to = "ps_var_full")

plan_long <- plan %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-c(id, region_name)) %>%
  tidyr::pivot_longer(-name, names_to = "ps_var_full")


### loop on project area targets -----------------------------------------------

# per 100% coverage pixel value collector
pxs_collector <- vector(length = nrow(targets), "list")

for (i in 1:nrow(targets)) {
  print(paste0("Starting ", i, " of ", nrow(targets), " at ", Sys.time()))

  this_row <- targets[i, ]
  this_rast <- terra::rast(this_row[["fullpath"]])

  # rough crop to reduce processing
  this_rast_crop <- terra::crop(this_rast, terra::vect(plan_buff))
  # determine reprojection algorithm -- UPDATE: PS does NEAREST for everything
  #prj_algo <- dplyr::if_else(this_row[["var"]] == "tx_key", "near", "bilinear")
  # reproject to PS storage 3857. and then to processing crs 4326
  this_rast_3857 <- terra::project(
    this_rast_crop,
    y = "EPSG:3857",
    method = "near"
  )
  this_rast_4326 <- terra::project(
    this_rast_3857,
    y = "EPSG:4326",
    method = "near"
  )
  #since we are stacking with cell sizes, we need raster name to be
  # the same across all variables in this loop (row details will have info)
  names(this_rast_4326) <- "value"

  # calculate cell areas (creates raster)
  this_sizes <- terra::cellSize(this_rast_4326, unit = "ha") #hectares

  # calculate binary if ANY pixel center is within proj areas
  this_rast_init <- terra::init(this_rast_4326, 1)
  this_center <- terra::mask(this_rast_init, proj, touches = FALSE)
  this_center <- terra::mask(
    this_center,
    this_center,
    inverse = TRUE,
    updatevalue = 1
  )
  names(this_center) <- "center_incl"

  # separate processing steps if biomass or not
  if (this_row[["var"]] %in% c("nonmerch_cuft", "bdft")) {
    #biomass is separated by wood type
    this_wood_crop <- terra::crop(wood, terra::vect(plan_buff))
    this_wood_3857 <- terra::project(
      this_wood_crop,
      y = "EPSG:3857",
      method = "near"
    )
    this_wood_4326 <- terra::project(
      this_wood_3857,
      y = "EPSG:4326",
      method = "near"
    )
    #softwood
    this_rast_soft <- terra::mask(
      this_rast_4326,
      mask = this_wood_4326,
      maskvalues = 1,
      inverse = TRUE
    )
    this_px_soft <- exactextractr::exact_extract(
      c(this_rast_soft, this_sizes, this_center),
      proj,
      fun = NULL,
      include_cols = "proj_id",
      include_cell = TRUE
    ) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(woodtype = "softwood")
    #hardwood
    this_rast_hard <- terra::mask(
      this_rast_4326,
      mask = this_wood_4326,
      maskvalues = 2,
      inverse = TRUE
    )
    this_px_hard <- exactextractr::exact_extract(
      c(this_rast_hard, this_sizes, this_center),
      proj,
      fun = NULL,
      include_cols = "proj_id",
      include_cell = TRUE
    ) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(woodtype = "hardwood")
    #mixed
    this_rast_mixed <- terra::mask(
      this_rast_4326,
      mask = this_wood_4326,
      maskvalues = 3,
      inverse = TRUE
    )
    this_px_mixed <- exactextractr::exact_extract(
      c(this_rast_mixed, this_sizes, this_center),
      proj,
      fun = NULL,
      include_cols = "proj_id",
      include_cell = TRUE
    ) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(woodtype = "mixed")

    # Combining all woods
    this_px <- dplyr::bind_cols(
      this_row,
      dplyr::bind_rows(this_px_soft, this_px_hard, this_px_mixed)
    ) %>%
      # make same format/columns as other variables
      dplyr::mutate(var = paste0(var, "-", woodtype)) %>%
      dplyr::select(-any_of(c("fullpath", "woodtype")))

    # save our pixel values to be worked with later
    pxs_collector[[i]] <- this_px

    if (qa_save) {
      # for QA. creating raster with center-selected pixels only
      r_sel_soft <- terra::init(this_rast_soft, NA)
      this_sel_soft <- this_px_soft %>% dplyr::filter(center_incl == 1)
      r_sel_soft[this_sel_soft$cell] <- this_rast_soft[this_sel_soft$cell]
      terra::writeRaster(
        r_sel_soft,
        file.path(
          folder_out,
          paste0(
            this_row[["var"]],
            "_",
            this_row[["year"]],
            "_",
            this_row[["scenario"]],
            "_soft_centerselected_4326.tif"
          )
        ),
        gdal = c("COMPRESS = DEFLATE"),
        overwrite = TRUE
      )
    } # end qa_save (special wood soft save)
  } else {
    #all other variables
    # all touched pixels with values and coverage fraction
    this_px_values <- exactextractr::exact_extract(
      c(this_rast_4326, this_sizes, this_center),
      proj,
      fun = NULL,
      include_cols = "proj_id",
      include_cell = TRUE
    ) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()

    # save our pixel values to be worked with later
    pxs_collector[[i]] <- cbind(this_row, this_px_values) %>%
      dplyr::select(-any_of(c("fullpath")))
  } # end else of biomass vars

  if (qa_save) {
    # for QA. creating raster with center-selected pixels only
    r_sel <- terra::init(this_rast_4326, NA)
    this_sel <- this_px_values %>% dplyr::filter(center_incl == 1)
    r_sel[this_sel$cell] <- this_rast_4326[this_sel$cell]
    terra::writeRaster(
      r_sel,
      file.path(
        folder_out,
        paste0(
          this_row[["var"]],
          "_",
          this_row[["year"]],
          "_",
          this_row[["scenario"]],
          "_centerselected_4326.tif"
        )
      ),
      gdal = c("COMPRESS = DEFLATE"),
      overwrite = TRUE
    )
    #QGIS can't display rasters in 4326 and in 5070 at the same time, bug?
    r_sel_3857 <- terra::project(r_sel, y = "EPSG:3857", method = "near")
    r_sel_5070 <- terra::project(
      r_sel_3857,
      y = this_rast_crop,
      method = "near"
    )
    terra::writeRaster(
      r_sel_5070,
      file.path(
        folder_out,
        paste0(
          this_row[["var"]],
          "_",
          this_row[["year"]],
          "_",
          this_row[["scenario"]],
          "_centerselected_5070.tif"
        )
      ),
      gdal = c("COMPRESS = DEFLATE"),
      overwrite = TRUE
    )
  } # end qa save
} # end target loop

all_pxs <- dplyr::bind_rows(pxs_collector) %>%
  tibble::as_tibble()

#quick save out
if (resave) {
  saveRDS(all_pxs, file.path(folder_out, "all_pxs.RDS"))
}

### Planning area level extract & calcs ----------------------------------------
# Water is calculated at planning area, but with project area values
# % of area with water increase above threshold
#  using combination of cellSize, masking, stacking, and exact_extract

#rough crop to reduce processing for projecting
water_crop <- terra::crop(water, terra::vect(plan_buff))
#reproject through PS projections (storage and processing)
water_3857 <- terra::project(
  water_crop,
  y = "EPSG:3857",
  method = "near"
)
water_4326 <- terra::project(
  water_3857,
  y = "EPSG:4326",
  method = "near"
)
# values do not matter, only getting included pixels for area calcs
#  but need all pixels, not just pixels with data
water_init <- terra::init(water_4326, 1)

# calculate cell areas (creates raster)
w_sizes <- terra::cellSize(water_init, unit = "ha") #hectares

# calculate binary if ANY pixel center is within PLAN areas
w_center <- terra::mask(water_init, plan, touches = FALSE)
w_center <- terra::mask(w_center, w_center, inverse = TRUE, updatevalue = 1)
names(w_center) <- "center_incl"

#stack both sizes and center incl.
# (don't need values here, but including just in case)
w_stack <- c(w_sizes, w_center, water_4326)

#and extract
# all touched pixels with values and coverage fraction
plan_info <- exactextractr::exact_extract(
  w_stack,
  plan, # plan not project
  fun = NULL,
  include_cols = "name",
  include_cell = TRUE
) %>%
  #only one, but still returned as list
  dplyr::bind_rows() %>%
  tibble::as_tibble()

if (resave) {
  saveRDS(plan_info, file.path(folder_out, "plan_water_pxs_info.RDS"))
}

### Calculations ---------------------------------------------------------------

#project-level xwalk
#ps var stems, with year_scenario, reduction brackets, or wood_unit, added
name_xwalk <- tibble::tribble(
  ~var                     , ~ps_var                                          ,
  "aboveground_total_live" , "ABOVEGROUND_TOTAL"                              ,
  "pot_smoke_sev"          , "POTENTIAL_SMOKE"                                ,
  "tot_flame_sev"          , "TOTAL_FLAME_SEVERITY"                           ,
  #with wood type
  "bdft-softwood"          , "BIOMASS_VOLUMES_merchantable_softwood_bf"       ,
  "bdft-hardwood"          , "BIOMASS_VOLUMES_merchantable_hardwood_bf"       ,
  "bdft-mixed"             , "BIOMASS_VOLUMES_merchantable_mixed_bf"          ,
  "nonmerch_cuft-softwood" , "BIOMASS_VOLUMES_non_merchantable_softwood_cuft" ,
  "nonmerch_cuft-hardwood" , "BIOMASS_VOLUMES_non_merchantable_hardwood_cuft" ,
  "nonmerch_cuft-mixed"    , "BIOMASS_VOLUMES_non_merchantable_mixed_cuft"
)


# Note:
# baseline is baseline
# value is legalmax
# delta means percent change here

# for tot flame sev
# delta, value, baseline, raw_value, total_area

## Project level
# means for atl, smoke

# flame length: g7l4, g6l4, g4l2
#  comparing baseline to legalmax, for that year
#  % of area
# Cheating by using cell numbers to link up baseline to legalmax
#  only works because same size rasters, same crop, same polygons
#   probably highly unrecommended.
# Otherwise would have needed to have seperate oop by year for tot flame
#  and pull in both rasters at once.

# biomass

### Comparisons to PS: ATL, Smoke ----------------------------------------------

#atl and smoke (aka the 'easy' ones)
atl_smoke <- all_pxs %>%
  # filter to only center included pixels
  dplyr::filter(center_incl == 1) %>%
  dplyr::filter(var %in% c("aboveground_total_live", "pot_smoke_sev")) %>%
  # convert hectacre to acres
  dplyr::mutate(
    area = units::set_units(area, "ha"),
    area = units::set_units(area, "acre"),
    area = units::drop_units(area),
    #they do NOT do per pixel area calcs, they just sum tons/acre values
    #valuearea = value * area
  )
atl_smoke_delta <- atl_smoke %>%
  # calc total atl or smoke per pixel
  dplyr::group_by(scenario, year, var, proj_id) %>%
  dplyr::summarize(
    #sum_valuearea = sum(valuearea, na.rm = TRUE),
    sum_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(year, var, proj_id),
    names_from = scenario,
    values_from = sum_value
  ) %>%
  #they call percent change delta
  dplyr::mutate(delta = (Legalmax - Baseline) / Baseline * 100) %>%
  #pivot back long
  tidyr::pivot_longer(
    cols = c(Baseline, Legalmax, delta),
    names_to = "outname"
  )
atl_smoke_recalc <- atl_smoke_delta %>%
  #make something that lines up with geopackage (though after it is pivoted)
  dplyr::mutate(
    outname = stringr::str_to_lower(outname),
    outname = dplyr::if_else(outname == "legalmax", "value", outname)
  ) %>%
  dplyr::left_join(name_xwalk, by = join_by(var)) %>%
  dplyr::mutate(ps_var_full = paste0(ps_var, "_", year, "_", outname)) %>%
  dplyr::rename(recalc_value = value) %>%
  dplyr::select(proj_id, ps_var_full, recalc_value)

# aboveground total and smoke Planscape
atl_smoke_ps <- proj_long %>%
  dplyr::filter(stringr::str_detect(ps_var_full, "^ABOVEGROUND|^POTENTIAL")) %>%
  dplyr::rename(for_value = value)

atl_smoke_jt <- atl_smoke_ps %>%
  dplyr::left_join(
    atl_smoke_recalc,
    by = join_by(proj_id, ps_var_full)
  ) %>%
  tidyr::pivot_longer(
    cols = c(for_value, recalc_value),
    names_to = "source"
  ) %>%
  tidyr::separate_wider_delim(
    ps_var_full,
    "_",
    names = c("first", "second", "year", "valuetype")
  ) %>%
  dplyr::mutate(metric = paste(first, second)) %>%
  dplyr::select(-c(first, second)) %>%
  dplyr::select(proj_id, metric, everything()) %>%
  dplyr::mutate(
    valuetype = factor(
      valuetype,
      levels = c("baseline", "value", "delta"),
      ordered = TRUE
    )
  )

# Graphing - loop per proj_id
proj_id_list <- atl_smoke_jt$proj_id %>% unique() %>% sort()
for (i in seq_along(proj_id_list)) {
  this_proj_id <- proj_id_list[[i]]
  this_atl_smoke_jt <- atl_smoke_jt %>%
    dplyr::filter(proj_id == this_proj_id)
  this_p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = this_atl_smoke_jt,
      mapping = aes(x = year, y = value, fill = source),
      position = "dodge2"
    ) +
    ggplot2::scale_fill_manual(
      values = c(for_value = "tan3", recalc_value = "turquoise4")
    ) +
    ggplot2::facet_wrap(~ metric + valuetype, scales = "free_y", nrow = 2) +
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::labs(
      title = paste0("Project ID ", this_proj_id),
      caption = tools::file_path_sans_ext(gpkg_name)
    )

  if (resave) {
    ggplot2::ggsave(
      plot = this_p,
      filename = file.path(
        folder_out,
        paste0("FOR_R_compare_atl_smoke_proj", this_proj_id, ".jpg")
      ),
      height = 5,
      width = 8,
      units = c("in")
    )
  }
}

### Water ----------------------------------------------------------------------
# Water planning area
# not concerned with values, just with area of planning area
# this is our denominator in % of planning area above user threshold
#  uncertain how PS does this, should be similar enough
plan_acres <- plan_info %>%
  dplyr::filter(center_incl == 1) %>%
  # convert hectacre to acres
  dplyr::mutate(
    area = units::set_units(area, "ha"),
    area = units::set_units(area, "acre"),
    area = units::drop_units(area),
    #area_exact = area * coverage_fraction #no exact
  ) %>%
  dplyr::summarize(plan_area = sum(area)) %>%
  dplyr::pull(plan_area)

#plan$AET_planning_area_acres

# percentage improvement user-set threshold
w_threshold <- plan$AET_percentage

# Water (proj area)
proj_id_list <- all_pxs$proj_id %>% unique() %>% sort()
water_proj <- all_pxs %>%
  # filter to only center included data pixels
  dplyr::filter(center_incl == 1) %>%
  dplyr::filter(var %in% c("water_pchange")) %>%
  # convert hectacre to acres
  dplyr::mutate(
    area = units::set_units(area, "ha"),
    area = units::set_units(area, "acre"),
    area = units::drop_units(area)
  )
proj_areas <- water_proj %>%
  dplyr::group_by(proj_id) %>%
  dplyr::summarize(tot_area = sum(area))

water_proj_recalc <- water_proj %>%
  dplyr::filter(value >= w_threshold) %>%
  dplyr::group_by(proj_id) %>%
  dplyr::summarize(improved_area = sum(area)) %>%
  # RIGHT join to get any proj areas without any improvement
  dplyr::right_join(proj_areas, by = dplyr::join_by(proj_id)) %>%
  dplyr::mutate(
    improved_area = dplyr::if_else(is.na(improved_area), 0, improved_area),
    improved_percent = improved_area / tot_area * 100
  ) %>%
  dplyr::rename(
    AET_total_acres = tot_area,
    AET_improved_acres = improved_area,
    AET_improved_area_percent = improved_percent
  ) %>%
  tidyr::pivot_longer(
    cols = -proj_id,
    names_to = "ps_var_full",
    values_to = "value"
  )

# While not the displayed values, first let's check the proj-level values
water_proj_ps <- proj_long %>%
  #note: it's not AET, but wording during metric development was very confused
  dplyr::filter(stringr::str_detect(ps_var_full, "^AET")) %>%
  dplyr::rename(for_value = value)

# Joint at PROJECT level
water_proj_jt <- water_proj_ps %>%
  dplyr::left_join(
    water_proj_recalc,
    by = dplyr::join_by(proj_id, ps_var_full)
  ) %>%
  dplyr::rename(recalc_value = value) %>%
  tidyr::pivot_longer(cols = c(for_value, recalc_value), names_to = "source")

p_water_proj <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = water_proj_jt,
    mapping = aes(x = source, y = value, fill = source)
  ) +
  ggplot2::scale_fill_manual(
    values = c(for_value = "tan3", recalc_value = "turquoise4")
  ) +
  ggplot2::facet_wrap(
    ~ ps_var_full + proj_id,
    scales = "free_y",
    ncol = length(proj_id_list)
  ) +
  ggplot2::theme_bw() +
  ggplot2::ylab("") +
  ggplot2::labs(
    title = paste0("Water availability with threshold of ", w_threshold, "%"),
    subtitle = "At Project Area level",
    caption = paste0(
      "Note: Variable names include 'AET' but these are water availability.\n",
      tools::file_path_sans_ext(gpkg_name)
    )
  )
#if more than 3 proj areas, move legend to bottom
if (length(proj_id_list) > 3) {
  p_water_proj <- p_water_proj +
    ggplot2::theme(legend.position = "bottom")
}

if (resave) {
  ggplot2::ggsave(
    plot = p_water_proj,
    filename = file.path(
      folder_out,
      "FOR_R_compare_water_proj.jpg"
    ),
    height = 5,
    #quick automatic scaling factor depending on the number of project areas
    width = 11 * (length(proj_id_list) / 3) * 0.75,
    units = c("in")
  )
}

# Summarize proj to plan for what is displayed in PS when all proj areas selected
water_recalc_toplan <- water_proj_recalc %>%
  dplyr::filter(!ps_var_full == "AET_improved_area_percent") %>%
  dplyr::group_by(ps_var_full) %>%
  dplyr::summarize(value = sum(value)) %>%
  tidyr::pivot_wider(names_from = ps_var_full, values_from = value) %>%
  dplyr::rename(AET_total_project_area_acres = AET_total_acres) %>%
  dplyr::mutate(
    AET_planning_area_acres = .env$plan_acres,
    AET_improved_area_percent = AET_improved_acres /
      AET_planning_area_acres *
      100
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "ps_var_full",
    values_to = "recalc_value"
  )

water_plan_jt <- plan_long %>%
  dplyr::filter(stringr::str_detect(ps_var_full, "^AET")) %>%
  dplyr::rename(for_value = value) %>%
  dplyr::left_join(water_recalc_toplan, by = join_by(ps_var_full)) %>%
  dplyr::filter(!ps_var_full == "AET_percentage") %>%
  tidyr::pivot_longer(cols = c(for_value, recalc_value), names_to = "source")

p_water <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = water_plan_jt,
    mapping = aes(x = source, y = value, fill = source)
  ) +
  ggplot2::scale_fill_manual(
    values = c(for_value = "tan3", recalc_value = "turquoise4")
  ) +
  ggplot2::facet_wrap(~ps_var_full, scales = "free_y") +
  ggplot2::theme_bw() +
  ggplot2::ylab("") +
  ggplot2::labs(
    title = paste0("Water availability with threshold of ", w_threshold, "%"),
    subtitle = "At Planning Area level",
    caption = paste0(
      "Note: Variable names include 'AET' but these are water availability.\n",
      tools::file_path_sans_ext(gpkg_name)
    )
  )

if (resave) {
  ggplot2::ggsave(
    plot = p_water,
    filename = file.path(
      folder_out,
      "FOR_R_compare_water_plan.jpg"
    ),
    height = 5,
    width = 8,
    units = c("in")
  )
}

### Flame Length ---------------------------------------------------------------

# flame length: g7l4, g6l4, g4l2
#  comparing baseline to legalmax, for that year
#  % of area
# Cheating by using cell numbers to link up baseline to legalmax
#  only works because same size rasters, same crop, same polygons
#   probably highly unrecommended.
# Otherwise would have needed to have seperate oop by year for tot flame
#  and pull in both rasters at once.

fl <- all_pxs %>%
  # filter to only center included pixels
  dplyr::filter(center_incl == 1) %>%
  dplyr::filter(var %in% c("tot_flame_sev")) %>%
  # convert hectacre to acres
  dplyr::mutate(
    area = units::set_units(area, "ha"),
    area = units::set_units(area, "acre"),
    area = units::drop_units(area)
  )
fl_wide <- fl %>%
  dplyr::select(-c(var, center_incl)) %>%
  tidyr::pivot_wider(
    id_cols = c(year, proj_id, cell),
    names_from = scenario,
    names_sep = "_",
    values_from = c(value, area)
  )
# flame length: g7l4, g6l4, g4l2
fl_class <- fl_wide %>%
  #spot checked baseline and legalmax same, as it should be
  dplyr::select(-area_Legalmax) %>%
  #project area acres
  dplyr::group_by(proj_id, year) %>%
  dplyr::mutate(
    tot_proj_area = sum(area_Baseline)
  ) %>%
  dplyr::ungroup() %>%
  #flame length reduction classes
  # categories are NOT mutually exclusive, need separate fields
  dplyr::mutate(
    f7_4 = dplyr::if_else(value_Baseline >= 7 & value_Legalmax <= 4, "7_4", NA),
    f6_4 = dplyr::if_else(value_Baseline >= 6 & value_Legalmax <= 4, "6_4", NA),
    f4_2 = dplyr::if_else(value_Baseline >= 4 & value_Legalmax <= 2, "4_2", NA)
  )

# separate summaries for each category
fl_red_cat <- dplyr::bind_rows(
  fl_class %>%
    dplyr::group_by(year, proj_id, f7_4, tot_proj_area) %>%
    dplyr::summarize(acres = sum(area_Baseline), .groups = "drop") %>%
    dplyr::filter(!is.na(f7_4)) %>%
    dplyr::rename(reduce_cat = f7_4),
  fl_class %>%
    dplyr::group_by(year, proj_id, f6_4, tot_proj_area) %>%
    dplyr::summarize(acres = sum(area_Baseline), .groups = "drop") %>%
    dplyr::filter(!is.na(f6_4)) %>%
    dplyr::rename(reduce_cat = f6_4),
  fl_class %>%
    dplyr::group_by(year, proj_id, f4_2, tot_proj_area) %>%
    dplyr::summarize(acres = sum(area_Baseline), .groups = "drop") %>%
    dplyr::filter(!is.na(f4_2)) %>%
    dplyr::rename(reduce_cat = f4_2)
) %>%
  dplyr::mutate(perc_proj = acres / tot_proj_area * 100) %>%
  dplyr::select(proj_id, reduce_cat, year, perc_proj, everything()) %>%
  dplyr::arrange(proj_id, reduce_cat, year)

#need to expand to all possible combinations
fl_expand <- fl_red_cat %>%
  tidyr::expand(
    year = all_pxs$year %>% unique() %>% sort(),
    reduce_cat = c("7_4", "6_4", "4_2"),
    nesting(proj_id, tot_proj_area)
  ) %>%
  dplyr::left_join(
    fl_red_cat,
    by = join_by(year, reduce_cat, proj_id, tot_proj_area)
  ) %>%
  dplyr::mutate(
    perc_proj = dplyr::if_else(is.na(perc_proj), 0, perc_proj),
    acres = dplyr::if_else(is.na(acres), 0, acres)
  )

# Recalculations in R, in PS-style for joining
fl_recalc <- fl_expand %>%
  #matching up with the fields as PS seems to use the labels
  dplyr::rename(
    delta = perc_proj,
    total_area = tot_proj_area,
    value = acres
  ) %>%
  dplyr::mutate(
    ps_var_stem = paste0("TOTAL_FLAME_SEVERITY_", reduce_cat, "_", year, "_")
  ) %>%
  dplyr::select(-c(reduce_cat, year)) %>%
  tidyr::pivot_longer(
    cols = c(delta, total_area, value),
    names_to = "ps_var_tail",
    values_to = "recalc_value"
  ) %>%
  dplyr::mutate(ps_var_full = paste0(ps_var_stem, ps_var_tail)) %>%
  dplyr::select(proj_id, ps_var_full, recalc_value) %>%
  dplyr::arrange(proj_id, ps_var_full)

#PS funding report download
fl_proj_ps <- proj_long %>%
  dplyr::filter(stringr::str_detect(ps_var_full, "^TOTAL_FLAME")) %>%
  # raw_value and value appear to be the same, George didn't know the difference
  dplyr::filter(stringr::str_detect(
    ps_var_full,
    "_raw_value$",
    negate = TRUE
  )) %>%
  # also baseline and total_area seem to be the same as well.
  dplyr::filter(stringr::str_detect(
    ps_var_full,
    "_baseline$",
    negate = TRUE
  )) %>%
  dplyr::rename(for_value = value)

#join R recalc and PS FOR geopackage data, prep for graphing
fl_jt <- fl_proj_ps %>%
  dplyr::left_join(fl_recalc, by = join_by(proj_id, ps_var_full)) %>%
  # pivot and split out names for graphing
  tidyr::pivot_longer(
    cols = c(for_value, recalc_value),
    names_to = "source"
  ) %>%
  tidyr::separate_wider_delim(
    ps_var_full,
    "_",
    names = c(NA, NA, NA, "from", "to", "year", "valuetype", "second_vt"),
    #total_area has an underscore
    too_few = "align_start"
  ) %>%
  dplyr::mutate(
    valuetype = dplyr::if_else(
      !is.na(second_vt),
      paste0(valuetype, "_", second_vt),
      valuetype
    )
  ) %>%
  #mutate metric with from to
  dplyr::mutate(metric = paste0("gte", from, "_lte", to)) %>%
  dplyr::mutate(
    valuetype = factor(
      valuetype,
      levels = c("value", "total_area", "delta"),
      ordered = TRUE
    )
  ) %>%
  dplyr::select(-c(second_vt, from, to))


# Graphing - loop per proj_id
proj_id_list <- fl_jt$proj_id %>% unique() %>% sort()
for (i in seq_along(proj_id_list)) {
  this_proj_id <- proj_id_list[[i]]
  this_fl_jt <- fl_jt %>%
    dplyr::filter(proj_id == this_proj_id)
  this_p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = this_fl_jt,
      mapping = aes(x = year, y = value, fill = source),
      position = "dodge2"
    ) +
    ggplot2::scale_fill_manual(
      values = c(for_value = "tan3", recalc_value = "turquoise4")
    ) +
    ggplot2::facet_wrap(~ metric + valuetype, scales = "free_y", nrow = 3) +
    # 3 values per flame class reduction metric
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::labs(
      title = "Flame length reduction categories",
      subtitle = paste0("Project ID ", this_proj_id),
      caption = paste0(
        "value = acres with that reduction;\n delta = percent of project area with reduction\n",
        tools::file_path_sans_ext(gpkg_name)
      )
    )

  if (resave) {
    ggplot2::ggsave(
      plot = this_p,
      filename = file.path(
        folder_out,
        paste0("FOR_R_compare_flame_length_proj", this_proj_id, ".jpg")
      ),
      height = 6,
      width = 8,
      units = c("in")
    )
  }
}

### Biomass --------------------------------------------------------------------

# merch in bf/acre, and nonmerch in cuft/acre
# convert to totals
# use both cellsize,
# and PS hard-coded constant (0.2224 per AI engineers for 30x30) (????)

# Biomass (proj area)
biomass_vars <- c(
  "bdft-softwood",
  "bdft-hardwood",
  "bdft-mixed",
  "nonmerch_cuft-softwood",
  "nonmerch_cuft-hardwood",
  "nonmerch_cuft-mixed"
)
proj_id_list <- all_pxs$proj_id %>% unique() %>% sort()
biomass_pxs <- all_pxs %>%
  # filter to only center included data pixels
  dplyr::filter(center_incl == 1) %>%
  dplyr::filter(var %in% biomass_vars) %>%
  # convert hectacre to acres
  dplyr::mutate(
    area = units::set_units(area, "ha"),
    area = units::set_units(area, "acre"),
    area = units::drop_units(area)
  ) %>%
  #only cells with values
  # will lose categories without values, will get back in ps join
  # if set NA to 0, then area sum would be full area not wood type area
  dplyr::filter(!is.na(value)) %>%
  #mult by area for bf or cuft (not per acre)
  dplyr::mutate(biomass = value * area)

biomass_proj_sum <- biomass_pxs %>%
  #only baseline scenario, 2026 already
  dplyr::group_by(proj_id, var) %>%
  dplyr::summarize(
    biomass = sum(biomass),
    tot_area = sum(area),
    per_acre_sum = sum(value),
    .groups = "drop"
  ) %>%
  #hard coded value calc
  dplyr::mutate(biomass_hc = per_acre_sum * 0.2224)

biomass_proj_recalc <- biomass_proj_sum %>%
  dplyr::rename("recalc" = biomass, "rc_hardcode" = biomass_hc) %>%
  dplyr::select(-c(tot_area, per_acre_sum)) %>%
  dplyr::left_join(name_xwalk, by = join_by(var)) %>%
  dplyr::rename(ps_var_full = ps_var) %>%
  dplyr::select(c(proj_id, ps_var_full, recalc, rc_hardcode))
#not long yet, since need to add ps for value first

#funding report
biomass_ps <- proj_long %>%
  dplyr::filter(stringr::str_detect(ps_var_full, "^BIOMASS")) %>%
  dplyr::rename(for_value = value)

biomass_jt <- biomass_ps %>%
  dplyr::left_join(
    biomass_proj_recalc,
    by = join_by(proj_id, ps_var_full)
  ) %>%
  #convert recalc NAs to 0s (as empty categories were dropped in earlier calc)
  tidyr::replace_na(list(recalc = 0, rc_hardcode = 0)) %>%
  # now with all three go long
  tidyr::pivot_longer(
    cols = c(for_value, recalc, rc_hardcode),
    names_to = "source"
  ) %>%
  #get shorter var names for presenting
  #first make "non_merchantable" into something without "_", and shorter
  dplyr::mutate(
    ps_var_full = gsub("non_merchantable", "nonmerch", ps_var_full),
    ps_var_full = gsub("merchantable", "merch", ps_var_full)
  ) %>%
  tidyr::separate_wider_delim(
    ps_var_full,
    "_",
    names = c(NA, NA, "merch_type", "wood_type", NA)
  )

# graphing
# Graphing - loop per proj_id
for (i in seq_along(proj_id_list)) {
  this_proj_id <- proj_id_list[[i]]
  this_jt <- biomass_jt %>%
    dplyr::filter(proj_id == this_proj_id)
  this_p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = this_jt,
      mapping = aes(x = wood_type, y = value, fill = source),
      position = "dodge2"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        for_value = "tan3",
        recalc = "turquoise4",
        rc_hardcode = "darkblue"
      )
    ) +
    ggplot2::facet_wrap(~merch_type, scales = "free_y", nrow = 1) +
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::labs(
      title = paste0("Project ID ", this_proj_id),
      caption = tools::file_path_sans_ext(gpkg_name),
      y = "bf (merch) or cuft (nonmerch)"
    )

  if (resave) {
    ggplot2::ggsave(
      plot = this_p,
      filename = file.path(
        folder_out,
        paste0("FOR_R_compare_biomass_proj", this_proj_id, ".jpg")
      ),
      height = 5,
      width = 8,
      units = c("in")
    )
  }
}

### Treatment type area --------------------------------------------------------
# this is the one that uses touched = TRUE!
# so any pixel level coverage, not just center-included
# weighted by coverage pixel area

tx_px <- all_pxs %>%
  #the treatment key raster
  dplyr::filter(
    var == "tx_key",
    #only rows(pixels) with a treatment value
    !is.na(value)
  ) %>%
  # calculate fractional area
  # first convert hectacre to acres
  dplyr::mutate(
    area = units::set_units(area, "ha"),
    area = units::set_units(area, "acre"),
    area = units::drop_units(area),
    #now fractional area based on pixel coverage of project polygon
    area_frac = area * coverage_fraction
  ) %>%
  #not going by center_incl, but including here to do summary comparisons
  dplyr::select(c(proj_id, value, area_frac, center_incl))

# in placerville, 3-7 acres difference
#tx_px %>%
#  dplyr::group_by(proj_id, value, center_incl) %>%
#  dplyr::summarize(sum_area = sum(area_frac))

tx_xwalk <- tibble::tribble(
  ~value , ~ps_var_full                      ,
       1 , "rx???"                           ,
       2 , "TREATMENT_AREA_Thin_and_Rx_Burn"
)


tx_recalc <- tx_px %>%
  dplyr::group_by(proj_id, value) %>%
  dplyr::summarize(area_recalc = sum(area_frac), .groups = "drop")


# funding report
tx_ps <- proj_long %>%
  dplyr::filter(stringr::str_detect(ps_var_full, "^TREATMENT")) %>%
  dplyr::rename(for_value = value)


# join FOR and recalc values
tx_ps %>%
  dplyr::left_join(
    tx_recalc,
    by = join_by(proj_id, ps_var_full)
  ) %>%
  #convert recalc NAs to 0s (as empty categories were dropped in earlier calc)
  tidyr::replace_na(list(recalc = 0, rc_hardcode = 0)) %>%
  # now with all three go long
  tidyr::pivot_longer(
    cols = c(for_value, recalc, rc_hardcode),
    names_to = "source"
  )
