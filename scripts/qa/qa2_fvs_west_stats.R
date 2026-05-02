# qa
# global min, mean, max change
# and pixel counts

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

folder_mos <- file.path("data", "output", "fvs_mosaic")

diff_files <- list.files(
  path = folder_mos,
  pattern = "Difference_.*\\.tif$",
  recursive = FALSE,
  full.names = TRUE
)

### Target & loop for stats ----------------------------------------------------

targets <- diff_files %>%
  tibble::as_tibble() %>%
  dplyr::rename(fullpath = value) %>%
  dplyr::mutate(
    filenm = tools::file_path_sans_ext(basename(fullpath)),
    year = stringr::str_split_i(filenm, "_", 2),
    fvsvar = stringr::str_split_i(filenm, "_[0-9]{4}_", 2)
  ) %>%
  dplyr::select(-filenm)

stat_collector <- vector(length = nrow(targets), "list")

for (i in 1:nrow(targets)) {
  this_row <- targets[i, ]
  this_rast <- terra::rast(this_row[["fullpath"]])

  #global min, mean, max, notNA.
  this_stats <- terra::global(
    this_rast,
    c("mean", "range", "notNA"),
    na.rm = TRUE
  ) %>%
    tibble::as_tibble()

  #all data for output
  stat_collector[[i]] <- cbind(this_row, this_stats)
}

all_stats <- dplyr::bind_rows(stat_collector) %>%
  dplyr::arrange(fvsvar, year)

# Check that all notNA are the same
# Yes, all 809085611

# facet graph
long_stats <- all_stats %>%
  dplyr::select(-c(fullpath, notNA)) %>%
  tidyr::pivot_longer(
    cols = c(min, mean, max),
    values_to = "value",
    names_to = "stat"
  )
p_stats <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = long_stats,
    aes(x = year, y = value, color = stat)
  ) +
  ggplot2::scale_color_viridis_d(
    "Statistic",
    direction = -1,
    begin = 0.1
  ) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~fvsvar, scales = "free_y") +
  ggplot2::ylab("Difference (Treated - Baseline)") +
  ggplot2::xlab("") +
  ggplot2::ggtitle(
    "Westwide global statistics",
    subtitle = "Baseline subtracted from Legalmax treated"
  )
p_stats
ggplot2::ggsave(
  plot = p_stats,
  file.path("data", "qa", "mosaic", "westwide_stats_20260430.jpg"),
  width = 8,
  height = 5,
  units = c("in")
)

# graph rest, per variable
#fvsvars <- all_stats %>% pull(fvsvar) %>% unique()
# for (i in seq_along(fvsvars)) {
#   this_var <- fvsvars[[i]]
#   this_stats <- all_stats %>%
#     dplyr::filter(fvsvar == this_var) %>%
#     dplyr::select(-c(fullpath, notNA)) %>%
#     tidyr::pivot_longer(
#       cols = c(min, mean, max),
#       values_to = "value",
#       names_to = "stat"
#     )
#   ggplot() +
#     geom_point(data = this_stats, aes(x = year, y = value, color = stat)) +
#     theme_bw()
# }

### SPECIFIC -------------------------------------------------------------------

# ATL 2041 has at least one really high max
# get frequencies (of diff)
atl41_tg <- targets %>%
  dplyr::filter(fvsvar == "aboveground_total_live", year == 2041)
atl41_r <- terra::rast(atl41_tg[["fullpath"]])
atl41_freq <- terra::freq(atl41_r, digits = 0) %>% tibble::as_tibble()

atl41_freq %>% dplyr::filter(value > 200)
