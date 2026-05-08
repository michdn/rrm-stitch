# Graphing QA stats
# see qa4_extract_wcs.R for data generation

# PART 1
# FVS results (ATL, pot_smoke, tot flame) percent change
# Mean, min, max, 25th percentile, 50th, 75th
# 5 years

# PART 2
# FVS results merchantable total biomass
# mcuft, tcuft
# Min, max, 25th, 50th, 75th
# Only 2026

# PART 3
# Water results
# acres with increase
# % of area with increase (with negative AET values)
# single raster, single 'year'

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  viridis
)

### Settings -------------------------------------------------------------------

#preventing scientific notation
options(scipen = 999)

#in/output folder
folder_qaz <- file.path("data", "output", "qa", "qa_zonal")

stamp <- format(Sys.time(), "%Y%m%d")

### Data in --------------------------------------------------------------------

stats_p1 <- read_csv(file.path(
  folder_qaz,
  "part1_fvs_percchange_stats_20260506.csv"
))

stats_p2 <- read_csv(file.path(
  folder_qaz,
  "part2_fvs_biomass_stats_20260506.csv"
))

stats_p3 <- read_csv(file.path(
  folder_qaz,
  "part3_AETwater_stats_20260506.csv"
))

### Part 1 graphs --------------------------------------------------------------

# a very large graph with 23 sites

# p1_long <- stats_p1 %>%
#   dplyr::select(NAME_SHORT, fvsvar, year, mean, min, max, q25, q50, q75) %>%
#   tidyr::pivot_longer(
#     cols = c(mean, min, max, q25, q50, q75),
#     names_to = "statistic",
#     values_to = "value"
#   )

# Manual boxplot

p1_atl <- stats_p1 %>% filter(fvsvar == "aboveground_total_live")
ggplot(
  data = p1_atl,
  mapping = aes(x = factor(year))
) +
  geom_boxplot(
    stat = "identity",
    aes(
      ymin = min,
      lower = q25,
      middle = q50,
      upper = q75,
      ymax = max
    )
  ) +
  geom_point(aes(y = mean), color = "blue") +
  facet_wrap(~NAME_SHORT, scales = "free_y") +
  theme_bw() +
  xlab("Year")

# loop by fvsvar
p1_fvsvars <- stats_p1 %>% dplyr::pull(fvsvar) %>% unique() %>% sort()

for (i in seq_along(p1_fvsvars)) {
  this_var <- p1_fvsvars[[i]]
  this_data <- stats_p1 %>%
    dplyr::filter(fvsvar == this_var)
  this_plot <- ggplot(
    data = this_data,
    mapping = aes(x = factor(year))
  ) +
    geom_boxplot(
      stat = "identity",
      aes(
        ymin = min,
        lower = q25,
        middle = q50,
        upper = q75,
        ymax = max
      )
    ) +
    geom_point(aes(y = mean), color = "blue") +
    facet_wrap(~NAME_SHORT, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("") +
    ggtitle(this_var, subtitle = "Percent change baseline to legalmax treated")
  ggsave(
    plot = this_plot,
    filename = file.path(folder_qaz, paste0("p1_boxplots_", this_var, ".jpg")),
    height = 10,
    width = 10,
    units = "in"
  )
  this_plot_simple <- ggplot(
    data = this_data,
    mapping = aes(x = factor(year))
  ) +
    geom_boxplot(
      stat = "identity",
      aes(
        ymin = q25,
        lower = q25,
        middle = q50,
        upper = q75,
        ymax = q75
      )
    ) +
    geom_point(aes(y = mean), color = "blue") +
    facet_wrap(~NAME_SHORT, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("") +
    ggtitle(this_var, subtitle = "Percent change baseline to legalmax treated")
  ggsave(
    plot = this_plot_simple,
    filename = file.path(
      folder_qaz,
      paste0("p1_boxplotcenter_", this_var, ".jpg")
    ),
    height = 10,
    width = 10,
    units = "in"
  )
  this_plot_simplefixed <- ggplot(
    data = this_data,
    mapping = aes(x = factor(year))
  ) +
    geom_boxplot(
      stat = "identity",
      aes(
        ymin = q25,
        lower = q25,
        middle = q50,
        upper = q75,
        ymax = q75
      )
    ) +
    geom_point(aes(y = mean), color = "blue") +
    facet_wrap(~NAME_SHORT, scales = "fixed") +
    theme_bw() +
    xlab("Year") +
    ylab("") +
    ggtitle(this_var, subtitle = "Percent change baseline to legalmax treated")
  ggsave(
    plot = this_plot_simplefixed,
    filename = file.path(
      folder_qaz,
      paste0("p1_boxplotcenterfixed_", this_var, ".jpg")
    ),
    height = 10,
    width = 10,
    units = "in"
  )
}

# tot flame sev special request
# #SWID too high compared to others
tf <- stats_p1 %>%
  dplyr::filter(fvsvar == "tot_flame_sev")
p_tf <- ggplot(
  data = tf,
  mapping = aes(x = factor(year))
) +
  geom_boxplot(
    stat = "identity",
    aes(
      ymin = q25,
      lower = q25,
      middle = q50,
      upper = q75,
      ymax = q75
    )
  ) +
  coord_cartesian(ylim = c(-100, 100)) +
  geom_point(aes(y = mean), color = "blue") +
  facet_wrap(~NAME_SHORT, scales = "fixed") +
  theme_bw() +
  xlab("Year") +
  ylab("") +
  ggtitle(
    "tot_flame_sev",
    subtitle = "Percent change baseline to legalmax treated"
  )
ggsave(
  plot = p_tf,
  filename = file.path(
    folder_qaz,
    "p1_boxplotcenterfixed_tot_flame_sev_special.jpg"
  ),
  height = 10,
  width = 10,
  units = "in"
)
