# water availability percent change
#  frequency & landscape proportions
# for various 'significant' cut-offs

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### settings -------------------------------------------------------------------

folder_qa <- file.path("data", "qa")


### Data in --------------------------------------------------------------------

folder_wa <- file.path("data", "output", "water_availability")
wapc <- terra::rast(file.path(
  folder_wa,
  "water_avail_perc_change_legalmax_fvsmasked.tif"
))


### Frequencies & histograms ---------------------------------------------------

#default freq rounds to integer
freq_pc <- terra::freq(wapc) %>% as_tibble()

p_hist <- ggplot(data = freq_pc, aes(value)) +
  geom_histogram(aes(weight = count), binwidth = 1) +
  ylab("Count of pixels") +
  xlab("Water availability percent change (baseline to legalmax)") +
  theme_bw() +
  coord_cartesian(xlim = c(-25, 50))

ggplot2::ggsave(
  plot = p_hist,
  file.path(folder_qa, "wateravail_pchange_histogram.jpg"),
  height = 4,
  width = 6,
  units = c("in")
)


freq_pc %>%
  dplyr::mutate(signif = if_else(abs(value) > 10, TRUE, FALSE)) %>%
  dplyr::group_by(signif) %>%
  dplyr::summarize(pixels = sum(count)) %>%
  dplyr::mutate(perctot = pixels / sum(pixels) * 100)
# 2026 08 run
# 10% 90.9%

# 2026 May-ish run. (prot2 issue)
# 5% 98.9%
# 10% 97.6%
