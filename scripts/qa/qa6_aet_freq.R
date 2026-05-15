# AET frequency & landscape proportions
# for various 'significant' cut-offs

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Data in --------------------------------------------------------------------

#all pixel water availability
folder_wa <- file.path(
  "data",
  "output",
  "water_availability"
)

aetd <- terra::rast(file.path(
  folder_wa,
  "AET_legalmax_difference_fvsmasked.tif"
))
aetlm <- terra::rast(file.path(
  folder_wa,
  "AET_legalmax_fvsmasked.tif"
))
aetbl <- terra::rast(file.path(
  folder_wa,
  "AET_baseline_fvsmasked.tif"
))

### Percent change -------------------------------------------------------------

aet_pc <- (aetlm - aetbl) / aetbl * 100

### Frequencies & histograms ---------------------------------------------------

#default freq rounds to integer
freq_d <- terra::freq(aetd) %>% as_tibble()

ggplot(data = freq_d, aes(value)) +
  geom_histogram(aes(weight = count), binwidth = 10) +
  ylab("Count of pixels") +
  xlab("AET Difference (legalmax minus baseline") +
  theme_bw()

ggplot(data = freq_d, aes(value)) +
  geom_histogram(
    aes(weight = count, y = after_stat(count / sum(count))),
    binwidth = 10
  ) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent of total pixels") +
  xlab("AET Difference (legalmax minus baseline") +
  theme_bw()

#freq_d %>% dplyr::filter(value > 0) %>% arrange(desc(value))
# a few values > positive 50
freq_d %>%
  dplyr::mutate(signif = if_else(abs(value) > 25, TRUE, FALSE)) %>%
  dplyr::group_by(signif) %>%
  dplyr::summarize(pixels = sum(count)) %>%
  dplyr::mutate(perctot = pixels / sum(pixels) * 100)
# 50: 1.9% nonsignificant, 98.1% significant
# 25: 1% ns, 99% sig

#terra::hist(aetd) #samples

freq_pc <- terra::freq(aet_pc) %>% as_tibble()

ggplot(data = freq_pc, aes(value)) +
  geom_histogram(aes(weight = count), binwidth = 1) +
  ylab("Count of pixels") +
  xlab("AET percent change (baseline to legalmax") +
  theme_bw() +
  coord_cartesian(xlim = c(25, -50))

freq_pc %>%
  dplyr::mutate(signif = if_else(abs(value) > 10, TRUE, FALSE)) %>%
  dplyr::group_by(signif) %>%
  dplyr::summarize(pixels = sum(count)) %>%
  dplyr::mutate(perctot = pixels / sum(pixels) * 100)
# 5% 98.9%
# 10% 97.6%
