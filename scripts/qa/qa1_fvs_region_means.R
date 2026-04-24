# Plotting Dave's QA tables of FVS results
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  viridis
)

### Settings -------------------------------------------------------------------

#base output folder
folder_out <- file.path("data", "output", "qa")

### Data in --------------------------------------------------------------------

# folder csv tables of region means of FVS results, e.g. PC612_R1_means.csv
# NOTE: 2 headers
folder_csv <- file.path("data", "fvs_qa")

files_means <- list.files(
  path = folder_csv,
  pattern = "means.csv$",
  full.names = TRUE
)

# Set up specific colors (not all regions have all thins and want consistent colors)
txs = c("baseline", "rxb", "thin-1", "thin-2", "thin-3")
tx_colors <- viridis::turbo(n = 5, begin = 0.2)
names(tx_colors) <- txs

### Loop on files (region) -----------------------------------------------------

for (i in seq_along(files_means)) {
  this_file <- files_means[[i]]
  this_regn <- stringr::str_split_i(basename(this_file), pattern = "_", i = 2)

  #2 headers, so handlng in two separate reads
  this_headers <- readr::read_csv(this_file, n_max = 2, col_names = FALSE)
  combined_headers <- sapply(this_headers, paste, collapse = "_")
  #replace X1 with actual header (not "Year_Treatment" dual header info)
  combined_headers[["X1"]] <- "fvs_variable"
  #read data and attached combined headers
  this_data_raw <- readr::read_csv(this_file, skip = 2, col_names = FALSE)
  names(this_data_raw) <- combined_headers

  #pivot this very wide data to long for graphing with ggplot
  data_long <- tidyr::pivot_longer(
    this_data_raw,
    cols = !fvs_variable,
    names_sep = "_",
    names_to = c("year", "treatment")
  )

  #plotting
  plot_reg <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = data_long,
      aes(x = year, y = value, color = treatment, group = treatment)
    ) +
    # scale_color_viridis(
    #   "Treatment",
    #   discrete = TRUE,
    #   option = "turbo",
    #   begin = 0.1
    # ) +
    ggplot2::scale_color_manual("Treatment", values = tx_colors) +
    ggplot2::facet_wrap(~fvs_variable, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("Region means: ", this_regn))
  ggplot2::ggsave(
    plot = plot_reg,
    file.path(folder_out, paste0("qa_means_", this_regn, ".jpg")),
    height = 5,
    width = 8,
    units = c("in")
  )
}
