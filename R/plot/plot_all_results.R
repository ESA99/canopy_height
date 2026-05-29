### Plot pixel shuffle differences ###

invisible(lapply(c("terra","dplyr","purrr","ggplot2","ggrepel","viridis","tidyterra"), 
                 require, character.only = TRUE))

tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", "35VML" = "Finland", 
  "49NHC" = "Malaysia", "49UCP" = "Mongolia","34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West"
)


# Result Tables ----------------------------------------------------------
shuffle_results <- read.csv("results/2026-05-28_shuffle_1/results.csv") |>
  mutate(
    Location = factor(tile, levels = names(tile_label), labels = tile_label)
)

spectral_results <- read.csv("results/2026-05-29_spectral_1/results.csv") |>
  mutate(
    Location = factor(tile, levels = names(tile_label), labels = tile_label),
    increment = increment*100,
    abs_increment = abs(increment)
)

geo_results <- read.csv("results/2026-06-01_geographical_1/results.csv") |>
  mutate(
    Location = factor(tile, levels = names(tile_label), labels = tile_label)
)

# Plot Functions -------------------------------------------------------------------
source("R/plot/plot_functions.R")

# Saving the Plots in different ratios -----------------------------------

# Wide save:
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_wide.png"), width = 270, height = 175, units = "mm", dpi = 300, bg = "white")
# Medium save:
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
# Tall save
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_tall.png"), width = 200, height = 250, units = "mm", dpi = 300, bg = "white")


# Spectral ---------------------------------------------------------------

plot_spectral_butterfly(spectral_results, y_var = "mean_change", y_lab = "Average Difference [m]")

plot_spectral_labels(spectral_results, y_var = "relative_mean_change", y_lab = "Average Relative Difference [%]")

plot_spectral_facets(spectral_results, y_var = "relative_mean_change", y_lab = "Average Relative Difference [%]")

# Plot is grey at smalls scale test at the moment...
plot_location_band(spectral_results, band_name = "Blue", y_var = "mean_change", y_lab = "Average Difference [m]")

# Shuffle ----------------------------------------------------------------

plot_shuffle(shuffle_results, "mean_change", "Average difference [m]")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle(shuffle_results, "relative_mean_change", "Average relative difference [%]")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle(shuffle_results, "mean_abs_change", "Absolute average difference [m]")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle(shuffle_results, "relative_mean_abs_change", "Absolute average relative difference [%]")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")

plot_shuffle_byTile(shuffle_results, "mean_change", "Average difference [m]", TRUE)
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle_byTile(shuffle_results, "relative_mean_change", "Average relative difference [%]", TRUE, 8)
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle_byTile(shuffle_results, "mean_abs_change", "Absolute average difference [m]", TRUE, avg_nudge = 10)
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle_byTile(shuffle_results, "relative_mean_abs_change", "Absolute average relative difference [%]", TRUE, avg_nudge = 9)
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")




# Geographical -----------------------------------------------------------






