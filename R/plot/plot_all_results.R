### Plot pixel shuffle differences ###

invisible(lapply(c("terra","dplyr","purrr","ggplot2","ggrepel",
                   "viridis","tidyterra"), 
                    require, character.only = TRUE))

source("R/deploy/info_tables.R")
source("R/plot/plot_functions.R")
source("R/tools/tools.R")


# Result Tables ----------------------------------------------------------
spectral_results <- read.csv("results/2026-06_spectral_main.csv")

geo_results <- read.csv("results/2026-06_geo_main.csv")

shuffle_results <- read.csv("results/2026-06_shuffle_main.csv")



# Spectral ---------------------------------------------------------------

plot_spectral_labels(spectral_results, y_var = "relative_mean_change", y_lab = "Average Relative Difference [%]")
plot_spectral_labels(spectral_results, y_var = "mean_change", y_lab = "Average Difference [m]")
save_spectral_plot(filename, "medium")

plot_spectral_butterfly(spectral_results, y_var = "mean_change", y_lab = "Average Difference [m]")

plot_spectral_facets(spectral_results, y_var = "mean_change",   y_lab = "Average Difference [m]")
save_spectral_plot(filename, "tall")
plot_spectral_facets(spectral_results, y_var = "relative_mean_change",   y_lab = "Average Relative Difference [%]")
save_spectral_plot(filename, "tall")

plot_spectral_band(spectral_results, band_name = "Green",  
  y_var = "mean_change", y_lab = "Average Difference [m]",
  x_var = "increment")

plot_spectral_band( spectral_results, band_name = "RedEdge", 
  y_var = "mean_change", y_lab = "Average Difference [m]", 
  x_var = "abs_increment")

save_spectral_plot(filename, "medium")


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
save_shuffle_plot(filename, "medium")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle_byTile(shuffle_results, "relative_mean_change", "Average relative difference [%]", TRUE, 8)
save_shuffle_plot(filename, "medium")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle_byTile(shuffle_results, "mean_abs_change", "Absolute average difference [m]", TRUE, avg_nudge = 10)
save_shuffle_plot(filename, "medium")
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
plot_shuffle_byTile(shuffle_results, "relative_mean_abs_change", "Absolute average relative difference [%]", TRUE, avg_nudge = 9)
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")


plot_shuffle_heatmap(shuffle_results, "mean_change")
save_shuffle_plot(filename, "medium")

plot_shuffle_heatmap_discrete(shuffle_results, "mean_change")
save_shuffle_plot(filename, "medium")

plot_shuffle_heatmap_tiles(shuffle_results, "mean_change", "Average Difference [m]")
save_shuffle_plot(filename, "medium")


# Geographical -----------------------------------------------------------

plot_geo_shift(geo_results, y_var = "mean_change", y_lab = "Average Difference [m]")
save_geo_plot(filename, "medium")

plot_geo_byTile(geo_results, y_var = "mean_change", y_lab = "Average Difference [m]")
save_geo_plot(filename, "wide")

plot_geo_latitude(geo_results,tile_coordinates, y_var = "mean_change", y_lab = "Average Difference [m]")
save_geo_plot(filename, "wide")


plot_geo_equator_trend(geo_shifted, y_var = "mean_change", y_lab = "Mean change") #,tile_colors = tile_colors
save_geo_plot(filename, "medium")

plot_geo_tile_trends(geo_shifted, y_var = "mean_change", y_lab = "Mean change")
save_geo_plot(filename, "medium")

plot_geo_main_trend(geo_shifted, y_var = "mean_change", y_lab = "Mean change")
save_geo_plot(filename, "medium")


