### Plot pixel shuffle differences ###

invisible(lapply(c("terra","dplyr","purrr","ggplot2","ggrepel","viridis","tidyterra"), 
                 require, character.only = TRUE))

source("R/deploy/info_tables.R")
source("R/plot/plot_functions.R")
source("R/tools/tools.R")


# Result Tables ----------------------------------------------------------
spectral_results <- read.csv("results/2026-06_spectral_main.csv")

geo_results <- read.csv("results/2026-06_geo_main.csv")

shuffle_results <- read.csv("results/2026-06_shuffle_main.csv")



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

# # Summary stat for plotting
# geo_summary <- geo_results %>%
#   group_by(shift_distance, shift_direction) %>%
#   summarise(
#     mean_change = mean(mean_change, na.rm = TRUE),
#     se = sd(mean_change, na.rm = TRUE) / sqrt(n()),
#     .groups = "drop"
#   )

ggplot( geo_results,
  aes(
    x = shift_distance,
    y = mean_change,
    color = shift_direction
  )
) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0
  ) +
  labs(
    x = "Shift distance (km)",
    y = "Mean change",
    color = "Direction"
  ) +
  theme_bw()

save_geo_plot("MeanChange_Line", "medium")



# By Tile
ggplot(
  geo_results,
  aes(
    x = shift_distance,
    y = mean_change,
    color = shift_direction,
    group = interaction(location, shift_direction)
  )
) +
  geom_line(alpha = 0.5) +
  geom_point() +
  facet_wrap(~location)+
  theme_minimal()

save_geo_plot("ShiftDist_MeanChange_TileFacett", "wide")



# Latitude ---------------------------------------------------------------

geo_shifted <- geo_results %>%
  left_join(
    tile_coordinates,
    by = c("tile" = "Name")
  ) %>%
  mutate(
    new_lat = case_when(
      shift_direction == "N" ~ lat + shift_distance / 111.32,
      shift_direction == "S" ~ lat - shift_distance / 111.32,
      TRUE ~ lat
    ),
    dist_equator = abs(new_lat)
  )

# Latitude by tile mean change
ggplot(
  geo_shifted,
  aes(
    x = new_lat,
    y = mean_change,
    group = tile,
    colour = tile
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +

  # highlight original position with same colour as line
  geom_point(
    data = subset(geo_shifted, original),
    size = 3,
    shape = 21,
    stroke = 1.2,
    fill = "white"
  ) +

  labs(
    x = "Latitude after shift",
    y = "Mean change",
    colour = "Tile"
  ) +
  theme_bw()

save_geo_plot("LAT_meanChange_byTile", "wide")

# lat_summary <- geo_shifted %>%
#   group_by(dist_equator) %>%
#   summarise(
#     mean_change = mean(mean_change, na.rm = TRUE),
#     se = sd(mean_change, na.rm = TRUE) / sqrt(n()),
#     .groups = "drop"
#   )




# Trend in relation to equator -------------------------------------------

ggplot(
  geo_shifted,
  aes(
    x = dist_equator,
    y = mean_change
  )
) +
  geom_point(
    aes(colour = tile),
    size = 2,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    colour = "black",
    linewidth = 1.2
  ) +
  labs(
    x = "Distance to equator (° latitude)",
    y = "Mean change",
    colour = "Tile"
  ) +
  theme_bw()

save_geo_plot("lat_equator_trend_colour", "medium")


geo_summary <- geo_shifted %>%
  group_by(dist_equator) %>%
  summarise(
    mean_change = mean(mean_change, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(geo_summary, aes(dist_equator, mean_change)) +
  geom_point(alpha = 0.25) +
  # geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  geom_smooth(method = "gam", formula = y ~ s(x), linewidth = 1.2, color = "darkgreen") +
  theme_bw() +
  labs(
    x = "Distance to equator (° latitude)",
    y = "Mean change"
  )

save_geo_plot("lat_equator_trend", "medium")

stop()




