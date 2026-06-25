library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
source("R/tools/tools.R")
source("R/deploy/info_tables.R")

### DATA ###
geo <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-16_geographical_1/results.csv")
# geo <- merge_backup_files("/results/runs/2026-06-04_geographical_1/loop_backups/", F)

geo <- add_location_column(geo, order.by.mean = FALSE)

geo$shift_direction <- ifelse(
        grepl("_[NS]$", geo$out_name),
        sub(".*_([NS])$", "\\1", geo$out_name),
        NA_character_
      )

# Duplicate originals for N and S
geo <- geo %>%
  rowwise() %>%
  reframe(
    across(everything()),
    shift_direction = if (is.na(shift_direction)) c("N", "S") else shift_direction
  )

# # Summary stat for plotting
# geo_summary <- geo %>%
#   group_by(shift_distance, shift_direction) %>%
#   summarise(
#     mean_change = mean(mean_change, na.rm = TRUE),
#     se = sd(mean_change, na.rm = TRUE) / sqrt(n()),
#     .groups = "drop"
#   )

ggplot( geo,
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
  geo,
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

geo_shifted <- geo %>%
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












# OLD APPROACH -----------------------------------------------------------

library(data.table)
library(ggplot2)

setDT(tile_coordinates)

geo <- merge(
  geo,
  tile_coordinates[, .(Name, lat)],
  by.x = "tile",
  by.y = "Name",
  all.x = TRUE
)

# create signed shift
geo[, signed_shift_km := fifelse(
  direction == "N", shift_distance,
  fifelse(direction == "S", -shift_distance, 0)
)]
# resulting lat
geo[, resulting_latitude := lat + signed_shift_km / 111]

### Difference by resulting latitude
ggplot(
  geo,
  aes(
    x = resulting_latitude,
    y = mean_change,
    color = tile
  )
) +
  geom_point(size = 2) +
  geom_line(aes(group = tile)) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(
    x = "Latitude after shift (°)",
    y = "Mean canopy height change",
    title = "Response along latitude gradient"
  )


# Distance to the equator
geo[, distance_to_equator := abs(resulting_latitude)]

# ggplot(
#   geo,
#   aes(
#     x = distance_to_equator,
#     y = mean_change,
#     color = tile
#   )
# ) +
#   geom_point(size = 2) +
#   geom_line(aes(group = tile)) +
#   theme_bw() +
#   labs(
#     x = "Distance from equator (degrees latitude)",
#     y = "Mean canopy height change",
#     title = "Response vs distance from equator"
#   )


# ggplot(
#   geo,
#   aes(
#     x = distance_to_equator,
#     y = mean_change,
#     color = tile
#   )
# ) +
#   geom_line(aes(group = tile), linewidth = 0.8) +
#   geom_point(size = 2) +

#   # ⭐ highlight originals safely
#   geom_point(
#     data = geo[original == TRUE],
#     aes(
#       x = abs(lat),   # <- SAFE fallback (original lat exists)
#       y = mean_change,
#       color = tile
#     ),
#     shape = 22,
#     size = 5,
#     stroke = 1.2
#   ) +

#   theme_bw() +
#   labs(
#     x = "Distance from equator (degrees latitude)",
#     y = "Mean canopy height change",
#     title = "Response vs distance from equator (original positions highlighted)"
#   )


### ADD ORIGINAL POSITION TO PLOT ###
orig_points <- tile_coordinates[, .(
  tile = Name,
  lat,
  distance_to_equator = abs(lat),
  mean_change = 0
)]
plot_geo <- rbind(
  geo,
  orig_points,
  fill = TRUE
)
plot_geo <- plot_geo[order(tile, distance_to_equator)]

ggplot(
  plot_geo,
  aes(
    x = distance_to_equator,
    y = mean_change,
    color = tile
  )
) +
  geom_line(aes(group = tile), linewidth = 0.9) +

  geom_point(size = 2, alpha = 0.7) +

  geom_point(
    data = plot_geo[mean_change == 0],
    aes(
      x = distance_to_equator,
      y = mean_change,
      fill = tile,
      color = tile
    ),
    shape = 23,
    size = 2.5,
    stroke = 1.6
  ) +

  theme_bw() +
  labs(
    x = "Distance from equator (degrees latitude)",
    y = "Mean canopy height change",
    title = "Response vs distance from equator (with explicit original baseline)"
  )


### Latitude Plot with originals
geo[, resulting_latitude := lat + signed_shift_km / 111]

orig_points <- tile_coordinates[, .(
  tile = Name,
  lat,
  mean_change = 0
)]

plot_geo <- rbind(
  geo[, .(tile, lat = resulting_latitude, mean_change)],
  orig_points,
  fill = TRUE
)

ggplot(
  plot_geo,
  aes(
    x = lat,
    y = mean_change,
    color = tile
  )
) +
  geom_line(aes(group = tile), linewidth = 0.9, alpha = 0.7) +

  geom_point(size = 2, alpha = 0.7) +

  # ⭐ original positions (baseline at native latitude)
  geom_point(
    data = orig_points,
    aes(x = lat, y = mean_change, color = tile, fill = tile),
    shape = 23,
    size = 2.5,
    stroke = 1.6
  ) +

  geom_vline(xintercept = 0, linetype = 2, alpha = 0.4) +

  theme_bw() +
  labs(
    x = "Latitude (°) — South (-) to North (+)",
    y = "Mean canopy height change",
    title = "Response along latitude gradient (with original baseline)"
  )








### Cosine Experiments ###
geo[, lat_rad := resulting_latitude * pi / 180]
geo[, cos_lat := cos(lat_rad)]

cos_summary <- geo[
  !is.na(cos_lat),
  .(
    mean_change = mean(mean_change, na.rm = TRUE),
    sd_change = sd(mean_change, na.rm = TRUE),
    n = .N
  ),
  by = .(tile, cos_lat)
]

cos_summary[, se := sd_change / sqrt(n)]

ggplot(
  cos_summary,
  aes(
    x = cos_lat,
    y = mean_change,
    color = tile,
    group = tile
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = mean_change - se,
      ymax = mean_change + se
    ),
    width = 0
  ) +
  theme_bw() +
  labs(
    x = "cos(latitude) (equator-ward scaling)",
    y = "Mean canopy height change",
    title = "Response vs cosine(latitude)"
  )




#### MEAN CHANGE BY LATITUDE ####
orig_points <- tile_coordinates[, .(
  tile = Name,
  distance_to_equator = abs(lat),
  mean_change = 0
)]

plot_geo <- rbind(
  geo[, .(tile, distance_to_equator, mean_change)],
  orig_points,
  fill = TRUE
)

plot_geo[, dist_bin := round(distance_to_equator)]

dist_summary <- plot_geo[
  ,
  .(
    mean_change = mean(mean_change, na.rm = TRUE),
    sd_change = sd(mean_change, na.rm = TRUE),
    n = .N
  ),
  by = dist_bin
]

dist_summary[, se := sd_change / sqrt(n)]

ggplot(
  plot_geo,
  aes(
    x = distance_to_equator,
    y = mean_change
  )
) +
  geom_point(alpha = 0.25) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    linewidth = 1.2,
    color = "darkgreen"
  ) +
  theme_bw() +
  labs(
    x = "Distance from equator (° latitude)",
    y = "Mean CH change [m]",
  )



ggplot(
  dist_summary,
  aes(
    x = dist_bin,
    y = mean_change
  )
) +
  geom_ribbon(
    aes(
      ymin = mean_change - se,
      ymax = mean_change + se
    ),
    alpha = 0.2
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_bw() +
  labs(
    x = "Distance from equator (° latitude)",
    y = "Mean CH change [m]",
  )
