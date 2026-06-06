library(dplyr)
library(stringr)
source("R/tools/tools.R")

geo <- merge_backup_files("/home/emilio/canopy_height/results/runs/2026-06-04_geographical_1/loop_backups/", F)

geo[, direction :=
      fifelse(
        grepl("_[NS]$", out_name),
        sub(".*_([NS])$", "\\1", out_name),
        NA_character_
      )]

geo[, shift_distance :=
      fifelse(
        grepl("_original$", out_name),
        0,
        as.numeric(sub(".*_geographical_([0-9]+)_[NS]$", "\\1", out_name))
      )]




library(data.table)
library(ggplot2)

# originals
orig <- geo[
  shift_distance == 0,
  .(tile, mean_change)
]

# duplicate originals for N and S
orig_plot <- rbind(
  copy(orig)[, direction := "N"],
  copy(orig)[, direction := "S"]
)

orig_plot[, shift_distance := 0]

# non-original scenarios
shift_plot <- geo[
  shift_distance > 0,
  .(tile, direction, shift_distance, mean_change)
]

# combine
plot_df <- rbindlist(list(orig_plot, shift_plot), fill = TRUE)

# average across tiles
plot_summary <- plot_df[
  ,
  .(
    mean_change = mean(mean_change, na.rm = TRUE),
    sd_change = sd(mean_change, na.rm = TRUE),
    n = .N
  ),
  by = .(direction, shift_distance)
]

plot_summary[, se := sd_change / sqrt(n)]

ggplot(
  plot_summary,
  aes(
    x = shift_distance,
    y = mean_change,
    color = direction
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
  labs(
    x = "Shift distance (km)",
    y = "Mean change",
    color = "Direction"
  ) +
  theme_bw()


ggplot(
  plot_df,
  aes(
    x = shift_distance,
    y = mean_change,
    color = direction,
    group = interaction(tile, direction)
  )
) +
  geom_line(alpha = 0.5) +
  geom_point() +
  facet_wrap(~tile)






# NEW APPROACH -----------------------------------------------------------

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

ggplot(
  geo,
  aes(
    x = distance_to_equator,
    y = mean_change,
    color = tile
  )
) +
  geom_point(size = 2) +
  geom_line(aes(group = tile)) +
  theme_bw() +
  labs(
    x = "Distance from equator (degrees latitude)",
    y = "Mean canopy height change",
    title = "Response vs distance from equator"
  )


ggplot(
  geo,
  aes(
    x = distance_to_equator,
    y = mean_change,
    color = tile
  )
) +
  geom_line(aes(group = tile), linewidth = 0.8) +
  geom_point(size = 2) +

  # ⭐ highlight originals safely
  geom_point(
    data = geo[original == TRUE],
    aes(
      x = abs(lat),   # <- SAFE fallback (original lat exists)
      y = mean_change,
      color = tile
    ),
    shape = 22,
    size = 5,
    stroke = 1.2
  ) +

  theme_bw() +
  labs(
    x = "Distance from equator (degrees latitude)",
    y = "Mean canopy height change",
    title = "Response vs distance from equator (original positions highlighted)"
  )


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
    size = 4.5,
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



