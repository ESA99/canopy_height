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
