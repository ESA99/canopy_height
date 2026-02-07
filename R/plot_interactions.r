# plot selection for interaction data
library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)

# Complete Interaction results of all Tiles - 4 Interaction groups (High, Low, RGB, All (+ Blue)) # + mean height column
result_table <- read.csv("results/2026-01-29_main_interactions.csv")

cbf_colors <- c(ALL = "#009E73", Blue = "#88CCEE", High = "#DDCC77", Low = "#882285", RGB = "#CC79A7" )


# Spectral Line -----------------------------------------------------------

ggplot(result_table, aes(x = abs_increment, y = avg_difference_percent, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = cbf_colors, breaks=c("RGB", "ALL", "Blue", "High", "Low")) +
  scale_fill_manual(values = cbf_colors, breaks=c("RGB", "ALL", "Blue", "High", "Low")) +
  labs(x = "Manipulation Degree [%]", y = "Average Relative Difference [%]",
       color = "Band", fill = "Band") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

ggplot(result_table, aes(x = abs_increment, y = average_difference, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = cbf_colors, breaks=c("RGB", "ALL", "Blue", "High", "Low")) +
  scale_fill_manual(values = cbf_colors, breaks=c("RGB", "ALL", "Blue", "High", "Low")) +
  labs(x = "Manipulation Degree [%]", y = "Average Difference [m]",
       color = "Band", fill = "Band") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")


# Spectral Facett ---------------------------------------------------------

# Facetted by band line plot
ggline(
  result_table,
  x = "increment",
  y = "avg_difference_percent",
  color = "band",
  fill = "band",
  add = "mean_se",
  linewidth = 1.2,
  alpha = 0.2,
  palette = cbf_colors,
  facet.by = "band",
  scales = "fixed",
  ncol = 2
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Relative Difference [%]",
    color = "Band"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "none"  )

# Boxplot -----------------------------------------------------------------

# Absolute diff Facett by band + points
ggboxplot(result_table, x = "increment",  y = "average_difference",
          fill = "band",  color = "black",
          palette = cbf_colors,  facet.by = "band",  scales = "fixed", alpha = 0.8, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  rotate_x_text(angle = 45) +
  labs(x = "Manipulation [%]", y = "Average Difference [m]") +
  theme_pubr(base_size = 14) +
  theme(legend.position = "none")

# Location Facett ----------------------------------------------------------------

plot_data <- result_table %>%
  group_by(band, Location, abs_increment) %>%
  summarise(avg_difference_percent = mean(avg_difference_percent, na.rm = TRUE), .groups = "drop")
plot_data$manipulation <- plot_data$abs_increment#*100

# Facetted by band location plot, avg relative difference
ggline(
  plot_data,
  x = "manipulation",
  y = "avg_difference_percent",
  color = "Location",
  fill  = "Location",
  add = "mean_se",
  linewidth = 1.2,
  alpha = 0.2,
  palette = tol_muted_11,
  facet.by = "band",
  scales = "fixed",
  ncol = 2
) +
  geom_hline(
    yintercept = c(50, 100),
    linetype = "dashed",
    color = "grey85",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Relative Difference [%]",
    color = "Location",
    fill  = "Location"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "bottom"
  )
