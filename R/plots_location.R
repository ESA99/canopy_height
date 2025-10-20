library(ggplot2)
library(dplyr)
library(viridis)
library(ggrepel)


# ggsave(paste0("plots/",Sys.Date(),"_",length(unique(result_table$Location)),"T_B",band_names,
#               "_","Loc_absRelDiff_absInc_line",".png"),
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")


# PERCENT line plots ------------------------------------------------------

# Avg diff PERCENT by location
ggplot(result_table, aes(x = increment, y = avg_difference_percent, color = Location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ band, scales = "fixed") + # free_y
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Manipulation [%]", 
       y = "Average Difference [%]",
       title = "Average relative difference to original prediction by manipulation degreee by location and band.") +
  theme_minimal(base_size = 14)



## Right sided % avg line
# average over all values per abs_increment, Location, and band
plot_data <- result_table %>%
  group_by(band, Location, abs_increment) %>%
  summarise(avg_abs_diff_perc = mean(avg_abs_diff_perc, na.rm = TRUE), .groups = "drop")

ggplot(plot_data, aes(x = abs_increment, y = avg_abs_diff_perc, color = Location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ band, scales = "fixed") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(option = "plasma") +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference [%]",
    title = "Average relative absolute difference to original prediction by\nmanipulation degree, location, and band."
  ) +
  theme_minimal(base_size = 14)


# Facett by band, Location as line
ggplot(result_table, aes(x = increment, y = avg_abs_diff_perc, color = Location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ band, scales = "fixed") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(option = "plasma") +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference [%]",
    title = "Average relative absolute difference to original prediction by\nmanipulation degree, location, and band."
  ) +
  theme_minimal(base_size = 14)


# Facett Location and Band ------------------------------------------

# Absolute difference
ggplot(result_table, aes(x = abs(increment), y = average_difference, color = band, fill = band)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  facet_wrap(~Location) +
  labs(title = "Effect of Manipulation Degree on Prediction Change per Band",
       x = "Manipultaion Degree [%]", y = "Absolute Difference [m]") +
  theme_minimal()

# model <- lm(avg_abs_diff_perc ~ band * Location * abs_increment, data = result_table)
# anova(model)
# summary(model)


# Relative difference
ggplot(result_table, aes(x = abs(increment), y = avg_abs_diff_perc, color = band, fill = band)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  # scale_color_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  # scale_fill_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  facet_wrap(~Location) +
  labs(title = "Effect of Manipulation Degree on Relative Prediction Change per Band",
       x = "Manipulation Degree [%]", y = "Mean Relative Difference [%]") +
  theme_minimal()



# Heatmap -----------------------------------------------------------------

ggplot(result_table %>%
         group_by(band, Location) %>%
         summarise(M_Rel_Diff = mean(avg_abs_diff_perc, na.rm = TRUE), .groups = "drop"),
       aes(x = Location, y = band, fill = M_Rel_Diff)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "cividis") +
  labs(title = "Band Sensitivity by Location",
       x = "Location", y = "Band", fill = "Mean rel diff (%)") +
  theme_minimal()


# Single band line plots ----------------------------------------------

band_color <- "NIR"

# Example color-blind–friendly palette (viridis)
cb_palette <- viridis::viridis(length(unique(result_table$Location)), option = "C")

ggplot(
  filter(result_table, band == band_color),  # select band
  aes(
    x = increment,
    y = average_difference,
    color = Location,   # color by Location
    group = Location    # group by Location
  )
) +
    # Add shaded SD ribbon
    geom_ribbon(
      aes(
        ymin = average_difference - std_dev,
        ymax = average_difference + std_dev,
        fill = Location    # fill by Location
      ),
      alpha = 0.2,
      color = NA
    ) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    
    # Color-blind–friendly scales
    scale_color_manual(values = cb_palette) +
    scale_fill_manual(values = cb_palette) +
    
    labs(
      title = "Average Difference by degree of manipulation",
      x = "Manipulation [%]",
      y = "Average Difference [m]",
      color = "Location",
      fill = "Location"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
)




# Location Names in Plot --------------------------------------------------

# Prepare color-blind-friendly palette
locations <- unique(result_table$Location)
cb_palette <- viridis::viridis(length(locations), option = "C")

# Aggregate for plotting (mean per increment and location)
plot_data <- result_table %>%
  filter(band == band_color) %>%
  group_by(Location, increment) %>%
  summarise(
    mean_diff = mean(avg_difference_percent, na.rm = TRUE),
    sd_diff   = sd(avg_difference_percent, na.rm = TRUE),
    .groups = "drop"
  )

# Find the last x-value per Location for label positioning
label_positions <- plot_data %>%
  group_by(Location) %>%
  filter(!is.na(increment)) %>%           # remove NA increments
  slice_max(order_by = increment, n = 1) %>%  # pick the row with the max increment safely
  ungroup()

# Title
title_name <- paste0("Average Difference by degree of manipulation for band ", band_color)

# Plot
ggplot(plot_data, aes(x = increment, y = mean_diff, color = Location, group = Location)) +
  geom_ribbon(aes(ymin = mean_diff - sd_diff, ymax = mean_diff + sd_diff, fill = Location),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Color scales
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  
  # Direct labels at the end of each line
  geom_text_repel(
    data = label_positions,
    aes(label = Location),
    nudge_x = 0.5,         # adjust horizontal label position
    direction = "y",
    hjust = 0,
    segment.color = NA,    # remove connecting line
    size = 3.5,
    box.padding = 0.2,
    point.padding = 0.5
  ) +
  
  labs(
    title = title_name,
    x = "Manipulation Degree [%]",
    y = "Average Relative Difference [%]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",   # remove legend, labels are in the plot
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

