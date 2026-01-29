### Plot Selection for Thesis ###
library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)

# Setup -------------------------------------------------------------------

# Full results of all Tiles X all Bands
result_table <- read.csv("results/2025-10-20_main.csv")
# Complete Interaction results of all Tiles - 4 Interaction groups (High, Low, RGB, All (+ Blue)) # + mean height column
result_table <- read.csv("results/2026-01-29_main_interactions.csv")


band_map <- c( Blue = "02",  Green  = "03",  Red = "04",  RedEdge= "05",  
               NIR = "08", NIR2 = "8A",  SWIR1  = "11",  SWIR2  = "12")
band_names <- paste( band_map[unique(result_table$band)], collapse = "+")


cbf_colors <- c(  Blue     = "#0072B2", Green    = "#009E73", Red      = "#D55E00", RedEdge  = "#CC79A7",
                  NIR      = "#9E0142", NIR2     = "#5E4FA2", SWIR1    = "#E6AB02", SWIR2    = "#999999")

tol_muted_11 <- c("#332288",  "#6699CC",  "#88CCEE",  "#44AA99",  "#117733",  "#999933",  
                  "#DDCC77",  "#661100",  "#CC6677",  "#882255",  "#AA4499")

int_colors <- c(ALL = "#009E73", Blue = "#88CCEE", High = "#DDCC77", Low = "#CC79A7", RGB = "#882255" )
# For interactions:
# cbf_colors <- int_colors

# Spectral Line -----------------------------------------------------------

# result_table$band <- factor(result_table$band, levels = c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge"))

ggplot(result_table, aes(x = abs_increment, y = avg_difference_percent, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  # scale_color_manual(values = cbf_colors, breaks=c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge")) +
  # scale_fill_manual(values = cbf_colors, breaks=c("Blue", "NIR2", "NIR", "Green", "SWIR2", "Red", "SWIR1", "RedEdge")) +
  labs(x = "Manipulation Degree [%]", y = "Average Relative Difference [%]",
       color = "Band", fill = "Band") +
  theme_minimal(base_size = 14)


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

# Relative diff Facett by band + points
ggboxplot(result_table, x = "increment",  y = "avg_difference_percent",
          fill = "band",  color = "black",
          palette = cbf_colors,  facet.by = "band",  scales = "fixed", alpha = 0.8, ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  rotate_x_text(angle = 45) +
  scale_x_discrete(
    breaks = c("-20", "-10", "0", "10", "20")  # specify which factor levels to show
  ) +  labs(x = "Manipulation [%]", y = "Average Relative Difference [%]") +
  theme_pubr(base_size = 14) +
  theme(legend.position = "none")

# Location ----------------------------------------------------------------

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


# Cluster -----------------------------------------------------------------

# Right Sided
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Summarize data: average over tile, band, and abs_increment per Location
location_summary <- result_table %>%
  group_by(Location, abs_increment) %>%
  summarise(avg_diff_percent = mean(avg_difference_percent, na.rm = TRUE),
            .groups = "drop")

# Pivot to wide format: one row per Location, columns = abs_increment
features <- location_summary %>%
  pivot_wider(names_from = abs_increment, values_from = avg_diff_percent, values_fill = 0)

# Keep row names as Location
feature_matrix <- as.data.frame(features[, -1])
rownames(feature_matrix) <- features$Location

# Standardize features
feature_matrix_scaled <- scale(feature_matrix)

# Hierarchical clustering
dist_matrix <- dist(feature_matrix_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical clustering of Locations based on abs_increment")

# Cut tree into k clusters
k <- 3
location_groups <- data.frame(
  Location = rownames(feature_matrix_scaled),
  group = factor(cutree(hc, k = k))
)

# Merge cluster info back to summary for plotting
plot_data <- location_summary %>%
  left_join(location_groups, by = "Location")

# Location lables
label_positions <- plot_data %>%
  group_by(Location) %>%
  filter(!is.na(abs_increment)) %>%           # remove NA increments
  slice_max(order_by = abs_increment, n = 1) %>%  # pick the row with the max increment safely
  ungroup()

### Colors
library(viridisLite)
cluster_colors <- viridis(k, option = "plasma", begin = 0.1, end = 0.8)  # D = purple→yellow, better contrast
plot_data$group <- factor(plot_data$group)
names(cluster_colors) <- levels(plot_data$group)

# cluster_colors["Cluster4"] <- "#" 

cluster_order <- plot_data %>%
  group_by(group) %>%
  filter(abs_increment == max(abs_increment)) %>%
  summarise(mean_val = mean(avg_diff_percent, na.rm = TRUE)) %>%
  arrange(desc(mean_val)) %>%
  pull(group)

# order colours by max value at 25% manipulation degree
plot_data$group <- factor(plot_data$group, levels = cluster_order)
names(cluster_colors) <- cluster_order


# Plot average difference over abs_increment colored by cluster
ggplot(plot_data, aes(x = abs_increment, y = avg_diff_percent, color = group, group = Location)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  # scale_color_viridis_d(option = "C") +
  scale_color_manual(values = cluster_colors) + 
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Difference [%]",
    color = "Cluster"
  ) +
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
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(face = "bold"))


# ggsave(paste0("plots/cluster/",Sys.Date(),"_",length(unique(result_table$Location)),"T_B",band_names,
#               "_","right_line_location_Percent_cluster5",".png"),
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")



# Interactions ------------------------------------------------------------





##################################################################################################
################                    ALTERNATIVE PLOTS TO CONSIDER                 ################
##################################################################################################

# Single band by location -------------------------------------------------


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
  scale_color_manual(values = tol_muted_11) +
  scale_fill_manual(values = tol_muted_11) +
  
  labs(
    title = "Near-Infrared",
    x = "Manipulation [%]",
    y = "Average Difference [m]",
    # y = "Average Relative Difference [%]",
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



# Facett Location and Band ------------------------------------------

# Relative difference
ggplot(result_table, aes(x = abs(increment), y = avg_abs_diff_perc, color = band, fill = band)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  scale_fill_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  facet_wrap(~Location) +
  labs(x = "Manipulation Degree [%]", y = "Mean Relative Difference [%]") +
  theme_minimal()
