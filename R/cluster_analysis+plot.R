# Detect similar behaiviour -> clstering
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Hierarchial clustering --------------------------------------------------

# Summarize data: average over tile and band per Location × increment
location_summary <- result_table %>%
  group_by(Location, increment) %>%
  summarise(avg_diff_percent = mean(avg_difference_percent, na.rm = TRUE),
            .groups = "drop")

# Pivot to wide format: one row per Location, columns = increments
features <- location_summary %>%
  pivot_wider(names_from = increment, values_from = avg_diff_percent, values_fill = 0)

# Keep row names as Location
feature_matrix <- as.data.frame(features[, -1])
rownames(feature_matrix) <- features$Location

# Standardize features
feature_matrix_scaled <- scale(feature_matrix)

# Hierarchical clustering
dist_matrix <- dist(feature_matrix_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram to check clusters
plot(hc, main = "Hierarchical clustering of Locations based on avg_difference_percent")

# Cut tree into k clusters
k <- 3  # choose number of groups
location_groups <- data.frame(
  Location = rownames(feature_matrix_scaled),
  group = factor(cutree(hc, k = k))
)

# Merge cluster info back to the summary for plotting
plot_data <- location_summary %>%
  left_join(location_groups, by = "Location")

# Plot average difference over increment colored by cluster
ggplot(plot_data, aes(x = increment, y = avg_diff_percent, color = group, group = Location)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_viridis_d(option = "C") +
  labs(
    title = "Average Difference Percent by Increment and Location Group",
    x = "Manipulation [%]",
    y = "Average Difference [%]",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(face = "bold"))



# As Function -------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

cluster_locations_plot <- function(data,
                                   response_var = "avg_difference_percent",
                                   group_var = "Location",
                                   increment_var = "increment",
                                   k = 3,
                                   scale_palette = "C") {
  
  # Summarize: mean per group_var × increment
  location_summary <- data %>%
    group_by(.data[[group_var]], .data[[increment_var]]) %>%
    summarise(
      mean_response = mean(.data[[response_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot to wide format
  features <- location_summary %>%
    pivot_wider(names_from = .data[[increment_var]], values_from = mean_response, values_fill = 0)
  
  # Keep row names as group_var
  feature_matrix <- as.data.frame(features[, -1])
  rownames(feature_matrix) <- features[[group_var]]
  
  # Standardize
  feature_matrix_scaled <- scale(feature_matrix)
  
  # Hierarchical clustering
  dist_matrix <- dist(feature_matrix_scaled, method = "euclidean")
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Assign clusters
  location_groups <- data.frame(
    group_name = rownames(feature_matrix_scaled),
    cluster = factor(cutree(hc, k = k))
  )
  colnames(location_groups)[1] <- group_var
  
  # Merge cluster info back
  plot_data <- location_summary %>%
    left_join(location_groups, by = group_var)
  
  label_positions <- plot_data %>%
    group_by(Location) %>%
    filter(!is.na(increment)) %>%           # remove NA increments
    slice_max(order_by = increment, n = 1) %>%  # pick the row with the max increment safely
    ungroup()
  
  # Plot
  p <- ggplot(plot_data, aes_string(x = increment_var, y = "mean_response",
                                    color = "cluster", group = group_var)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_viridis_d(option = scale_palette) +
    labs(
      title = paste("Clustering of mean", response_var, "by", group_var),
      x = increment_var,
      y = response_var,
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
  
  return(list(plot = p, dendrogram = hc, cluster_assignments = location_groups))
}

# "average_difference"        "avg_abs_diff"           
# "avg_difference_percent"    "avg_abs_diff_perc"
# "correlation"               "std_dev" 
# "abs_increment"   "increment"    "Location"  

# Cluster and plot based on avg_difference_percent
result1 <- cluster_locations_plot(result_table,
                                  response_var = "avg_difference_percent",
                                  k = 3)
result1$plot  
plot(result1$dendrogram)

result1$cluster_assignments %>% arrange(cluster)

# Cluster based on avg_abs_diff_perc
result2 <- cluster_locations_plot(result_table,
                                  response_var = "avg_abs_diff_perc",
                                  k = 3)
result2$plot
plot(result1$dendrogram)

result2$cluster_assignments %>% arrange(cluster)



comparison <- result1$cluster_assignments %>%
  rename(cluster_diff_percent = cluster) %>%
  left_join(result2$cluster_assignments %>% 
              rename(cluster_abs_diff = cluster), by = "Location")
print(comparison)



# ggsave(paste0("plots/cluster/",Sys.Date(),"_",length(unique(result_table$Location)),"T_B",band_names,
#               "_","line_location_Percent_cluster3",".png"),
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")



# Right sided clustering --------------------------------------------------



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
k <- 4
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

# Plot average difference over abs_increment colored by cluster
ggplot(plot_data, aes(x = abs_increment, y = avg_diff_percent, color = group, group = Location)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_viridis_d(option = "C") +
  labs(
    title = "Average Difference Percent by Absolute Increment and Location Group",
    x = "Absolute Manipulation [%]",
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
