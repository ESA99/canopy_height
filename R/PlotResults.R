# Git Setup
# Force push (required to overwrite history)
# system("git push origin main --force")

# Prepare data --------------------------------------------------------

library(ggplot2)
library(dplyr)
library(viridis)

original_data <- "/home/emilio/canopy_height/results/2025-06-25_result_table.csv"
data <- read.csv(original_data)
result_table <- data
head(result_table)

# Remove B01 (Inserted for original prediction)
# result_table <- result_table %>%
#   filter(band != "B01")

band_names <- paste(gsub("\\D", "", unique(result_table$band)), collapse = "+")


# Add 0.0 Increment Step per tile and band combination
result_table <- {
  missing <- dplyr::anti_join(
    dplyr::distinct(result_table, tile, band, decrease),
    dplyr::filter(result_table, average_difference == 0.0),
    by = c("tile", "band", "decrease")
  )
  dplyr::bind_rows(
    result_table,
    dplyr::mutate(
      missing,
      increment = 0.0,
      average_difference = 0.0,
      avg_diff_absoluteVals = 0.0,
      std_dev = 0.0,
      correlation = NA,
      out_name = paste(tile, band, "0.00", ifelse(decrease, "D", "I"), sep = "_"),
      year = 2020
    )
  )
}

# Convert increment to percent and turn decrease to negative values
result_table <- result_table %>%
  mutate(increment = increment * 100) %>% 
    mutate(increment = ifelse(decrease == "True", -abs(increment), abs(increment)))

# Turn decrease increment negative
# result_table <- result_table %>%
  # mutate(increment = ifelse(decrease == "True", -abs(increment), abs(increment)))

# Add tile_band combination name as column
result_table <- result_table %>%
  mutate(tile_band = paste(tile, band, sep = "_"))


# General Settings ---------------------------------------------------

band_labels <- c("B02" = "Blue", "B03" = "Green",  "B04" = "Red",  "B08" = "NIR")
result_table$band <- factor(result_table$band, levels = names(band_labels), labels = band_labels)

custom_colors <- c( "Blue" = "dodgerblue2", "Green" = "chartreuse3", "Red" = "firebrick3",  "NIR" = "mediumpurple2")
# custom_colors <- c( "B02" = "navy", "B03" = "green4", "B04" = "red",  "B08" = "purple")

# UNVERSAL PLOT SAVING
# ggsave(paste0("plots/",
#               sub("_.*", "", basename(original_data)), # date of the result data
#               "_TITLE.png"),
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")


# Full Overview Plot  ------------------------------------------------------------

## Band/Tile CB+grey plot
# Group uniquely by both tile and band, colour blind and greyscale friendly

diff_plot <- ggplot(result_table, aes(
  x = increment,
  y = average_difference,
  color = band,
  # linetype = tile,
  group = interaction(tile, band)
)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  # scale_color_viridis_d(option = "cividis") +
  scale_color_manual(values = custom_colors) + 
  labs(
    title = "Average Difference by degree of manipulation",
    x = "Manipulation [%]",
    y = "Average Difference",
    color = "Band",
    linetype = "Tile"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )+
  # scale_x_continuous(breaks = seq(-20, 20, by = 5))
  scale_x_continuous(breaks = sort(unique(result_table$increment)))


diff_plot

ggsave(paste0("plots/",
              Sys.Date(),
              "_",
              length(unique(result_table$tile)),
              "T_B",
              band_names,
              "_lineplot.png"), 
       diff_plot, 
       width = 300, height = 175, units = "mm", dpi = 300, bg = "white")

# paste0(Sys.Date(),
#        "_",
#        length(unique(result_table$tile)),
#        "T_B",
#        paste(gsub("\\D", "", unique(result_table$band)), collapse = "+"),
#        "_lineplot.png")





# Ribbon plot (with uncertainty) ------------------------------------------

ggplot(result_table, aes(x = increment, y = average_difference, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(x = "Manipulation [%]", y = "Average Difference") +
  theme_minimal(base_size = 14)

ggsave(paste0("plots/",Sys.Date(),"_",length(unique(result_table$tile)),"T_B",band_names,
              "_","ribbon_smooth",".png"), 
       width = 300, height = 175, units = "mm", dpi = 300, bg = "white")

# Heatmap | Increment x Band ----------------------------------------------

ggplot(result_table, aes(x = increment, y = band, fill = average_difference)) +
  geom_tile() +
  scale_fill_viridis(option = "cividis") +
  labs(x = "Manipulation [%]", y = "Band", fill = "Avg. Diff.") +
  theme_minimal(base_size = 14)

ggsave(paste0("plots/",Sys.Date(),"_",length(unique(result_table$tile)),"T_B",band_names,
              "_","HEATMAP",".png"), 
       width = 300, height = 175, units = "mm", dpi = 300, bg = "white")

# Facetted | Increment x Difference | per Band ----------------------------

ggplot(result_table, aes(x = increment, y = average_difference, color = tile)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ band, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Manipulation [%]", y = "Average Difference") +
  theme_minimal(base_size = 14)



# Std Dev Plot of one band ------------------------------------------------

band_color <- "NIR"

(diff_plot <- ggplot(
  filter(result_table, band == band_color),  # select band
  aes(
  x = increment,
  y = average_difference,
  color = band,
  linetype = tile,
  group = interaction(tile, band)
)) +
    # Add shaded SD ribbon
    geom_ribbon(
      aes(
        ymin = average_difference - std_dev,
        ymax = average_difference + std_dev,
        fill = band
      ),
      alpha = 0.2,
      color = NA
    ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  # scale_color_viridis_d(option = "cividis") +
  scale_color_manual(values = custom_colors) + 
    scale_fill_manual(values = custom_colors) +
  labs(
    title = "Average Difference by degree of manipulation",
    x = "Manipulation [%]",
    y = "Average Difference",
    color = "Band",
    fill = "Standard Deviation", 
    linetype = "Tile"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
)


# ggsave(paste0("plots/",
#               sub("_.*", "", basename(original_data)), # date of the result data
#               "_",
#               paste0(band_color,"_SD_bytile"),
#               "_lineplot.png"),
#        diff_plot,
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")



  



# Colour blind options ----------------------------------------------------


#### Same colour band plot ####

# Colour blind friendly display, bands same colour, not tile identification
ggplot(result_table, aes(x = increment, y = average_difference, color = band,linetype = tile, group = interaction(tile, band))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_color_viridis_d(option = "viridis") +  # Use 'cividis' or others if preferred
  labs(
    title = "Average Difference by degree of manipulation",
    x = "Manipulation [%]",
    y = "Average Difference",
    color = "Band"
  ) +
  theme_minimal(base_size = 14)


## Plot bands in same colour, different line styles per tile
ggplot(result_table, aes(x = increment, y = average_difference, color = band, linetype = tile, group = interaction(tile, band))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_color_manual(values = custom_colors) + 
  
  labs(
    title = "Average Difference by degree of manipulation",
    x = "Manipulation [%]",
    y = "Average Difference",
    color = "Band",
    linetype = "Tile"
  ) +
  theme_minimal(base_size = 14)




# Standard plots ----------------------------------------------------------


#### Generic Plot ####
p <- ggplot(result_table, aes(x = increment, y = average_difference, color = tile_band)) +
  geom_line(linewidth = 1.1) +  # Use linewidth instead of size
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title = "Average Difference by degree of manipulation",
    x = "Manipulation [%]",
    y = "Average Difference",
    color = "Tile_Band"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )
print(p)


