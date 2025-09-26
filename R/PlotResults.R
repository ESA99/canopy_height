# Git Setup
# Force push (required to overwrite history)
# system("git push origin main --force")

# Prepare data --------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)

# original_data <- "/home/emilio/canopy_height/results/2025-06-25_result_table.csv"
original_data <- "/home/emilio/canopy_height/results/2025-09-25_combined_results_49UCP_55HEV.csv"
data <- read.csv(original_data)
result_table <- data
head(result_table)
# result_table <- result_table %>% # Remove B01 (Inserted for original prediction)
#   filter(band != "B01")

band_names <- paste(gsub("\\D", "", unique(result_table$band)), collapse = "+")

# Add zero rows
add_zero_increment_rows <- function(result_table) {
  
  # Step 1: Identify which combinations are missing a zero increment row
  missing_zero_rows <- result_table %>%
    distinct(tile, band) %>%  # Identify all tile-band combinations
    anti_join(
      result_table %>% filter(increment == 0), # check which combinations are missing
      by = c("tile", "band")
    )
  
  # Step 2: Create the zero increment rows
  zero_increment_rows <- missing_zero_rows %>%
    mutate(
      increment = 0,
      decrease = "True",
      out_name = paste(tile, band, "original", sep = "_"),
      year = 2020
    )
  
  # Step 3: Add missing columns
  missing_cols <- setdiff(names(result_table), names(zero_increment_rows))
  zero_increment_rows[missing_cols] <- 0.0  # Add all missing columns as 0.0
  
  # Step 4: Combine with original table
  result_table <- bind_rows(result_table, zero_increment_rows)
  
  return(result_table)
}
result_table <- add_zero_increment_rows(result_table)

# Convert increment to percent and turn decrease to negative values
result_table <- result_table %>%
  mutate(increment = increment * 100) %>% 
    mutate(increment = ifelse(decrease == "True", -abs(increment), abs(increment)))

# Add tile_band combination name as column
result_table <- result_table %>%
  mutate(tile_band = paste(tile, band, sep = "_"))


# General Settings ---------------------------------------------------

band_labels <- c("B02" = "Blue", "B03" = "Green",  "B04" = "Red",  "B08" = "NIR")
result_table$band <- factor(result_table$band, levels = names(band_labels), labels = band_labels)

custom_colors <- c( "Blue" = "dodgerblue2", "Green" = "chartreuse3", "Red" = "firebrick3",  "NIR" = "mediumpurple2")
# custom_colors <- c( "B02" = "navy", "B03" = "green4", "B04" = "red",  "B08" = "purple")

# colorblind-friendly colour scale
cbf_colors <- c("Blue"  = "#0072B2","Green" = "#009E73", "Red"   = "#D55E00", "NIR"   = "#CC79A7" )

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





# Boxplot -----------------------------------------------------------------

ggplot(result_table, aes(x = factor(increment), y = average_difference, fill = band)) +
  geom_boxplot(outlier.size = 1.5, alpha = 0.6, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    fill = "Band",
    title = "Distribution of Average Differences per Increment and Band"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

### Increment as factor

# Convert increment to factor and get numeric positions for vlines
result_table_factor <- result_table %>%
  mutate(increment_f = factor(increment))

# Numeric positions of each increment on x-axis
increment_positions <- seq_along(levels(result_table_factor$increment_f))

ggplot(result_table_factor, aes(x = increment_f, y = average_difference, fill = band)) +
  geom_boxplot(outlier.size = 1.5, alpha = 0.6, color = "black", position = position_dodge(width = 0.8)) +
  # Draw vertical lines between increments
  geom_vline(xintercept = seq(1.5, length(increment_positions)-0.5, by = 1),
             color = "grey70", linetype = "dashed") +
  scale_fill_manual(values = cbf_colors) +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    fill = "Band",
    title = "Distribution of Average Differences per Increment and Band"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.major.x = element_blank(),  # remove default x-grid
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


### Facetted Boxplot
ggplot(result_table, aes(x = factor(increment), y = average_difference, fill = band)) +
  geom_boxplot(outlier.size = 1.5, alpha = 0.6) +
  facet_wrap(~ band, scales = "fixed") +    # Alternative: scales = "free_y"
  scale_fill_manual(values = cbf_colors) +
  labs(x = "Manipulation [%]", y = "Average Difference",
       title = "Average Differences per Increment and Band") +
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(face = "bold"), 
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

### ggpubr boxplot facetted

zero_pos <- which(levels(result_table_factor$increment_f) == "0")

ggboxplot(
  result_table,
  x = "increment",
  y = "average_difference",
  fill = "band",
  color = "black",
  palette = cbf_colors,
  facet.by = "band",
  scales = "fixed"
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = zero_pos, linetype = "dashed", color = "grey30") +
  rotate_x_text(angle = 45) +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    title = "Distribution of Average Differences per Increment and Band"
  ) +
  theme_pubr(base_size = 14) +
  theme(legend.position = "none")

### ggpubr full boxplot

ggboxplot(
  result_table_factor,
  x = "increment_f",
  y = "average_difference",
  fill = "band",
  color = "black",
  palette = cbf_colors,
  outlier.shape = 21
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +  # zero line
  rotate_x_text(angle = 45) +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    title = "Distribution of Average Differences per Increment and Band",
    fill = "Band"
  ) +
  theme_pubr(base_size = 14)


# Ribbon plot (with uncertainty) ------------------------------------------

ggplot(result_table, aes(x = increment, y = average_difference, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(x = "Manipulation [%]", y = "Average Difference") +
  theme_minimal(base_size = 14)

ggline(
  result_table,
  x = "increment",
  y = "average_difference",
  color = "band",
  fill = "band",
  add = "mean_se",          # adds mean line + SE ribbon
  linewidth = 1.2,
  alpha = 0.2,
  palette = cbf_colors,
  position = position_dodge(width = 0.3)  # <-- offset lines and ribbons
) +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    color = "Band",
    fill = "Band",
    title = "Average Difference with SE Ribbon per Band"
  ) +
  theme_pubr(base_size = 14)


## Facetted ggpubr version
ggline(
  result_table,
  x = "increment",
  y = "average_difference",
  color = "band",
  fill = "band",
  add = "mean_se",
  linewidth = 1.2,
  alpha = 0.2,
  palette = cbf_colors,
  facet.by = "band",
  scales = "fixed"
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = zero_pos, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    title = "Average Difference with SE Ribbon per Band"
  ) +
  theme_pubr(base_size = 14)


## Facetted ggpubr version plus points
ggline(
  result_table,
  x = "increment",
  y = "average_difference",
  color = "band",
  fill = "band",
  add = "mean_se",
  linewidth = 1.2,
  alpha = 0.2,
  palette = cbf_colors,
  facet.by = "band",
  scales = "fixed"
) +
  geom_point(aes(y = average_difference, color = "black"),
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = zero_pos, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation [%]",
    y = "Average Difference",
    title = "Average Difference with SE Ribbon per Band"
  ) +
  theme_pubr(base_size = 14)



ggsave(paste0("plots/",Sys.Date(),"_",length(unique(result_table$tile)),"T_B",band_names,
              "_","ribbon_facett_pub",".png"), 
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

ggsave(paste0("plots/",Sys.Date(),"_",length(unique(result_table$tile)),"T_B",band_names,
              "_","Facetted",".png"), 
       width = 300, height = 175, units = "mm", dpi = 300, bg = "white")


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

## Same colour band plot 

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

## Generic Plot
# p <- ggplot(result_table, aes(x = increment, y = average_difference, color = tile_band)) +
#   geom_line(linewidth = 1.1) +  # Use linewidth instead of size
#   geom_point(size = 2) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
#   labs(
#     title = "Average Difference by degree of manipulation",
#     x = "Manipulation [%]",
#     y = "Average Difference",
#     color = "Tile_Band"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "right",
#     axis.title = element_text(face = "bold"),
#     axis.text = element_text(color = "black"),
#     panel.grid.minor = element_blank()
#   )
# print(p)


