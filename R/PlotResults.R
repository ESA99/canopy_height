# Git Setup
# Force push (required to overwrite history)
# system("git push origin main --force")

# Prepare data --------------------------------------------------------

library(ggplot2)
library(dplyr)
library(viridis)

result_table <- read.csv("/home/emilio/canopy_height/final_results/2025-06-03_result_table.csv")
head(result_table)

# Remove B01
result_table <- result_table %>%
  filter(band != "B01")

# Add 0.0 Increment Steps per tile and band
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
      out_name = paste(tile, band, "0.00", ifelse(decrease, "D", "I"), sep = "_"),
      year = 2020
    )
  )
}


# Add tile_band combination name as column
result_table <- result_table %>%
  mutate(tile_band = paste(tile, band, sep = "_"))




# Plot results ------------------------------------------------------------

#### Band/Tile CB+grey plot #####
# Group uniquely by both tile and band, colour blind and greyscale friendly
(diff_plot <- ggplot(result_table, aes(
  x = increment,
  y = average_difference,
  color = band,
  linetype = tile,
  group = interaction(tile, band)
)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_color_viridis_d(option = "cividis") +
  labs(
    title = "Average Difference by Increment",
    x = "Increment",
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
  )
)

ggsave(paste0("plots/",
              Sys.Date(),
              "_",
              length(unique(result_table$tile)),
              "T_B",
              paste(gsub("\\D", "", unique(result_table$band)), collapse = "+"),
              "_lineplot.png"), 
       diff_plot, 
       width = 300, height = 175, units = "mm", dpi = 300, bg = "white")

paste0(Sys.Date(),
       "_",
       length(unique(result_table$tile)),
       "T_B",
       paste(gsub("\\D", "", unique(result_table$band)), collapse = "+"),
       "_lineplot.png")


#### Same coulour band plot ####
## Plot bands in same colour, different line styles per tile
ggplot(result_table, aes(x = increment, y = average_difference, color = band, linetype = tile, group = interaction(tile, band))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    title = "Average Difference by Increment",
    x = "Increment",
    y = "Average Difference",
    color = "Band",
    linetype = "Tile"
  ) +
  theme_minimal(base_size = 14)


# Colour blind friendly display, bands same colour

ggplot(result_table, aes(x = increment, y = average_difference, color = band, group = interaction(tile, band))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_color_viridis_d(option = "viridis") +  # Use 'cividis' or others if preferred
  labs(
    title = "Average Difference by Increment",
    x = "Increment",
    y = "Average Difference",
    color = "Band"
  ) +
  theme_minimal(base_size = 14)





#### Generic Plot ####
p <- ggplot(result_table, aes(x = increment, y = average_difference, color = tile_band)) +
  geom_line(linewidth = 1.1) +  # Use linewidth instead of size
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title = "Average Difference by Increment",
    x = "Increment",
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