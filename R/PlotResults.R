# Git Setup
# Force push (required to overwrite history)
# system("git push origin main --force")

# Plotting Results --------------------------------------------------------


library(ggplot2)
library(dplyr)

result_table <- read.csv("/home/emilio/canopy_height/final_results/2025-06-03_result_table.csv")
head(result_table)

# Recreate or modify your data with tile_band
result_table <- result_table %>%
  mutate(tile_band = paste(tile, band, sep = "_"))

# Plot
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


## Colour blind friendly display
library(viridis)

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


# Group uniquely by both tile and band, colour blind and greyscale friendly
ggplot(result_table, aes(
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
