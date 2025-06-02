{# 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
}
library(ggplot2)
library(dplyr)
 
result_table <- read.csv("/home/emilio/canopy_height/final_rsult_table_test.csv")
head(result_table)

# Recreate or modify your data with tile_band
result_table <- result_table %>%
  mutate(tile_band = paste(tile, band, sep = "_"))

# Plot
p <- ggplot(result_table, aes(x = increment, y = avg_diff, color = tile_band)) +
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




system("git remote -v")

usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/ESA99/canopy_height",
  overwrite = TRUE
)

usethis::use_git_remote(
  name = "upstream",
  url = "https://github.com/langnico/global-canopy-height-model"
)


