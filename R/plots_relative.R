# Relative changes [%]

ggline(
  result_table,
  x = "increment",
  y = "avg_difference_percent",
  color = "band",
  fill = "band",
  add = "mean_se",          # adds mean line + SE ribbon
  linewidth = 1.2,
  alpha = 0.2,
  palette = cbf_colors,
  position = position_dodge(width = 0.3)  # <-- offset lines and ribbons
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Relative Difference [%]",
    color = "Band",
    fill = "Band",
    title = "Average Relative Difference per Band"
  ) +
  theme_pubr(base_size = 14)
