# Plot results by tile

# Avg diff PERCENT by location
ggplot(result_table, aes(x = increment, y = avg_difference_percent, color = Location)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ band, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Manipulation [%]", 
       y = "Average Difference [%]",
       title = "Average relative difference to original prediction by manipulation degreee by location and band.") +
  theme_minimal(base_size = 14)

