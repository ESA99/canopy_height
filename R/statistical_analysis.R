library(dplyr)

# Pearson correlation
cor_test <- cor.test(result_table$increment,
                     result_table$average_difference,
                     method = "pearson")

print(cor_test)

# Spearman correlation (in case relationship is non-linear but monotonic)
cor_test_spearman <- cor.test(result_table$increment,
                              result_table$average_difference,
                              method = "spearman")

print(cor_test_spearman)



# Average values per band -> correlation ----------------------------------


# Step 1: Average across tiles for each band × increment
avg_by_band_increment <- result_table %>%
  group_by(band, increment) %>%
  summarise(
    avg_diff = mean(average_difference, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: For each band, calculate correlations across increments
cor_by_band <- avg_by_band_increment %>%
  group_by(band) %>%
  summarise(
    pearson_r  = cor(increment, avg_diff, method = "pearson"),
    spearman_r = cor(increment, avg_diff, method = "spearman"),
    .groups = "drop"
  )

print(cor_by_band)



# Correlations with p-values ----------------------------------------------

# Function to compute correlation + p-value
cor_with_p <- function(df) {
  pearson_test <- cor.test(df$increment, df$avg_diff, method = "pearson")
  spearman_test <- cor.test(df$increment, df$avg_diff, method = "spearman")
  
  tibble(
    pearson_r = pearson_test$estimate,
    pearson_p = pearson_test$p.value,
    spearman_r = spearman_test$estimate,
    spearman_p = spearman_test$p.value
  )
}

# Apply per band
cor_by_band <- avg_by_band_increment %>%
  group_by(band) %>%
  group_modify(~ cor_with_p(.x)) %>%
  ungroup()

print(cor_by_band)




# Calculate cor per combination and average results afterwards ------------


library(dplyr)

# Compute correlation per band & tile
cor_by_band_tile <- result_table %>%
  group_by(band, tile) %>%
  summarise(
    pearson_r = cor(increment, average_difference, method = "pearson"),
    spearman_r = cor(increment, average_difference, method = "spearman"),
    .groups = "drop"
  )

# Now summarise per band (average correlation across tiles)
cor_by_band <- cor_by_band_tile %>%
  group_by(band) %>%
  summarise(
    mean_pearson = mean(pearson_r, na.rm = TRUE),
    sd_pearson   = sd(pearson_r, na.rm = TRUE),
    mean_spearman = mean(spearman_r, na.rm = TRUE),
    sd_spearman   = sd(spearman_r, na.rm = TRUE),
    n_tiles = n(),
    .groups = "drop"
  )

print(cor_by_band)
