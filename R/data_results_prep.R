library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)
# Data Preperation --------------------------------------------------------

# Read individual data files and merge
viz_bands <- read.csv("/home/emilio/canopy_height/results/2025-10-06_full_results.csv")
edge_bands <- read.csv("/home/emilio/canopy_height/results/2025-10-15_merged_581112.csv")

merged <- rbind(viz_bands, edge_bands)



# create absolute increment column
merged$abs_increment <- abs(merged$increment)

# Add missing zero-step rows
add_zero_step_rows <- function(result_table) {
  
  # Step 1: Identify which combinations are missing a zero increment row
  missing_zero_rows <- merged %>%
    distinct(tile, band) %>%  # Identify all tile-band combinations
    anti_join(
      merged %>% filter(increment == 0), # check which combinations are missing
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
  missing_cols <- setdiff(names(merged), names(zero_increment_rows))
  zero_increment_rows[missing_cols] <- 0.0  # Add all missing columns as 0.0
  
  # Step 4: Combine with original table
  merged <- bind_rows(merged, zero_increment_rows)
  
  return(merged)
}
merged <- add_zero_step_rows(merged)

# Convert increment to percent and turn decrease to negative values
merged <- merged %>%
  mutate(increment = increment * 100) %>% 
  mutate(increment = ifelse(decrease == "True", -abs(increment), abs(increment)))


# Add tile_band combination name as column
merged <- merged %>%
  mutate(tile_band = paste(tile, band, sep = "_"))


# BAND NAMES
# get band names for export name
band_names <- paste(gsub("\\D", "", unique(merged$band)), collapse = "+")
# Convert Band names to factor
band_labels <- c("B02" = "Blue", "B03" = "Green",  "B04" = "Red",  "B08" = "NIR",
                 "B05" = "RedEdge", "B8A" = "NIR2",  "B11" = "SWIR1",  "B12" = "SWIR2")
merged$band <- factor(merged$band, levels = names(band_labels), labels = band_labels)

# LOCATION
# Add location information as factor
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", 
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia", 
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")
merged$Location <- factor(merged$tile, levels = names(tile_label), labels = tile_label)




write.csv(merged, "results/2025-10-20_merged_results_8_Bands.csv", row.names = F)



# ### Increment as factor ####
# # Convert increment to factor and get numeric positions for vlines
# result_table_factor <- merged %>%
#   mutate(increment_f = factor(increment))
# 
# # Zero position for plots
# zero_pos <- which(levels(result_table_factor$increment_f) == "0")
# 
# # Numeric positions of each increment on x-axis
# increment_positions <- seq_along(levels(result_table_factor$increment_f))
