# New data frame with correct sampling and original files and percent inclusion
library(terra)
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(ggrepel)
library(hexbin)
library(viridis)
library(RColorBrewer)

#=============================================================================================

diff_folder <- "/data/ESA99/export/all/difference_rasters/"
orig_folder <- "/data/ESA99/export/all/predictions/"

tile_names <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV")
bands <- c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12")
directions <- c("I", "D")

sample_size <- 100000

tile_label <- c( "55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", 
                 "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia", "34UFD" = "Poland", 
                 "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")

scenario_levels <- c("-25%", "-20%", "-15%", "-10%", "-5%","+5%", "+10%", "+15%", "+20%", "+25%")

tile_order <- c("Finland", "Mongolia", "Poland", "Australia", "Germany", "Switzerland", "Cameroon", "USA East", "USA West", "Brazil", "Malaysia")

heights <-data.frame(tile = c("Finland", "Mongolia", "Poland", "Australia", "Germany", "Switzerland", "Cameroon", "USA East", "USA West", "Brazil", "Malaysia"),
                     mean_height = c(4.6, 4.8, 6.2, 9.3, 10.1, 17.2, 21.1, 21.1, 22.8, 33.3, 43.5) ) 
heights <- heights %>%
  mutate(tile = factor(tile, levels = tile_order))


# Metadata ----------------------------------------------------------------

diff_files <- list.files(diff_folder, pattern = "\\.tif$", full.names = TRUE)
orig_files <- list.files(orig_folder, pattern = "original", full.names = T)

meta <- tibble(file = diff_files) %>%
  mutate(filename = basename(file)) %>%
  separate(
    filename,
    into = c("Prefix", "Tile", "Band", "Increment", "Direction"),
    sep = "_",
    remove = TRUE
  ) %>%
  mutate(Direction = str_remove(Direction, "\\.tif$")) %>%
  select(-Prefix) %>% 
  filter(Band != "original.tif")

meta_original <- tibble(file = orig_files) %>%
  mutate(filename = basename(file)) %>%
  separate(filename,
           into = c("Tile", "Band"),
           sep = "_",
           remove = TRUE) %>%
  mutate(Band = "original",
         Increment = "00",
         Direction = NA) %>%
  select(file, Tile, Band, Increment, Direction)



# Process: Extract pixel --------------------------------------------------

# 100k sample
# lapply
# incremental saving and recombine
# memory efficient
output_folder <- "R/data/tile_samples"
dir.create(output_folder, showWarnings = FALSE)


# Function to process one tile
process_tile_sample <- function(diff_file, tile_name, band, increment, direction, meta_original, n_sample = 100000) {
  
  # Load difference raster
  diff_r <- rast(diff_file)
  
  # Find the single matching original raster
  orig_files <- meta_original %>% filter(Tile == tile_name) %>% pull(file)
  
  if(length(orig_files) != 1) stop("Expected exactly one original raster for tile ", tile_name)
  orig_file <- orig_files[[1]]
  
  orig_r <- rast(orig_file)
  
  # Ensure rasters match
  if (!all(dim(diff_r) == dim(orig_r))) stop("Dimension mismatch for tile: ", tile_name)
  
  # Sample pixel indices
  n_pix <- ncell(diff_r)
  sample_idx <- sample(1:n_pix, min(n_sample, n_pix))
  
  # Extract pixel values
  vals_diff <- terra::extract(diff_r, sample_idx)[[1]]
  vals_orig <- terra::extract(orig_r, sample_idx)[[1]]
  
  # Build Scenario factor
  scenario_str <- paste0(ifelse(direction == "I", "+", "-"), as.numeric(increment), "%")
  scenario_factor <- factor(scenario_str, levels = scenario_levels)
  
  # Handle zero original values safely
  percent_diff <- ifelse(vals_orig != 0, (vals_diff / vals_orig) * 100, NA_real_)
  
  # Build tibble
  df <- tibble(
    Tile = tile_name,
    Band = band,
    Increment = increment,
    Direction = direction,
    Scenario = scenario_factor,
    Difference = vals_diff,
    Original = vals_orig,
    Percent_Difference = percent_diff
  )
  
  return(df)
}

# Sequential processing and incremental saving
for(i in seq_len(nrow(meta_diff))) {
  row <- meta_diff[i, ]
  
  df_tile <- process_tile_sample(
    diff_file = row$file,
    tile_name = row$Tile,
    band = row$Band,
    increment = row$Increment,
    direction = row$Direction,
    meta_original = meta_original,
    n_sample = sample_size
  )
  
  # Save CSV
  output_file <- file.path(output_folder, paste0("sample_", row$Tile, "_", row$Band, "_", row$Increment, "_", row$Direction, ".csv"))
  write_csv(df_tile, output_file)
  
  cat("Saved:", output_file, "\n")
}



# Data preperation --------------------------------------------------------

# Combine all CSVs into one data frame
csv_files <- list.files(output_folder, pattern = "\\.csv$", full.names = TRUE)

pixel_df <- lapply(csv_files, function(f) {
  read_csv(f,
           col_types = cols(
             Tile = col_character(),
             Band = col_character(),
             Increment = col_character(),   # force character
             Direction = col_character(),
             Scenario = col_character(),
             Difference = col_double(),
             Original = col_double(),
             Percent_Difference = col_double()
           ))
}) %>% bind_rows()
# Quick check
pixel_df %>% glimpse()


# Factor Tiles by CH 
pixel_df <- pixel_df %>%
  mutate(Tile = tile_label[Tile]) %>%
  mutate(Tile = factor(Tile, levels = tile_order) ) 


# Order heights df
heights <-data.frame(
  tile = c("Finland", "Mongolia", "Poland", "Australia", "Germany", 
           "Switzerland", "Cameroon", "USA East", "USA West", "Brazil", "Malaysia"),
  mean_height = c(4.6, 4.8, 6.2, 9.3, 10.1, 17.2, 21.1, 21.1, 22.8, 33.3, 43.5)
) 
heights <- heights %>%
  mutate(tile = factor(tile, levels = tile_order))


names(pixel_df) <- c("tile", "band", "increment", "direction", "scenario", "difference", "original", "percent_diff")

# Create abs_scenario variable
pixel_df <- pixel_df %>%
  mutate(
    abs_scenario = str_remove(scenario, "^[+-]")  # removes leading + or -
  )

pixel_df <- pixel_df %>%
  mutate(
    abs_scenario = factor(
      abs_scenario,
      levels = c("5%", "10%", "15%", "20%", "25%"),
      ordered = TRUE
    ),
    scenario = factor(
      scenario,
      levels = scenario_levels,
      ordered = TRUE
    )
  )


# SAVE table / Import data ------------------------------------------------

# saveRDS(pixel_df, file = "R/data/pixel_df.rds", compress = TRUE)
pixel_df <- readRDS("R/data/pixel_df.rds")
str(all_tiles_long)


# Statistics --------------------------------------------------------------

df_clean <- pixel_df %>% 
  filter(!is.na(original), original != 0)

# Correlation models
cor_diff <- cor(df_clean$original, df_clean$difference, use = "complete.obs")
cor_pct  <- cor(df_clean$original, df_clean$percent_diff, use = "complete.obs")
cor_diff
cor_pct

# Optional: Fit linear models for visualization / comparison
lm_diff <- lm(original ~ difference, data = df_clean)
lm_pct  <- lm(original ~ percent_diff, data = df_clean)

summary(lm_diff)
summary(lm_pct)


# Plotting ----------------------------------------------------------------

x_var <- "difference"
x_range <- 15

# x_var <- "percent_diff"
# x_range <- 50

# fill_var <- "abs_scenario"
fill_var <- "scenario"

scale_var <- "continuous"
# scale_var <- "discrete"



# Colour Scales -----------------------------------------------------------

if(scale_var == "discrete"){
  
  if(fill_var == "abs_scenario"){
    scenario_palette <- c("#AA3377","#EE6677","#CCBB44","#228833","#66CCEE")  # 5 colors
  } else {
    # scenario_palette <- c("#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60",   # negatives -25% to -5%
    # "#66c2a5","#3288bd","#5e4fa2","#9e0168","#fdae61")    # positives +5% to +25%
    
    # scenario_palette <- brewer.pal(10, "Paired")
    scenario_palette <- brewer.pal(10, "Spectral")
  }
  
} else {
  
  ifelse(fill_var == "abs_scenario",
         scenario_palette <- viridis(5, option = "D"),
         scenario_palette <- viridis(10, option = "D") )  
  
}


# Plots -------------------------------------------------------------------

# Mean height label position
label_x_pos <- x_range * 0.98
y_offset <- 0.20 


### Shaded density curves, ridge plot -> tiles sorted by CH, 
ggplot(pixel_df,
       aes(x = !!sym(x_var),
           y = tile,
           fill = !!sym(fill_var))) +   # fill colours
  
  geom_density_ridges(
    aes(group = interaction(tile, !!sym(fill_var)), colour = !!sym(fill_var)),
    scale = 1,        # vertical scaling
    alpha = 0.2,      # transparency so overlapping colours are visible
    bandwidth = 2     # smooth the curve
  ) +
  
  scale_fill_manual(values = scenario_palette, name = "Manipulation") +
  scale_colour_manual(values = scenario_palette, guide = "none") + 
  
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35") +
  coord_cartesian(xlim = c(-x_range, x_range)) +            # clip range
  scale_x_continuous(breaks = seq(-x_range, x_range, by = 5)) + 
  
  # Add mean_height as text labels at the right side
  geom_text(
    data = heights,
    aes(x = label_x_pos, 
        y = as.numeric(factor(tile)) + y_offset, 
        label = paste0(mean_height, " m")),
    inherit.aes = FALSE,   # prevents inheriting other mappings
    hjust = 1,             # right-align the text
    size = 3.5,             # adjust text size
    colour = "grey35"
  ) +
  
  theme_minimal(base_size = 14) +
  labs(x = "Difference [m]",
       y = "Tile") +
  theme(legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text  = element_text(size = 10),
        panel.grid.minor = element_blank()       # don't show minor grid lines
  )+
  guides(
    fill = guide_legend(
      override.aes = list(
        alpha = 0.2,
        colour = scenario_palette, # add colored outline
        size = 1.6        # thickness of outline
      )
    )
  )


