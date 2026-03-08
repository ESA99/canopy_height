# Setup -------------------------------------------------------------------
library(terra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(hexbin)
library(ggrepel)
library(viridis)
library(RColorBrewer)

diff_folder <- "/data/ESA99/export/all/difference_rasters/"
orig_folder <- "/data/ESA99/export/2026-01-29/predictions/"

tile_names <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV")
bands <- c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12")
directions <- c("I", "D")

sample_size <- 100000

tile_label <- c( "55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", 
                 "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia", "34UFD" = "Poland", 
                 "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")

scenario_levels <- c("-25%", "-20%", "-15%", "-10%", "-5%",
                     "+5%", "+10%", "+15%", "+20%", "+25%")

# Height values (rounded version later)
# mean_height <- data.frame( 
#   tile = c("Finland", "Mongolia", "Poland", "Australia", "Germany",
#            "Switzerland", "Cameroon", "USA East", "USA West","Brazil", "Malaysia"),
#   height = c(4.56, 4.77, 6.19, 9.27, 10.1, 17.2, 21.1, 21.1, 22.8,33.3, 43.5) )


# Files and meta data table -----------------------------------------------

files <- list.files(diff_folder, pattern = "\\.tif$", full.names = TRUE)

meta <- tibble(file = files) %>%
  mutate(filename = basename(file)) %>%
  separate(
    filename,
    into = c("Prefix", "Tile", "Band", "Increment", "Direction"),
    sep = "_",
    remove = TRUE
  ) %>%
  mutate(Direction = str_remove(Direction, "\\.tif$")) %>%
  select(-Prefix)

meta_original <- meta %>%
  filter(Band == "original.tif") %>%
  mutate(
    Band = str_remove(Band, "\\.tif$"),
    Increment = "00"
  )


# Process -----------------------------------------------------------------

all_tiles_long <- lapply(tile_names, function(tile_sel) {
  
  # 1) Select rasters
  selection <- meta %>% 
    filter(Tile == tile_sel,
           Band != "original.tif")   # exclude original from curves
  
  selection_originals <- meta_original %>% 
    filter(Tile == tile_sel)
  
  
  # 2) Sample baseline height
  r_orig <- rast(selection_originals$file[1])
  vals_orig <- sample(values(r_orig, mat = FALSE), sample_size)
  
  
  # 3) Sample scenario rasters
  scenario_samples <- lapply(selection$file, function(f) {
    r <- rast(f)
    sample(values(r, mat = FALSE), sample_size)
  })
  
  
  # 4) Combine into wide format
  df_tile <- data.frame(
    height = vals_orig,
    do.call(cbind, scenario_samples)
  )
  
  colnames(df_tile)[-1] <- paste0(selection$Increment, "_", selection$Direction)
  
  
  # 5) Convert to long + clean
  df_tile_long <- df_tile %>%
    
    pivot_longer(
      cols = -height,
      names_to = "scenario_raw",
      values_to = "difference"
    ) %>%
    
    filter(!is.na(height)) %>%
    filter(!scenario_raw %in% c("NA", "NA_NA", NA)) %>%
    
    # Signed numeric manipulation
    mutate(
      scenario_num = case_when(
        grepl("_I$", scenario_raw) ~  as.numeric(sub("_I$", "", scenario_raw)),
        grepl("_D$", scenario_raw) ~ -as.numeric(sub("_D$", "", scenario_raw)),
        TRUE ~ NA_real_
      )
    ) %>%
    
    filter(!is.na(scenario_num)) %>%
    
    mutate(
      # Signed factor with %
      scenario = factor(
        paste0(ifelse(scenario_num > 0, "+", ""), scenario_num, "%"),
        levels = scenario_levels
      ),
      
      # Absolute manipulation degree
      abs_scenario = factor(
        paste0(abs(scenario_num), "%"),
        levels = paste0(sort(unique(abs(scenario_num))), "%")
      ),
      
      tile = tile_label[tile_sel]
    ) %>%
    
    select(tile, height, scenario, abs_scenario, difference)
  
  df_tile_long
  
}) %>% bind_rows()



# Save/Import Results -----------------------------------------------------

# saveRDS(all_tiles_long, file = "R/data/all_tiles_long.rds", compress = TRUE)
all_tiles_long <- readRDS("R/data/all_tiles_long.rds")
str(all_tiles_long)



# Add mean heights and order tiles --------------------------------------------------------

# Apply factor manually by mean canopy height
tile_order <- c("Finland", "Mongolia", "Poland", "Australia", "Germany", "Switzerland", "Cameroon", "USA East", "USA West", "Brazil", "Malaysia")
all_tiles_long$tile <- factor(all_tiles_long$tile, levels = heights$tile)

# # Order By median difference
# all_tiles_long <- all_tiles_long %>%
#     group_by(tile) %>%
#     mutate(median_diff = median(difference)) %>%
#     ungroup() %>%
#     mutate(tile = factor(tile, levels = unique(tile[order(median_diff, decreasing = TRUE)]))) # decreasing = TRUE → largest median on top

# Mean height label data
heights <-data.frame(
  tile = c("Finland", "Mongolia", "Poland", "Australia", "Germany", 
           "Switzerland", "Cameroon", "USA East", "USA West", "Brazil", "Malaysia"),
  mean_height = c(4.6, 4.8, 6.2, 9.3, 10.1, 17.2, 21.1, 21.1, 22.8, 33.3, 43.5)
) 

# Order heights df
heights <- heights %>%
  mutate(tile = factor(tile, levels = tile_order))


# Select scenario ---------------------------------------------------------

# # Filter for desired scenario
# filter_scenarios <- "both"  # options: "positive", "negative", "both"
# 
# all_tiles_long <- all_tiles_long %>%
#   filter(
#     (filter_scenarios == "positive" & grepl("^\\+", scenario)) |
#       (filter_scenarios == "negative" & grepl("^\\-", scenario)) |
#       (filter_scenarios == "both")
#   )


# Plot variables ----------------------------------------------------------

# Select variables
fill_var <- "abs_scenario"
# fill_var <- "scenario"

# scale_var <- "discrete"
scale_var <- "continuous"

x_range <- 15

# Mean height label position
label_x_pos <- x_range * 0.98
y_offset <- 0.20 

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

# Shaded density curves, ridge plot -> tiles sorted by CH, 
ggplot(all_tiles_long,
       aes(x = difference,
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



# Export ------------------------------------------------------------------

# Tall format to fit inner margins of A4 document, with space for caption
ggsave(paste0("plots/",format(Sys.Date(), "%Y-%m-%d"),"_DiffDist_TileRidge_MH_disc_5.png"), width = 200, height = 260, units = "mm", dpi = 300, bg = "white")




# EXPERIMENTAL -> AS FUNCTION ---------------------------------------------
#####################################################################################################
#####################################################################################################

