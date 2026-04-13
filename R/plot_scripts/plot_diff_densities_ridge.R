# Setup -------------------------------------------------------------------
library(terra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(hexbin)
library(ggrepel)
library(viridis)

diff_folder <- "/data/ESA99/export/all/difference_rasters/"
orig_folder <- "/data/ESA99/export/2026-01-29/predictions/"

tile_names <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", 
                "35VML", "49NHC", "49UCP", "55HEV")
bands <- c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12")
directions <- c("I", "D")
sample_size <- 100000


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



# Table -------------------------------------------------------------------

all_tiles_long <- lapply(tile_names, function(tile_sel) {
  
  # Filter rasters
  selection <- meta %>% filter(Tile == tile_sel, Band != "original.tif")
  selection_originals <- meta_original %>% filter(Tile == tile_sel)
  
  # Read original raster and sample pixels
  r_orig <- rast(selection_originals$file[1])
  vals_orig <- sample(values(r_orig, mat = FALSE), sample_size)
  
  # Initialize list to hold scenario samples
  scenario_samples <- lapply(selection$file, function(f) {
    r <- rast(f)
    sample(values(r, mat = FALSE), sample_size)
  })
  
  # Combine into a data frame
  df_tile <- data.frame(
    height = vals_orig,
    do.call(cbind, scenario_samples)
  )
  
  # Name scenario columns
  colnames(df_tile)[-1] <- paste0(selection$Increment, "_", selection$Direction)
  
  # Long format
  df_tile_long <- df_tile %>%
    pivot_longer(
      cols = -height,
      names_to = "scenario",
      values_to = "difference"
    ) %>%
    mutate(tile = tile_sel)
  
  df_tile_long
}) %>% bind_rows()



# Relable tiles
tile_label <- c(
  "55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon",
  "32UQU" = "Germany", "35VML" = "Finland", "49NHC" = "Malaysia",
  "49UCP" = "Mongolia", "34UFD" = "Poland", "32TMT" = "Switzerland",
  "10TES" = "USA East", "17SNB" = "USA West"
)

all_tiles_long <- all_tiles_long %>%
  mutate(tile = tile_label[tile])

all_tiles_long_allbands$tile <- factor(all_tiles_long_allbands$tile,
                                       levels = unique(all_tiles_long_allbands$tile))

# Relable increment scenarios and remove NA
all_tiles_long <- all_tiles_long %>%
  # Remove NA scenario if present
  filter(!scenario %in% c("NA", NA)) %>%
  # Recode scenario to +/- percent
  mutate(
    scenario = case_when(
      grepl("_I$", scenario) ~ paste0("+", as.numeric(sub("_I$", "", scenario)), "%"),
      grepl("_D$", scenario) ~ paste0("-", as.numeric(sub("_D$", "", scenario)), "%"),
      TRUE ~ scenario
    )
  )

# Remove NA
all_tiles_long <- all_tiles_long %>%
  filter(!scenario %in% c("NA_NA", NA))

all_tiles_long <- all_tiles_long %>%
  filter(!is.na(height))

all_tiles_long <- all_tiles_long %>%
  filter(!is.na(height)) %>%                       # remove raster NAs
  filter(!scenario %in% c("NA_NA", NA)) %>%        # remove invalid scenarios
  mutate(
    scenario = factor(scenario,
                      levels = c("-25%", "-20%", "-15%", "-10%", "-5%",
                                 "+5%", "+10%", "+15%", "+20%", "+25%"))
  )



##### EXPORT TABLE #####
# saveRDS(all_tiles_long, file = "R/data/all_tiles_long.rds", compress = TRUE)
all_tiles_long <- readRDS("R/data/all_tiles_long.rds")
str(all_tiles_long)


# Filter for desired scenario
filter_scenarios <- "both"  # options: "positive", "negative", "both"

all_tiles_long <- all_tiles_long %>%
  filter(
    (filter_scenarios == "positive" & grepl("^\\+", scenario)) |
      (filter_scenarios == "negative" & grepl("^\\-", scenario)) |
      (filter_scenarios == "both")
  )


# Colour scale
scenario_levels <- sort(unique(all_tiles_long$scenario))  # order scenarios
scenario_palette <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e")  # 5 colors
scenario_palette <- c("#66CCEE","#228833","#CCBB44","#EE6677","#AA3377")  # 5 colors
scenario_palette <- c("#AA3377","#EE6677","#CCBB44","#228833","#66CCEE")  # 5 colors
names(scenario_palette) <- scenario_levels

scenario_palette <- c(
  "#d73027","#fc8d59","#fee08b","#d9ef8b","#91cf60",   # negatives -25% to -5%
  "#66c2a5","#3288bd","#5e4fa2","#9e0168","#fdae61"    # positives +5% to +25%
)
names(scenario_palette) <- scenario_levels

scenario_palette <- c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', 
                      '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD')


library(viridis)
# 10 colours for 10 scenarios
scenario_levels <- c("-25%", "-20%", "-15%", "-10%", "-5%",
                     "+5%", "+10%", "+15%", "+20%", "+25%")
scenario_palette <- viridis::viridis(n = length(scenario_levels), option = "D")
names(scenario_palette) <- scenario_levels


# Adjust Tile order
# # by Median difference
# backup <- all_tiles_long
# all_tiles_long <- all_tiles_long %>%
#   group_by(tile) %>%
#   mutate(median_diff = median(difference)) %>%
#   ungroup() %>%
#   mutate(tile = factor(tile, levels = unique(tile[order(median_diff, decreasing = TRUE)]))) # decreasing = TRUE → largest median on top

# Manual order
tile_order <- c("Finland", "Mongolia", "Poland", "Australia", "Germany", "Switzerland", "Cameroon", "USA East", "USA West", "Brazil", "Malaysia")  
all_tiles_long$tile <- factor(all_tiles_long$tile, levels = tile_order)



# Plots -------------------------------------------------------------------
### Vididis, 1 line per scenario,
library(ggridges)
ggplot(all_tiles_long,
       aes(x = difference,
           y = tile,
           colour = scenario)) +
  geom_density_ridges(
    aes(group = interaction(tile, scenario)),
    fill = NA,         # only lines
    scale = 1.5,         # vertical scaling
    alpha = 0.8,       # transparency
    size = 0.8,
    bandwidth = 2      # smooth the curve
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35") +
  scale_colour_viridis_d(option = "D") +
  # scale_colour_manual(values = scenario_palette) +
  coord_cartesian(xlim = c(-20, 20)) +
  theme_minimal(base_size = 14) +
  labs(x = "Difference [m]",
       y = "Tile",
       colour = "Manipulation") +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "right"
  )



### CBF palette, shaded curves
ggplot(all_tiles_long,
       aes(x = difference,
           y = tile,
           fill = abs_scenario)) +   # fill colours
  geom_density_ridges(
    aes(group = interaction(tile, abs_scenario), colour = abs_scenario),
    scale = 1,        # vertical scaling
    alpha = 0.2,      # transparency so overlapping colours are visible
    bandwidth = 2     # smooth the curve
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35") +
  
  scale_fill_manual(values = scenario_palette, name = "Manipulation") +   # discrete colours
  scale_colour_manual(values = scenario_palette, guide = "none") +        # discrete colours
  # scale_fill_viridis_d(option = "C", name = "Manipulation") +
  # scale_colour_viridis_d(option = "C", guide = "none") +

  coord_cartesian(xlim = c(-20, 20)) +            # clip range
  theme_minimal(base_size = 14) +
  labs(x = "Difference [m]",
       y = "Tile") +
  theme(legend.position = "right")+
  guides(
    fill = guide_legend(
      override.aes = list(
        alpha = 0.2,
        colour = scenario_palette, # add colored outline
        size = 1.6        # thickness of outline
      )
    )
  )




#### Absolute manipulation degree
# remove + and % and get numeric values
abs_vals <- abs(as.numeric(gsub("[+%]", "", as.character(all_tiles_long$scenario))))

# create a factor with levels in ascending numeric order, keep % 
all_tiles_long$abs_scenario <- factor(
  paste0(abs_vals, "%"),
  levels = paste0(sort(unique(abs_vals)), "%")
)

unique(all_tiles_long$abs_scenario)





# all_tiles_long$abs_scenario <- paste0(abs(as.numeric(gsub("[+%]", "", as.character(all_tiles_long$scenario)))), "%")
# 
# # all_tiles_long$abs_scenario <- as.numeric(gsub("[+%]", "", as.character(all_tiles_long$scenario)))
# # all_tiles_long$abs_scenario <- abs(all_tiles_long$abs_scenario)
# 
# all_tiles_long$abs_scenario <- factor(all_tiles_long$abs_scenario, 
#                                       levels = sort(unique(all_tiles_long$abs_scenario)))