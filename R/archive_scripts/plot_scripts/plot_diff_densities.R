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

tile_names <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV")
manip_deg <- c("05", "10", "15", "20", "25")
bands <- c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12")
directions <- c("I", "D")
sample_size = 150000


tile_selection <- tile_names[1]
band_selection <- bands[5]
direction_selection <- directions[1]


### Get all file infos in one table ###
files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)

meta <- tibble(file = files) %>%
  mutate(filename = basename(file)) %>%
  separate(
    filename,
    into = c("Prefix", "Tile", "Band", "Increment", "Direction"),
    sep = "_",
    remove = TRUE
  ) %>%
  mutate(
    Direction = str_remove(Direction, "\\.tif$")
  ) %>%
  select(-Prefix)


meta_original <- meta %>%
  filter(Band == "original.tif")%>%
  mutate(
    Band = str_remove(Band, "\\.tif$"),
    Increment = "00",
  )

## LOOP START HERE ?? -> per tile? ###

### Filter to desired tiles ###
selection <- meta %>%
  filter(Tile %in% tile_selection,
         Band %in% band_selection,
         Direction %in% direction_selection)

selection_originals <- meta_original %>%
  filter(Tile %in% tile_selection)



# Read rasters
r_original <- rast(selection_originals$file[1])

r_diff_list <- lapply(selection$file, rast)
names(r_diff_list) <- paste0(
  selection$Increment,
  "_",
  selection$Direction
)

# Extract pixel values
df_original <- as.data.frame(r_original, xy = FALSE, na.rm = TRUE)
colnames(df_original) <- "height"


df_diff <- lapply(r_diff_list, function(r) {
  as.data.frame(r, xy = FALSE, na.rm = TRUE)
})

df_diff <- bind_cols(df_diff)

# Combine
df_all <- bind_cols(df_original, df_diff)


# Convert format
df_long <- df_all %>%
  pivot_longer(
    cols = -height,
    names_to = "scenario",
    values_to = "difference"
  )

set.seed(42)
df_long_sample <- df_long %>%
  slice_sample(n = sample_size)


## Difference densities
ggplot(df_long_sample,
       aes(x = difference,
           colour = scenario,
           fill = scenario)) +
  geom_density(alpha = 0.2) +
  theme_minimal()


# Violin plot
ggplot(df_long_sample,
       aes(y = scenario,
           x = difference,
           fill = scenario)) +
  geom_violin(trim = TRUE, alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(df_long_sample,
       aes(y = scenario,
           x = difference,
           fill = scenario)) +
  geom_violin(scale = "width",
              width = 0.8,
              alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none")















