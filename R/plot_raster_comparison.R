library(dplyr)
library(terra)
library(ggplot2)
library(ggnewscale)

# Meta data ---------------------------------------------------------------

diff_folder <- "/data/ESA99/export/all/difference_rasters"
pred_folder <- "/data/ESA99/export/all/predictions"

diff_files <- list.files(diff_folder, pattern = "\\.tif$", full.names = TRUE)
orig_files <- list.files(pred_folder, pattern = "original", full.names = T)
pred_files <- tibble(file = list.files(pred_folder, full.names = TRUE)) %>%
  filter(!grepl("original", file))

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


meta_pred <- pred_files %>%
  mutate(filename = basename(file)) %>%
  separate(
    filename,
    into = c("Tile", "Band", "Increment", "Direction"),
    sep = "_",
    remove = TRUE
  ) %>%
  mutate(Direction = str_remove(Direction, "\\.tif$"),
         label = paste0(ifelse(Direction == "I", "+", "-"), as.character(Increment), " %")) 




# Variable selection ------------------------------------------------------
# "10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"

tile_sel <- "10TES"
band_sel <- "B02"
direct_sel <- "I"

### Original selection
orig_sel <- meta_original %>%
  filter(Tile == tile_sel)

o <- rast(orig_sel$file)
names(o) <- "Original"


### Prediction selection
pred_sel <- meta_pred %>%
  filter(Tile == tile_sel,
         Band == band_sel,
         Direction == direct_sel) %>%
  arrange(as.numeric(Increment))

p <- rast(pred_sel$file)
names(p) <- paste0("+",as.character(pred_sel$Increment), " %")


### Difference selection
# Filter one tile-band combination
diff_sel <- meta %>%
  filter(Tile == tile_sel,
         Band == band_sel,
         Direction == direct_sel) %>%
  arrange(as.numeric(Increment))

d <- rast(diff_sel$file)
names(d) <- paste0("+",as.character(diff_sel$Increment), " %")
lim <- max(abs(values(d)), na.rm = TRUE)


### Combined
# original + predictions
combined_op <- c(o, p)
lyr_names <- names(combined)

# original + difference
combined_od <- c(o, d)
lyr_names <- names(combined_od)


# Prediction rasters -------------------------------------------------------

ggplot() +
  geom_spatraster(data = p, maxcell = 5e5) +
  facet_wrap(~lyr, nrow = 1) +
  scale_fill_viridis(name = "Difference [m]", option = "D") + # E also good
  coord_sf(expand = FALSE) +
  scale_x_continuous(n.breaks = 3)+
  theme_minimal(base_size = 12) +
  theme(
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    # axis.ticks = element_blank(),
    # panel.grid = element_blank(),
    
    panel.spacing = unit(0.2, "lines"),
    strip.text = element_text(face = "bold"),
    
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Difference rasters ------------------------------------------------------

ggplot() +
  geom_spatraster(data = d, maxcell = 5e5) +
  facet_wrap(~lyr, nrow = 1) +
  scale_fill_gradient2(
    name = "Difference [m]",
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-lim, lim)
  ) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(n.breaks = 3)+
  theme_minimal(base_size = 12) +
  theme(
    # axis.title = element_blank(),
    # axis.text = element_blank(),
    # axis.ticks = element_blank(),
    # panel.grid = element_blank(),
    
    panel.spacing = unit(0.2, "lines"),
    strip.text = element_text(face = "bold"),
    
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


# Combined original + prediction --------------------------------------------------

ggplot() +
  geom_spatraster(data = combined_op, maxcell = 5e5) +
  facet_wrap(~lyr, nrow = 1, labeller = labeller(lyr = setNames(lyr_names, lyr_names))) +
  scale_fill_viridis(name = "Prediction [m]", option = "D") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(n.breaks = 3) +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing = unit(0.2, "lines"),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Combined original + difference --------------------------------------------------


ggplot() +
  # Original raster first
  geom_spatraster(data = o, maxcell = 5e5) +
  scale_fill_viridis(
    name = "Original [m]", 
    option = "D"
  ) +
  
  # Tell ggplot we will use a new fill scale for differences
  ggnewscale::new_scale_fill() +
  
  # Difference rasters
  geom_spatraster(data = d, maxcell = 5e5) +
  scale_fill_gradient2(
    name = "Difference [m]",
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-lim, lim)
  ) +
  
  # Facets
  facet_wrap(~lyr, nrow = 1, labeller = labeller(lyr = setNames(lyr_names, lyr_names))) +
  
  # Layout
  coord_sf(expand = FALSE) +
  scale_x_continuous(n.breaks = 5) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(0.2, "lines"),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



# Export ------------------------------------------------------------------

ggsave(paste0("plots/comp_strips/",format(Sys.Date(), "%Y-%m-%d"),"_USAWest_Blue_Odiff.png"), width = 300, height = 100, units = "mm", dpi = 300, bg = "white")

