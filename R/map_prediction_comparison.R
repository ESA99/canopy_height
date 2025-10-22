library(terra)
library(tmap)

tif_files <- list.files("/data/ESA99/resultmaps_bands/",
                        pattern = "49UCP.*\\.tif$",
                        full.names = TRUE)

# Read tifs in correct order 
order_pattern <- c("25_D", "20_D", "15_D", "10_D", "05_D",
                   "original",
                   "05_I", "10_I", "15_I", "20_I", "25_I")

extract_order_index <- function(x, order_pattern) {
  match(TRUE, sapply(order_pattern, grepl, x = x))
}

order_index <- sapply(tif_files, extract_order_index, order_pattern = order_pattern)
tif_files <- tif_files[order(order_index)]

rasters <- lapply(tif_files, rast)

# Define titles
titles <- c("-25%", "-20%", "-15%", "-10%", "-5%",
            "Original",
            "+5%", "+10%", "+15%", "+20%", "+25%")

# Compute global min/max for consistent color scale
all_values <- unlist(lapply(rasters, function(r) values(r, na.rm = TRUE)))
global_min <- min(all_values, na.rm = TRUE)
global_max <- max(all_values, na.rm = TRUE)


# Create a tmap for each raster
tmap_list <- lapply(seq_along(rasters), function(i) {
  tm_shape(rasters[[i]]) +
    tm_raster(
      title = titles[i],
      palette = "viridis",      # consistent palette
      style = "cont",
      breaks = seq(global_min, global_max, length.out = 20),
      legend.show = TRUE
    ) +
    tm_layout(
      main.title = titles[i],
      main.title.size = 0.8,
      legend.outside = FALSE
    )
})

tmap_mode("plot")  
tmap_arrange(
  plotlist = tmap_list,
  ncol = length(tmap_list),   # horizontal layout
  sync = TRUE,                # shared legend/color scale
  outer.margins = 0
)






#######


tmap_list <- lapply(seq_along(rasters), function(i) {
  tm_shape(rasters[[i]]) +
    tm_raster(
      palette = "viridis",
      style = "cont",
      breaks = seq(global_min, global_max, length.out = 20),
      legend.show = (i == length(rasters))  # show legend only once (on last)
    ) +
    tm_layout(
      main.title = titles[i],
      main.title.size = 0.9,
      main.title.position = "center",
      legend.position = c("center", "bottom"),
      legend.outside = TRUE,
      legend.outside.position = "bottom"
    )
})

# Arrange horizontally with shared legend
tmap_mode("plot")
tmap_arrange(
  plotlist = tmap_list,
  ncol = length(tmap_list),
  sync = TRUE,                # shared color scale
  legend.outside = TRUE,      # show one shared legend
  legend.outside.position = "bottom"
)
