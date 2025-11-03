library(terra)
library(ggplot2)
library(viridis)
library(cowplot)


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


# Define titles
titles <- c("-25%", "-20%", "-15%", "-10%", "-5%",
            "Original",
            "+5%", "+10%", "+15%", "+20%", "+25%")




# read all rasters
rasters <- lapply(tif_files, rast)

# determine global min/max for shared scale
all_vals <- unlist(lapply(rasters, values))
zlim <- range(all_vals, na.rm = TRUE)

# create a ggplot object for each raster
plots <- lapply(seq_along(rasters), function(i) {
  df <- as.data.frame(rasters[[i]], xy = TRUE)
  names(df)[3] <- "value"
  
  ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis(name = "Height [m]", limits = zlim, option = "D") +
    coord_equal() +
    ggtitle(titles[i]) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
})

# extract legend from one plot
legend <- cowplot::get_legend(
  plots[[1]] + theme(legend.position = "bottom")
)

# arrange plots horizontally and add shared legend
final_plot <- cowplot::plot_grid(
  plotlist = plots,
  nrow = 1,
  align = "hv"
)

# combine with legend
final_with_legend <- cowplot::plot_grid(
  final_plot,
  legend,
  ncol = 1
)

# display
print(final_with_legend)


ggsave("/data/ESA99/combined_maps_2.png", final_with_legend, width = 24, height = 3.5, dpi = 300, bg = "white")
