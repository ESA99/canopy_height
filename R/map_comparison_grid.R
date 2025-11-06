library(terra)
library(tmap)
library(ggplot2)
library(viridis)
library(cowplot)
library(cols4all)


tif_files <- list.files("/data/ESA99/resultmaps_bands/49UCP/",
                        pattern = "49UCP.*\\.tif$",
                        full.names = TRUE)

# Read tifs in correct order 
order_pattern <- c("25_D", "20_D", "15_D", "10_D", "05_D",
                   "original",
                   "05_I", "10_I", "15_I", "20_I", "25_I")

# Define titles
titles <- c("-25%", "-20%", "-15%", "-10%", "-5%",
            "Original",
            "+5%", "+10%", "+15%", "+20%", "+25%")


extract_order_index <- function(x, order_pattern) {
  match(TRUE, sapply(order_pattern, grepl, x = x))
}

order_index <- sapply(tif_files, extract_order_index, order_pattern = order_pattern)
tif_files <- tif_files[order(order_index)]


# Read rasters in correct order into a list
rasters <- lapply(tif_files, rast)


# GGPLOT ------------------------------------------------------------------

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



# TMAP --------------------------------------------------------------------

# Determine global min/max
all_values <- unlist(lapply(rasters, function(r) values(r, na.rm = TRUE)))
global_min <- min(all_values, na.rm = TRUE)
global_max <- max(all_values, na.rm = TRUE)

# Define a color palette
palette_vals <- c4a("brewer.rd_yl_bu", n = 20)

# Generate maps in a list
tmap_list <- lapply(seq_along(rasters), function(i) {
  tm_shape(rasters[[i]]) +
    tm_raster(
      col.scale = tm_scale_continuous(
        values = palette_vals,
        ticks = seq(global_min, global_max, length.out = 20)
      ),
      col.legend = if (i == length(rasters)) tm_legend(title = "Height [m]") else tm_legend_hide()
    ) +
    tm_title(titles[i], size = 0.9)
})

# Arrange smaller batches to avoid graphics timeout
tmap_mode("plot")

# Example: arrange in 2 rows of 6 columns (adjust to fit your screen)
tmap_arrange(
  plotlist = tmap_list,
  ncol = 11,
  sync = TRUE,
  legend.outside = TRUE,
  legend.outside.position = "right"
)



# TMAP Improvement Try ----------------------------------------------------

# Stack into one multi-layer raster
stacked <- rast(rasters)
names(stacked) <- titles

# color scale range ---
all_values <- unlist(values(stacked, na.rm = TRUE))
global_min <- min(all_values, na.rm = TRUE)
global_max <- max(all_values, na.rm = TRUE)

# palette_vals <- c4a("brewer.rd_yl_bu", n = 20)
palette_vals <- viridis(20)

tmap_mode("plot")

tm_shape(stacked) +
  tm_raster(
    col.scale = tm_scale_continuous(
      values = palette_vals,
      limits = c(global_min, global_max)
    ),
    col.legend = tm_legend(
      title = "Height [m]",
      orientation = "horizontal",
      position = "bottom"
    )
  ) +
  tm_legend(
    title = "Height [m]",
    orientation = "horizontal",
    position = "bottom"
  )+
  tm_facets_wrap(
    ncol = 11,
    free.scales = FALSE
  ) +
  tm_layout(
    asp = 1,
    frame = TRUE,
    panel.labels = titles,
    panel.label.size = 1.0,
    panel.label.fontface = "bold",
    panel.label.bg.color = "white",
    legend.show = TRUE,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.title.size = 1.0,
    legend.text.size = 0.8
    # outer.margins = c(0.02, 0.02, 0.12, 0.02)
  )


# tmap_save(
#   tmap_arrange(final_plot, legend_plot, ncol = 2, widths = c(5, 1)),
#   filename = "maps_with_side_legend.png",
#   width = 18, height = 8, units = "in", dpi = 300
# )

