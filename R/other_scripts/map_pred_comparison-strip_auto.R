# ==========================
# Comparison Strips per tile + band incl export
# ==========================
# Works for non-interaction results, with prediction tifs exported -> base_dir

library(terra)
library(tmap)
library(dplyr)
library(stringr)
library(viridis)
library(purrr)

base_dir <- "/data/ESA99/export/2026-01-29/predictions/"
out_dir  <- "/data/ESA99/export/2026-01-29/comparison_grids_by_band/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

agg_fact <- 4

bands <- c("B02","B03","B04","B05","B8A","B08","B11","B12")

change_levels <- c(
  "-25%", "-20%", "-15%", "-10%", "-5%",
  "Original",
  "+5%", "+10%", "+15%", "+20%", "+25%"
)

order_pattern <- c(
  "25_D", "20_D", "15_D", "10_D", "05_D",
  "original",
  "05_I", "10_I", "15_I", "20_I", "25_I"
)

get_tile_zlim <- function(tile) {
  
  files <- list.files(
    base_dir,
    pattern = paste0("^", tile, "_.*\\.tif$"),
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    return(c(NA, NA))
  }
  
  mins <- numeric(length(files))
  maxs <- numeric(length(files))
  
  for (i in seq_along(files)) {
    r <- aggregate(rast(files[i]), fact = agg_fact)
    g <- global(r, fun = range, na.rm = TRUE)
    mins[i] <- g[1,1]
    maxs[i] <- g[1,2] 
  }
  
  c(min(mins, na.rm = TRUE),
    max(maxs, na.rm = TRUE))
}

get_tile_band_stack <- function(tile, band) {
  
  # --- tile-level original ---
  original_file <- list.files(
    base_dir,
    pattern = paste0("^", tile, "_original\\.tif$"),
    full.names = TRUE
  )
  
  # --- band-specific perturbations ---
  band_files <- list.files(
    base_dir,
    pattern = paste0("^", tile, "_", band, "_[0-9]{2}_[DI]\\.tif$"),
    full.names = TRUE
  )
  
  if (length(band_files) == 0 && length(original_file) == 0) {
    return(NULL)
  }
  
  df <- tibble(file = band_files) %>%
    mutate(
      pct = as.integer(str_extract(file, "(?<=_)[0-9]{2}(?=_[DI]\\.tif$)")),
      dir = str_extract(file, "(?<=_)[DI](?=\\.tif$)"),
      change = if_else(
        dir == "D",
        paste0("-", pct, "%"),
        paste0("+", pct, "%")
      )
    ) %>%
    select(file, change)
  
  # add original explicitly (no regex ambiguity)
  if (length(original_file) == 1) {
    df <- bind_rows(
      df,
      tibble(file = original_file, change = "Original")
    )
  }
  
  df <- df %>%
    mutate(change = factor(change, levels = change_levels)) %>%
    arrange(change) %>%
    distinct(change, .keep_all = TRUE)   # GUARANTEE uniqueness
  
  # safety check
  if (anyDuplicated(df$change)) {
    stop("Duplicate change labels detected for ", tile, " | ", band)
  }
  
  # read rasters
  rasters <- lapply(df$file, function(f) {
    aggregate(rast(f), fact = agg_fact)
  })
  
  stack <- rast(rasters)
  names(stack) <- as.character(df$change)
  
  return(stack)
}

plot_tile_band <- function(tile, band, zlim) {
  
  r <- get_tile_band_stack(tile, band)
  if (is.null(r)) return(NULL)
  
  p <- tm_shape(r) +
    tm_raster(
      col.scale = tm_scale_continuous(
        values = viridis(20),
        limits = zlim
      ),
      col.legend = tm_legend(
        title = "Height [m]",
        orientation = "horizontal"
      )
    ) +
    tm_facets_wrap(ncol = 11) +
    tm_layout(
      panel.labels = names(r),
      panel.label.size = 0.9,
      panel.label.fontface = "bold",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      asp = 1
    ) +
    tm_title(paste(tile, "|", band))
  
  out_file <- file.path(
    out_dir,
    paste0(tile, "_", band, "_comparison.png")
  )
  
  tmap_save(
    p,
    filename = out_file,
    width = 24,
    height = 4,
    dpi = 300,
    units = "in"
  )
}


tiles <- unique(str_extract(list.files(base_dir), "^[^_]+"))

tmap_mode("plot")

for (tile in tiles) {
  
  message("Computing zlim for tile: ", tile)
  tile_zlim <- get_tile_zlim(tile)
  
  for (band in bands) {
    message("Processing: ", tile, " | ", band)
    plot_tile_band(tile, band, tile_zlim)
    gc()
  }
}





###########################################################################
# Big comparison Grid of all bands ----------------------------------------


base_dir <- "/data/ESA99/export/2026-01-19/predictions/"
files <- list.files(base_dir, pattern = "\\.tif$", full.names = TRUE)

meta <- tibble(
  file = files,
  name = basename(files)
) %>%
  mutate(
    tile = str_extract(name, "^[^_]+"),
    band = str_extract(name, "B[0-9A]+"),
    pct  = str_extract(name, "(?<=_)\\d{2}(?=_)"),
    dir  = str_extract(name, "(?<=_)[:alpha:]+(?=\\.tif)")
  ) %>%
  mutate(
    pct = as.integer(pct),
    change = case_when(
      name == paste0(tile, "_original.tif") ~ "Original",
      dir == "D" ~ paste0("-", pct, "%"),
      dir == "I" ~ paste0("+", pct, "%")
    )
  )

