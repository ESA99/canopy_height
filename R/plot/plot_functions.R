# Plot functions for all manipulation types ------------------------------
library(ggplot2)
library(ggpubr)

### COLORS ###
# All pallets were carefully selected to be colour blind friendly and intuitive to understand

spectral_colors <- c( Blue     = "#0072B2", Green    = "#009E73", 
                      Red      = "#D55E00", RedEdge  = "#CC79A7",
                      NIR      = "#9E0142", NIR2     = "#5E4FA2", 
                      SWIR1    = "#E6AB02", SWIR2    = "#999999"
)

spectral_interactions_colors <- c(ALL = "#009E73", 
                                  High = "#DDCC77", 
                                  Low = "#CC79A7", 
                                  RGB = "#882255", 
                                  Blue = "#88CCEE"
)

tile_colors <- c( "#332288", "#6699CC", "#88CCEE",  
                  "#44AA99", "#117733", "#999933",  
                  "#DDCC77", "#661100", "#CC6677", 
                  "#882255",  "#AA4499"
)


# SPECTRAL ---------------------------------------------------------------

# Right sided line plot with labels at the end
plot_spectral_labels <- function(data, y_var, y_lab){

  label_data <- data %>%
    group_by(band, colour, abs_increment) %>%
    summarise(
      value = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(band) %>%
    filter(abs_increment == max(abs_increment))

  p <- ggplot(
          data,
          aes(
            x = abs_increment,
            y = .data[[y_var]],
            color = colour,
            fill = colour
          )
        ) +

        stat_summary(
          fun = mean,
          geom = "line",
          linewidth = 1.2
        ) +

        stat_summary(
          fun.data = mean_se,
          geom = "ribbon",
          alpha = 0.2,
          color = NA
        ) +

        geom_text_repel(
          data = label_data,
          aes(y = value, label = colour),
          direction = "y",
          hjust = 0,
          nudge_x = 0.5,
          segment.color = NA,
          size = 4,
          show.legend = FALSE
        ) +

        scale_color_manual(
          values = spectral_colors,
          breaks = c(
            "Blue", "NIR2", "NIR", "Green",
            "SWIR2", "Red", "SWIR1", "RedEdge"
          )
        ) +

        scale_fill_manual(
          values = spectral_colors,
          breaks = c(
            "Blue", "NIR2", "NIR", "Green",
            "SWIR2", "Red", "SWIR1", "RedEdge"
          )
        ) +

        scale_x_continuous(
          expand = expansion(mult = c(0.02, 0.15))
        ) +

        labs(
          x = "Manipulation Degree [%]",
          y = y_lab
        ) +

        theme_minimal(base_size = 14) +
        theme(legend.position = "none") +
        coord_cartesian(clip = "off")

  filename <<- paste0("line_labels_", y_var)

  return(p)
}

plot_spectral_facets <- function(data, y_var, y_lab) {

  plot_data <- data %>%
    group_by(band, location, abs_increment) %>%
    summarise(
      value = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      manipulation = abs_increment
    )

  # ---- range ----
  y_range <- range(plot_data$value, na.rm = TRUE)
  pad <- 0.03 * diff(y_range)

  y_min <- y_range[1] - pad
  y_max <- y_range[2] + pad

  # ---- automatic reference grid ----
  raw_step <- (y_max - y_min) / 4

  nice_step <- 10^floor(log10(raw_step))
  if (raw_step / nice_step >= 5) nice_step <- nice_step * 5
  else if (raw_step / nice_step >= 2) nice_step <- nice_step * 2

  ref_lines <- seq(
    floor(y_min / nice_step) * nice_step,
    ceiling(y_max / nice_step) * nice_step,
    by = nice_step
  )

  dashed_lines <- ref_lines[abs(ref_lines) > 1e-9]

  p <- ggline(
    plot_data,
    x = "manipulation",
    y = "value",
    color = "location",
    linewidth = 1.2,
    alpha = 0.2,
    palette = tile_colors,
    facet.by = "band",
    scales = "free_y",
    ncol = 2
  ) +

    geom_hline(
      yintercept = dashed_lines,
      linetype = "dashed",
      color = "grey85",
      linewidth = 0.6
    ) +

    geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = "grey20",
      linewidth = 0.8
    ) +

    scale_y_continuous(
      expand = c(0, 0)
    ) +

    labs(
      x = "Manipulation Degree [%]",
      y = y_lab,
      color = "location",
      fill  = "location"
    ) +

    theme_pubr(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.spacing = unit(0.6, "lines")
    )

  filename <<- paste0("facet_", y_var)

  return(p)
}

plot_spectral_butterfly <- function(data, y_var, y_lab){

  p <- ggplot(
          data,
          aes(
            x = increment,
            y = .data[[y_var]],
            color = colour,
            fill = colour
          )
        ) +
        stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
        stat_summary(fun.data = mean_se,
                     geom = "ribbon",
                     alpha = 0.2,
                     color = NA) +
        scale_color_manual(
          values = spectral_colors,
          breaks = c(
            "Blue", "Green", "Red", "RedEdge",
            "NIR", "NIR2", "SWIR1", "SWIR2"
          )
        ) +
        scale_fill_manual(
          values = spectral_colors,
          breaks = c(
            "Blue", "Green", "Red", "RedEdge",
            "NIR", "NIR2", "SWIR1", "SWIR2"
          )
        ) +
        labs(
          x = "Manipulation [%]",
          y = y_lab
        ) +
        theme_minimal(base_size = 14)

  filename <<- paste0("butterfly_", y_var)

  return(p)
}

plot_spectral_band <- function(data, band_name, y_var, y_lab, x_var = c("increment", "abs_increment")) {

  x_var <- match.arg(x_var)

  plot_data <- data %>%
    dplyr::filter(colour == band_name)

  band_col <- spectral_colors[[band_name]]

  # -----------------------------
  # aggregate data (same structure as working reference)
  # -----------------------------
  plot_data <- plot_data %>%
    dplyr::group_by(location, .data[[x_var]]) %>%
    dplyr::summarise(
      value = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )

  # -----------------------------
  # label data: last x per location (stable endpoint)
  # -----------------------------
  label_data <- plot_data %>%
    dplyr::group_by(location) %>%
    dplyr::filter(.data[[x_var]] == max(.data[[x_var]], na.rm = TRUE)) %>%
    dplyr::ungroup()

  # -----------------------------
  # plot
  # -----------------------------
  p <- ggplot(
    plot_data,
    aes(
      x = .data[[x_var]],
      y = value,
      group = location
    )
  ) +

    geom_line(
      colour = band_col,
      linewidth = 1.1,
      alpha = 0.85
    ) +

    geom_point(
      colour = band_col,
      size = 2
    ) +

    geom_text_repel(
      data = label_data,
      aes(
        x = .data[[x_var]],
        y = value,
        label = location
      ),
      inherit.aes = FALSE,
      colour = band_col,
      direction = "y",
      hjust = 0,
      nudge_x = 0.5,
      segment.color = NA,
      size = 4,
      box.padding = 0.4,
      point.padding = 0.3,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +

    scale_x_continuous(
      expand = expansion(mult = c(0.02, 0.18))
    ) +

    labs(
      title = band_name,
      x = ifelse(
        x_var == "increment",
        "Manipulation [%]",
        "Manipulation Degree [%]"
      ),
      y = y_lab
    ) +

    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none"
    ) +

    coord_cartesian(clip = "off")

  filename <<- paste0(y_var, "_", band_name, "_byTile")

  return(p)
}

# plot_spectral_facets <- function(data, y_var, y_lab, line_spacing = 50, n_lines = 3) {

#   # Create symmetric reference lines
#   # hlines <- sort(c(-reference_lines, reference_lines))
#     hlines <- seq(
#     line_spacing,
#     line_spacing * n_lines,
#     by = line_spacing
#   )

#   hlines <- c(-rev(hlines), hlines)

#   plot_data <- data %>%
#     group_by(band, location, abs_increment) %>%
#     summarise(
#       value = mean(.data[[y_var]], na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       manipulation = abs_increment
#     )

#   p <- ggline(
#           plot_data,
#           x = "manipulation",
#           y = "value",
#           color = "location",
#           # add = "mean_se",
#           linewidth = 1.2,
#           alpha = 0.2,
#           palette = tile_colors,
#           facet.by = "band",
#           scales = "free",
#           ncol = 2
#         ) +

#         geom_hline(
#           yintercept = hlines,
#           linetype = "dashed",
#           color = "grey85",
#           linewidth = 0.6
#         ) +

#         geom_hline(
#           yintercept = 0,
#           linetype = "dashed",
#           color = "grey30"
#         ) +

#         labs(
#           x = "Manipulation Degree [%]",
#           y = y_lab,
#           color = "location",
#           fill  = "location"
#         ) +

#         theme_pubr(base_size = 14) +
#         theme(
#           legend.position = "bottom"
#         )

#   filename <<- paste0("facet_", y_var)

#   return(p)
# }

# plot_spectral_band <- function(data, band_name, y_var, y_lab, x_var = c("increment", "abs_increment")) {

#   x_var <- match.arg(x_var)

#   plot_data <- data %>%
#     filter(colour == band_name)

#   label_data <- plot_data %>%
#     group_by(location) %>%
#     filter(.data[[x_var]] == max(.data[[x_var]], na.rm = TRUE)) %>%
#     slice_tail(n = 1) %>%
#     ungroup()

#   p <- ggplot(
#           plot_data,
#           aes(
#             x = .data[[x_var]],
#             y = .data[[y_var]],
#             group = location
#           )
#         ) +

#         geom_line(
#           colour = spectral_colors[[band_name]],
#           linewidth = 1.1,
#           alpha = 0.8
#         ) +

#         geom_point(
#           colour = spectral_colors[[band_name]],
#           size = 2
#         ) +

#         geom_text_repel(
#           data = label_data,
#           aes(label = location),
#           direction = "y",
#           hjust = 0,
#           nudge_x = 0.5,
#           segment.color = "grey70",
#           size = 4,
#           inherit.aes = TRUE
#         ) +

#         scale_x_continuous(
#           expand = expansion(mult = c(0.02, 0.15))
#         ) +

#         labs(
#           title = band_name,
#           x = ifelse(
#             x_var == "increment",
#             "Manipulation [%]",
#             "Manipulation Degree [%]"
#           ),
#           y = y_lab
#         ) +

#         theme_minimal(base_size = 14) +
#         theme(
#           legend.position = "none"
#         ) +

#         coord_cartesian(clip = "off")

#   filename <<- paste0(y_var, "_", band_name, "_byTile")
  
#   return(p)
# }

# SHUFFLE ----------------------------------------------------------------
# Average with SD
plot_shuffle <- function(data, y_var, y_lab){
  p <- ggplot(data, aes(x = shuffle_percentage, y = .data[[y_var]] )) +
          stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
          stat_summary(fun = mean, geom = "point", size = 2) +
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 2) +
          scale_x_continuous(breaks = c(0,10,20,30,50,80)) +
          labs( x = "Pixel Shuffle [%]",  y = y_lab  ) +
          theme_minimal(base_size = 14)
  
  filename <<- paste0("mean_", y_var)
  return(p)
}

plot_shuffle_byTile <- function(data, y_var, y_lab, show_average = FALSE, avg_nudge = 7) {
  
  # Label positions
  label_data <- data %>%
    dplyr::group_by(location, shuffle_percentage) %>%
    dplyr::summarise(value = mean(.data[[y_var]]), .groups = "drop") %>%
    dplyr::group_by(location) %>%
    dplyr::filter(shuffle_percentage == max(shuffle_percentage))
  
  
  # Base plot
  p <- ggplot(data,
              aes(x = shuffle_percentage,
                  y = .data[[y_var]],
                  color = location,
                  fill = location)) +
    
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon",
                 alpha = 0.15, color = NA) +
    
    geom_text_repel(
      data = label_data,
      aes(y = value, label = location),
      direction = "y",
      hjust = 0,
      nudge_x = 1,
      segment.color = NA,
      size = 4,
      show.legend = FALSE
    ) +
    
    annotate(
      "segment",
      x = 0, xend = 80,
      y = 0, yend = 0,
      linetype = "dashed",
      color = "grey60",
      linewidth = 0.8
    ) +
    
    scale_color_viridis_d(option = "D") +
    
    scale_x_continuous(
      expand = expansion(mult = c(0.02, 0.15)),
      breaks = c(0,10,20,30,50,80)
    ) +
    
    labs(
      x = "Pixel Shuffle [%]",
      y = y_lab
    ) +
    
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") +
    coord_cartesian(clip = "off")
  
  # Optional: add global average
  if (show_average) {
    
    avg_label <- data %>%
      dplyr::group_by(shuffle_percentage) %>%
      dplyr::summarise(value = mean(.data[[y_var]]), .groups = "drop") %>%
      dplyr::filter(shuffle_percentage == max(shuffle_percentage))
    
    p <- p +
      stat_summary(
        aes(group = 1),
        fun = mean,
        geom = "line",
        linewidth = 1.6,
        color = "purple"
      ) +
      stat_summary(
        aes(group = 1),
        fun.data = mean_se,
        geom = "ribbon",
        fill = "purple",
        alpha = 0.10,
        color = NA
      )+
      geom_text_repel(
        data = avg_label,
        aes(x = shuffle_percentage, y = value, label = "Average"),
        color = "purple",
        hjust = 0,
        nudge_x = avg_nudge,
        segment.color = NA,
        size = 4.2,
        fontface = "bold",
        inherit.aes = FALSE
      )
  }
  
  filename <<- paste0("byTile_", y_var)
  return(p)
}


### HEATMAP  ###
# plot_shuffle_heatmap <- function(data, value_var, y_lab = NULL) {

#   if (is.null(y_lab)) y_lab <- value_var

#   vals <- data[[value_var]]

#   lims <- quantile(abs(vals), probs = 0.98, na.rm = TRUE)

#   p <- ggplot(
#     data,
#     aes(
#       x = shuffle_percentage,
#       y = factor(patch_size),
#       fill = .data[[value_var]]
#     )
#   ) +

#     geom_tile(color = "white", linewidth = 0.2) +

#     scale_fill_gradient2(
#       low = "#2166AC",
#       mid = "white",
#       high = "#B2182B",
#       midpoint = 0,
#       limits = c(-lims, lims),
#       oob = scales::squish
#     ) +
    
#     # scale_fill_gradient2(
#     #   low = "#2166AC",
#     #   mid = "white",
#     #   high = "#B2182B",
#     #   midpoint = 0,
#     #   trans = "pseudo_log"
#     # )+

#     labs(
#       x = "Shuffle percentage [%]",
#       y = "Patch size",
#       fill = y_lab
#     ) +

#     theme_bw(base_size = 12) +
#     theme(
#       panel.grid = element_blank(),
#       legend.position = "right"
#     )

#   filename <<- paste0("shuffle_heatmap_", value_var)

#   return(p)
# }

plot_shuffle_heatmap <- function(data, value_var, y_lab = NULL) {

  if (is.null(y_lab)) y_lab <- value_var

  # Aggregate to one value per grid cell
  plot_data <- data %>%
    dplyr::group_by(shuffle_percentage, patch_size) %>%
    dplyr::summarise(
      value = mean(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    )

  # Heatmap plot
  p <- ggplot(
    plot_data,
    aes(
      x = shuffle_percentage,
      y = factor(patch_size),
      fill = value
    )
  ) +

    geom_tile(color = "white", linewidth = 0.2) +

    # proper diverging scale centered at 0
    # scale_fill_gradient2(
    #   low = "#2166AC",
    #   mid = "white",
    #   high = "#B2182B",
    #   midpoint = 0
    # ) +
    scale_fill_gradient2(
      low = "#2166AC", # "#2C7BB6"
      mid = "#FFFFE5", # "#FFFFBF"
      high = "#B2182B", # "#D7191C"
      midpoint = 0
    )+
   
    labs(
      x = "Shuffle percentage [%]",
      y = "Patch size",
      fill = y_lab
    ) +

    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )

  filename <<- paste0("shuffle_heatmap_", value_var)

  return(p)
}

plot_shuffle_heatmap_discrete <- function(data, value_var, y_lab = NULL) {

  if (is.null(y_lab)) y_lab <- value_var

  plot_data <- data %>%
    dplyr::group_by(shuffle_percentage, patch_size) %>%
    dplyr::summarise(
      value = mean(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      shuffle_percentage = factor(shuffle_percentage,
                                  levels = sort(unique(shuffle_percentage)))
    )

  p <- ggplot(plot_data, aes(
      x = shuffle_percentage,
      y = factor(patch_size),
      fill = value
    )) +

      geom_tile(color = "white", linewidth = 0.3) +

      scale_fill_gradient2(
        low = "#2166AC",
        mid = "#FFFFE5",
        high = "#B2182B",
        midpoint = 0
      ) +

      labs(
        x = "Shuffle percentage [%]",
        y = "Patch size",
        fill = y_lab
      ) +

      theme_bw(base_size = 12) +
      theme(
        panel.grid = element_blank()
      )
  
  filename <<- paste0("discrete_heatmap_", value_var)

  return(p)

}

plot_shuffle_heatmap_interpolated <- function(data, value_var, y_lab = NULL) {

  if (is.null(y_lab)) y_lab <- value_var

  plot_data <- data %>%
    dplyr::group_by(shuffle_percentage, patch_size) %>%
    dplyr::summarise(
      value = mean(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    )

  # interpolation grid
  interp <- akima::interp(
    x = plot_data$shuffle_percentage,
    y = plot_data$patch_size,
    z = plot_data$value,
    duplicate = "mean"
  )

  interp_df <- expand.grid(
    shuffle_percentage = interp$x,
    patch_size = interp$y
  )

  interp_df$value <- as.vector(interp$z)

  p <- ggplot(interp_df, aes(
      x = shuffle_percentage,
      y = patch_size,
      fill = value
    )) +

      geom_raster() +

      scale_fill_gradient2(
        low = "#2166AC",
        mid = "#FFFFE5",
        high = "#B2182B",
        midpoint = 0,
        na.value = "transparent"
      ) +

      labs(
        x = "Shuffle percentage [%]",
        y = "Patch size",
        fill = y_lab
      ) +

      theme_bw(base_size = 12) +
      theme(
        panel.grid = element_blank()
      )
  
  filename <<- paste0("interpolate_heatmap_", value_var)

  return(p)

}
# plot_shuffle_heatmap_interpolated(shuffle_results, "mean_change")

plot_shuffle_heatmap_tiles <- function(data, value_var, y_lab = NULL) {

  if (is.null(y_lab)) y_lab <- value_var

  plot_data <- data %>%
    dplyr::group_by(tile, shuffle_percentage, patch_size) %>%
    dplyr::summarise(
      value = mean(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      shuffle_percentage = factor(shuffle_percentage,
                                  levels = sort(unique(shuffle_percentage)))
    )

  p <- ggplot(plot_data, aes(
      x = shuffle_percentage,
      y = factor(patch_size),
      fill = value
    )) +

      geom_tile(color = "white", linewidth = 0.25) +

      scale_fill_gradient2(
        low = "#2166AC",
        mid = "#FFFFE5",
        high = "#B2182B",
        midpoint = 0
      ) +

      facet_wrap(~tile) +

      labs(
        x = "Shuffle percentage [%]",
        y = "Patch size",
        fill = y_lab
      ) +

      theme_bw(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top"
      )
  
  filename <<- paste0("tile_heatmap_", value_var)

  return(p)

}



# GEOGRAPHICAL -----------------------------------------------------------

plot_geo_shift <- function(data,y_var, y_lab) {

  p <- ggplot(
          data,
          aes(
            x = shift_distance,
            y = .data[[y_var]],
            color = shift_direction
          )
        ) +

        stat_summary(
          fun = mean,
          geom = "line",
          linewidth = 1
        ) +

        stat_summary(
          fun = mean,
          geom = "point",
          size = 2
        ) +

        stat_summary(
          fun.data = mean_se,
          geom = "errorbar",
          width = 0
        ) +

        labs(
          x = "Shift distance (km)",
          y = y_lab,
          color = "Direction"
        ) +

        theme_bw()

  filename <<- paste0("shift_", y_var)

  return(p)
}

plot_geo_byTile <- function(data, y_var, y_lab) {

  p <- ggplot(
          data,
          aes(
            x = shift_distance,
            y = .data[[y_var]],
            color = shift_direction,
            group = interaction(location, shift_direction)
          )
        ) +

        geom_line(
          alpha = 0.5,
          linewidth = 0.5
        ) +

        geom_point(
          size = 2
        ) +

        facet_wrap(
          ~location
        ) +

        labs(
          x = "Shift distance (km)",
          y = y_lab,
          color = "Direction"
        ) +

        theme_minimal(base_size = 14)

  filename <<- paste0("shift_facet_", y_var)

  return(p)
}

plot_geo_latitude <- function(data, tile_coordinates, y_var,  y_lab) {

  plot_data <- data %>%
    left_join(
      tile_coordinates,
      by = c("tile" = "Name")
    ) %>%
    mutate(
      new_lat = case_when(
        shift_direction == "N" ~ lat + shift_distance / 111.32,
        shift_direction == "S" ~ lat - shift_distance / 111.32,
        TRUE ~ lat
      ),
      dist_equator = abs(new_lat)
    )

  p <- ggplot(
          plot_data,
          aes(
            x = new_lat,
            y = .data[[y_var]],
            group = tile,
            colour = tile
          )
        ) +

        geom_line(
          linewidth = 1
        ) +

        geom_point(
          size = 2
        ) +

        geom_point(
          data = subset(plot_data, original),
          size = 3,
          shape = 21,
          stroke = 1.2,
          fill = "white"
        ) +

        labs(
          x = "Latitude after shift",
          y = y_lab,
          colour = "Tile"
        ) +

        theme_bw()

  filename <<- paste0("latitude_", y_var)

  return(p)
}

plot_geo_tile_trends <- function(data, y_var, y_lab, tile_colors = NULL) {

  # label positions (end of each line)
  label_data <- data %>%
  group_by(tile, location) %>%
  do({
    fit <- lm(.data[[y_var]] ~ dist_equator, data = .)

    x_end <- max(.$dist_equator, na.rm = TRUE)

    data.frame(
      dist_equator = x_end,
      pred = predict(fit, newdata = data.frame(dist_equator = x_end))
    )
  }) %>%
  ungroup()

  p <- ggplot(
          data,
          aes(
            x = dist_equator,
            y = .data[[y_var]]
          )
        ) +

        # points
        geom_point(
          aes(color = tile),
          alpha = 0.4,
          size = 1.5
        ) +

        # per-tile trend lines
        geom_smooth(
          aes(color = tile, group = tile),
          method = "lm",
          se = FALSE,
          linewidth = 0.9,
          alpha = 0.8
        ) +

        # global trend
        geom_smooth(
          aes(group = 1),
          method = "lm",
          se = TRUE,
          color = "black",
          linewidth = 0.6,
          alpha = 0.2
        ) +

        # labels = LOCATION
        ggrepel::geom_text_repel(
          data = label_data,
          aes(
            x = dist_equator,
            y = pred,
            label = location,
            color = tile
          ),
          direction = "y",
          nudge_x = 0.2,
          segment.color = "grey70",
          size = 3.5,
          box.padding = 0.5,
          point.padding = 0.3,
          show.legend = FALSE
        ) +

        labs(
          x = "Distance to equator (° latitude)",
          y = y_lab
        )

  if (!is.null(tile_colors)) {
    p <- p + scale_color_manual(values = tile_colors)
  }

    p <- p +
      theme_bw(base_size = 12) +
      theme(
        legend.position = "none",
        plot.margin = margin(5.5, 50, 5.5, 5.5)
      ) +
      coord_cartesian(clip = "off")

  filename <<- paste0("tile_trends_", y_var)

  return(p)
}

plot_geo_equator_trend <- function(data, y_var, y_lab, tile_colors = NULL) {

  p <- ggplot(
          data,
          aes(
            x = dist_equator,
            y = .data[[y_var]]
          )
        ) +

        geom_point(
          aes(colour = tile),
          size = 2,
          alpha = 0.7
        ) +
        
        geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 1.2) +

        # geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 1.2) +

        labs(
          x = "Absolute latitude distance from equator (°)",
          y = y_lab,
          colour = "Tile"
        ) +

        theme_bw()

  # apply manual palette only if provided
  if (!is.null(tile_colors)) {
    p <- p +
      scale_color_manual(values = tile_colors)
  }

  filename <<- paste0("equator_trend_", y_var)

  return(p)
}

plot_geo_main_trend <- function(data,y_var, y_lab, use_summary = FALSE) {

  if (use_summary) {
    plot_data <- data %>%
      group_by(dist_equator) %>%
      summarise(
        value = mean(.data[[y_var]], na.rm = TRUE),
        .groups = "drop"
      )
    y_mapped <- "value"
  } else {
    plot_data <- data
    y_mapped <- y_var
  }

  p <- ggplot(plot_data, aes(dist_equator, .data[[y_mapped]])) +

    geom_point(alpha = 0.15, size = 1) +

    geom_smooth(
      method = "gam",
      formula = y ~ s(x, k = 10),
      color = "darkgreen",
      linewidth = 1.1,
      se = TRUE
    ) +

    labs(
      x = "Distance to equator (° latitude)",
      y = y_lab
    ) +

    theme_bw()

  filename <<- paste0("main_trend_eq_", y_var)

  return(p)
}



# plot_geo_tile_trends <- function(data, y_var, y_lab, tile_colors = NULL) {

#   # -----------------------------
#   # Label positions (LM endpoint)
#   # -----------------------------
#   label_data <- data %>%
#     group_by(tile, location) %>%
#     do({
#       fit <- lm(.data[[y_var]] ~ dist_equator, data = .)

#       x_end <- max(.$dist_equator, na.rm = TRUE)

#       data.frame(
#         dist_equator = x_end + 0.3,  # push labels slightly right
#         pred = predict(fit, newdata = data.frame(dist_equator = x_end))
#       )
#     }) %>%
#     ungroup()

#   # -----------------------------
#   # Base plot
#   # -----------------------------
#   p <- ggplot(
#     data,
#     aes(
#       x = dist_equator,
#       y = .data[[y_var]]
#     )
#   ) +

#     # points
#     geom_point(
#       aes(color = tile),
#       alpha = 0.4,
#       size = 1.5
#     ) +

#     # per-tile trend lines
#     geom_smooth(
#       aes(color = tile, group = tile),
#       method = "lm",
#       se = FALSE,
#       linewidth = 0.9,
#       alpha = 0.8
#     ) +

#     # global trend
#     geom_smooth(
#       aes(group = 1),
#       method = "lm",
#       se = TRUE,
#       color = "black",
#       linewidth = 0.6,
#       alpha = 0.2
#     ) +

#     # labels (LOCATION)
#     ggrepel::geom_text_repel(
#       data = label_data,
#       aes(
#         x = dist_equator,
#         y = pred,
#         label = location,
#         color = tile
#       ),
#       direction = "y",
#       segment.color = "grey70",
#       size = 3.5,
#       box.padding = 0.6,
#       point.padding = 0.3,
#       max.overlaps = Inf,
#       show.legend = FALSE
#     ) +

#     # axes
#     labs(
#       x = "Distance to equator (° latitude)",
#       y = y_lab
#     ) +

#     # theme (single, consistent)
#     theme_bw(base_size = 12) +
#     theme(
#       legend.position = "none",
#       plot.margin = margin(5.5, 50, 5.5, 5.5)
#     ) +

#     coord_cartesian(clip = "off")

#   # -----------------------------
#   # Optional color override
#   # -----------------------------
#   if (!is.null(tile_colors)) {
#     p <- p + scale_color_manual(values = tile_colors)
#   }

#   filename <<- paste0("tile_trends_", y_var)

#   return(p)
# }
