# Plot functions for all manipulation types ------------------------------

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
    group_by(band, Colour, abs_increment) %>%
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
            color = Colour,
            fill = Colour
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
          aes(y = value, label = band),
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

  filename <<- paste0("mean_", y_var, "_labels")

  return(p)
}

plot_spectral_facets <- function(data, y_var, y_lab){

  plot_data <- data %>%
    group_by(band, Location, abs_increment) %>%
    summarise(
      value = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      manipulation = abs_increment
    )

  p <- ggline(
          plot_data,
          x = "manipulation",
          y = "value",
          color = "Location",
          fill  = "Location",
          add = "mean_se",
          linewidth = 1.2,
          alpha = 0.2,
          palette = tile_colors,
          facet.by = "band",
          scales = "fixed",
          ncol = 2
        ) +

        geom_hline(
          yintercept = c(-100, -50, 50, 100),
          linetype = "dashed",
          color = "grey85",
          linewidth = 0.6
        ) +

        geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "grey30"
        ) +

        labs(
          x = "Manipulation Degree [%]",
          y = y_lab,
          color = "Location",
          fill  = "Location"
        ) +

        theme_pubr(base_size = 14) +

        theme(
          legend.position = "bottom"
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
            color = band,
            fill = band
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

  filename <<- paste0("mean_", y_var)

  return(p)
}

plot_location_band <- function(data, band_name, y_var, y_lab){

  # map correct SD column
  sd_var <- switch(
    y_var,
    mean_change = "SD",
    relative_mean_change = "relative_sd_change",
    NULL
  )

  # filter band
  plot_data <- data %>%
    filter(Colour == band_name) %>%
    mutate(
      Location = factor(Location)
    )

  # order legend (at increment 25 if available)
  legend_order <- legend_order %>%
  dplyr::arrange(desc(.data[[y_var]])) %>%
  dplyr::pull(Location) %>%
  unique()

  plot_data <- plot_data %>%
    mutate(Location = factor(Location, levels = legend_order))

  p <- ggplot(
          plot_data,
          aes(
            x = increment,
            y = .data[[y_var]],
            color = Location,
            fill = Location,
            group = Location
          )
        ) +

        geom_ribbon(
          aes(
            ymin = .data[[y_var]] - .data[[sd_var]],
            ymax = .data[[y_var]] + .data[[sd_var]]
          ),
          alpha = 0.2,
          color = NA
        ) +

        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +

        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +

        # IMPORTANT: use your spectral color map
        scale_color_manual(values = spectral_colors) +
        scale_fill_manual(values = spectral_colors) +

        labs(
          x = "Manipulation [%]",
          y = y_lab,
          color = "Location",
          fill = "Location"
        ) +

        theme_minimal(base_size = 14) +
        theme(
          legend.position = "right",
          legend.box = "vertical",
          axis.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )

  filename <<- paste0("location_", band_name, "_", y_var)

  return(p)
}


plot_location_band <- function(data, band_name, y_var, y_lab){

  # ---- SD mapping (safe + explicit) ----
  sd_var <- switch(
    y_var,
    mean_change = "SD",
    relative_mean_change = "relative_sd_change",
    NULL
  )

  if (is.null(sd_var) || !sd_var %in% names(data)) {
    stop("No valid SD column found for y_var: ", y_var)
  }

  # ---- filter band ----
  plot_data <- data %>%
    dplyr::filter(Colour == band_name)

  # ---- legend order (safe fallback if increment 25 not present) ----
  legend_order <- plot_data %>%
    dplyr::filter(increment == 25)

  if (nrow(legend_order) == 0) {
    legend_order <- plot_data
  }

  legend_order <- legend_order %>%
    dplyr::arrange(desc(.data[[y_var]])) %>%
    dplyr::pull(Location) %>%
    unique()

  plot_data <- plot_data %>%
    dplyr::mutate(
      Location = factor(Location, levels = legend_order)
    )

  # ---- plot ----
  p <- ggplot(
    plot_data,
    aes(
      x = increment,
      y = .data[[y_var]],
      color = Location,
      fill = Location,
      group = Location
    )
  ) +

    geom_ribbon(
      aes(
        ymin = .data[[y_var]] - .data[[sd_var]],
        ymax = .data[[y_var]] + .data[[sd_var]],
        group = Location
      ),
      alpha = 0.2,
      color = NA
    ) +

    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +

    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +

    # ---- correct palette ----
    scale_color_manual(values = spectral_colors) +
    scale_fill_manual(values = spectral_colors) +

    labs(
      x = "Manipulation [%]",
      y = y_lab,
      color = "Location",
      fill  = "Location"
    ) +

    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  filename <<- paste0("location_", band_name, "_", y_var)

  return(p)
}


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
    dplyr::group_by(Location, shuffle_percentage) %>%
    dplyr::summarise(value = mean(.data[[y_var]]), .groups = "drop") %>%
    dplyr::group_by(Location) %>%
    dplyr::filter(shuffle_percentage == max(shuffle_percentage))
  
  
  # Base plot
  p <- ggplot(data,
              aes(x = shuffle_percentage,
                  y = .data[[y_var]],
                  color = Location,
                  fill = Location)) +
    
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon",
                 alpha = 0.15, color = NA) +
    
    geom_text_repel(
      data = label_data,
      aes(y = value, label = Location),
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


# GEOGRAPHICAL -----------------------------------------------------------

