### Plot pixel shuffle differences ###

invisible(lapply(c("terra","dplyr","purrr","ggplot2","ggrepel","viridis","tidyterra"), 
                 require, character.only = TRUE))

# SETUP
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", "35VML" = "Finland", 
  "49NHC" = "Malaysia", "49UCP" = "Mongolia","34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")


# Global shuffle results
result_table <- read.csv("results/2026-05-19_result_table.csv") |>
  mutate(
    Location = factor(tile, levels = names(tile_label), labels = tile_label),
    patch_size = c(1)
  )

# result_table <- read.csv("results/2026-04-28_result_table.csv") |>
#   mutate(
# shuffle_percentage = ifelse(
#   grepl("original$", out_name),
#   0,
#   as.numeric(sub(".*shuffle", "", out_name))
# ),
# Location = factor(tile, levels = names(tile_label), labels = tile_label)
# )

# Local Shuffle 512x512 Results
# result_table <- read.csv("results/2026-05-12_local-shuffle512.csv") |>
#   mutate(Location = factor(tile, levels = names(tile_label), labels = tile_label))


# Plots -------------------------------------------------------------------
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
  
  avg_label <- data %>%
    group_by(shuffle_percentage) %>%
    summarise(value = mean(.data[[y_var]]), .groups = "drop") %>%
    filter(shuffle_percentage == max(shuffle_percentage))
  
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

plot_shuffle(result_table, "average_difference", "Average difference [m]")
plot_shuffle(result_table, "avg_difference_percent", "Average relative difference [%]")
plot_shuffle(result_table, "avg_abs_diff", "Absolute average difference [m]")
plot_shuffle(result_table, "avg_abs_diff_perc", "Absolute average relative difference [%]")

plot_shuffle_byTile(result_table, "average_difference", "Average difference [m]", TRUE)
plot_shuffle_byTile(result_table, "avg_difference_percent", "Average relative difference [%]", TRUE, 8)
plot_shuffle_byTile(result_table, "avg_abs_diff", "Absolute average difference [m]", TRUE, avg_nudge = 10)
plot_shuffle_byTile(result_table, "avg_abs_diff_perc", "Absolute average relative difference [%]", TRUE, avg_nudge = 1)

#### EXPORT ####

# Wide save:
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_wide.png"), width = 270, height = 175, units = "mm", dpi = 300, bg = "white")
# Medium save:
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
# Tall save
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_",filename ,"_tall.png"), width = 200, height = 250, units = "mm", dpi = 300, bg = "white")







##################################################################

# c("terra","dplyr","purrr", "ggplot2","ggpubr","ggrepel","viridis", "tidyterra") |>
#   lapply(FUN = library, character.only = TRUE) |>
#   invisible()

# lapply(c("terra","dplyr","purrr","ggplot2","ggpubr","ggrepel","viridis"), function(pkg) {
#   if (require(pkg, character.only = TRUE)) {
#     message(pkg, " loaded")
#   } else {
#     message(pkg, " not installed")
#   }
# })

### DATA ###
# result_table <- read.csv("results/2026-04-28_result_table.csv")
# 
# # If percentage column is missing:
# result_table$shuffle_percentage <- ifelse(
#   grepl("original$", result_table$out_name),
#   0,
#   as.numeric(sub(".*shuffle", "", result_table$out_name))
# )
# # Add Location column as factor based on tiles
# tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany",
#                 "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia",
#                 "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")
# 
# result_table$Location <- factor(result_table$tile, levels = names(tile_label), labels = tile_label)