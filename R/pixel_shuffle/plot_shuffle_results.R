### Plot pixel shuffle differences ###

c("terra","dplyr", "ggplot2","ggpubr","ggrepel","viridis", "tidyterra") |>
  lapply(FUN = library, character.only = TRUE) |>
  invisible()

# lapply(c("terra","dplyr","ggplot2","ggpubr","ggrepel","viridis"), function(pkg) {
#   if (require(pkg, character.only = TRUE)) {
#     message(pkg, " loaded")
#   } else {
#     message(pkg, " not installed")
#   }
# })

### DATA ###
result_table <- read.csv("results/2026-04-28_result_table.csv")

# If percentage column is missing:
result_table$shuffle_percentage <- ifelse(
  grepl("original$", result_table$out_name),
  0,
  as.numeric(sub(".*shuffle", "", result_table$out_name))
)
# Add Location column as factor based on tiles
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany",
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia",
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")

result_table$Location <- factor(result_table$tile, levels = names(tile_label), labels = tile_label)


### Plot

# Average with SD
ggplot(result_table, aes(x = shuffle_percentage, y = avg_difference_percent)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2) +
  scale_x_continuous(breaks = c(0,10,20,30,50,80)) +
  labs( x = "Pixel Shuffle [%]",  y = "Average Relative Difference [%]"  ) +
  theme_minimal(base_size = 14)

ggplot(result_table, aes(x = shuffle_percentage, y = average_difference)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2) +
  scale_x_continuous(breaks = c(0,10,20,30,50,80)) +
  labs( x = "Pixel Shuffle [%]",  y = "Average Difference [m]"  ) +
  theme_minimal(base_size = 14)


### Labled line plot by tile
label_data <- result_table %>%
  group_by(Location, shuffle_percentage) %>%
  summarise(average_difference = mean(average_difference),
            .groups = "drop") %>%
  group_by(Location) %>%
  filter(shuffle_percentage == max(shuffle_percentage))

ggplot(result_table,
       aes(x = shuffle_percentage, y = average_difference,
           color = Location, fill = Location)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon",
               alpha = 0.15, color = NA) +
  geom_text_repel(data = label_data,
    aes(label = Location),
    direction = "y",
    hjust = 0, nudge_x = 1,
    segment.color = NA,
    size = 4,
    show.legend = FALSE) +
  
  # geom_hline(yintercept = 0, col = "grey40")+
  annotate("segment",
    x = 0, xend = 80, y = 0, yend = 0,
    linetype = "dashed", color = "grey60",linewidth = 0.8 )+
  
  scale_color_viridis_d(option = "D") +
  # scale_fill_viridis_d(option = "C") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)),
                     breaks = c(0,10,20,30,50,80)) +
  labs(x = "Pixel Shuffle [%]", y = "Average Difference [m]") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")





#### Inlcude average

ggplot(result_table,
       aes(x = shuffle_percentage,
           y = average_difference,
           color = Location,
           fill = Location)) +
  # per-location trends (your original structure)
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon",
               alpha = 0.15, color = NA) +
  # GLOBAL mean line (all locations combined)
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.6,
    color = "purple" ) +
  # GLOBAL uncertainty band
  stat_summary(
    aes(group = 1),
    fun.data = mean_se,
    geom = "ribbon",
    fill = "purple",
    alpha = 0.10,
    color = NA) +
  geom_text_repel(
    data = label_data,
    aes(label = Location),
    direction = "y",
    hjust = 0,
    nudge_x = 1,
    segment.color = NA,
    size = 4,
    show.legend = FALSE ) +
  annotate(
    "segment",
    x = 0, xend = 80,
    y = 0, yend = 0,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.8  ) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.15)),
    breaks = c(0,10,20,30,50,80)  ) +
  labs( x = "Pixel Shuffle [%]",y = "Average Difference [m]") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")





#### EXPORT ####

# Wide save:
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_PLOTNAME_wide.png"), width = 270, height = 175, units = "mm", dpi = 300, bg = "white")
# Medium save:
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_PLOTNAME_medium.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")
# Tall save
# ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_PLOTNAME_tall.png"), width = 200, height = 250, units = "mm", dpi = 300, bg = "white")










##### Difference raster visualization ####

shuffle_degree <- "20"
tile <- "17SNB"
base_dir <- "/data/ESA99/export/2026-04-28"

"/data/ESA99/export/2026-04-28/predictions/10TES_original.tif"
original_rast <- rast(file.path(base_dir, "predictions", paste0(tile,"_original",".tif") )  )
names(original_rast) <- paste0(tile, " original prediction")
diff_rast <- rast(file.path(base_dir, "difference_rasters", paste0("DIFF_",tile,"_shuffle",shuffle_degree,".tif"))  )
names(diff_rast) <- paste0(shuffle_degree, " % shuffled | Difference raster")
lim <- max(abs(values(diff_rast)), na.rm = TRUE)

mean_diff <- mean(na.omit(values(diff_rast)))


ggplot() +
  # Original raster first
  geom_spatraster(data = original_rast, maxcell = 5e5) +
  scale_fill_viridis(
    name = "Original [m]", 
    option = "D"
  ) +
  
  # Tell ggplot we will use a new fill scale for differences
  ggnewscale::new_scale_fill() +
  
  # Difference rasters
  geom_spatraster(data = diff_rast, maxcell = 5e5) +
  scale_fill_gradient2(
    name = "Difference [m]",
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-lim, lim)
  ) +
  
  # Facets
  facet_wrap(~lyr) +
  
  # Layout
  coord_sf(expand = FALSE) +
  scale_x_continuous(n.breaks = 4) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 9),
    panel.spacing = unit(0.2, "lines"),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


### Zonal statistics based on difference values
mean_neg <- global(mask(original_rast, diff_rast < 0, maskvalues = 0),
                   fun = "mean", na.rm = TRUE)[1, 1] |> round(digits = 1)

mean_pos <- global(mask(original_rast, diff_rast > 0, maskvalues = 0),
                   fun = "mean", na.rm = TRUE)[1, 1] |> round(digits = 1)



## Auto for all combinations
all_tiles <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV") 
all_degrees <-  c(10, 20, 30, 50, 80)
combinations <- expand.grid(tile = all_tiles, perc = all_degrees)

mean_results <- data_frame(
  tile = character(nrow(combinations)),
  shuffled = numeric(nrow(combinations)),
  mean_neg = numeric(nrow(combinations)),
  mean_pos = numeric(nrow(combinations)),
  mean_diff = numeric(nrow(combinations))
)

pb <- txtProgressBar(min = 0, max = nrow(combinations), style = 3)

for (i in 1:nrow(combinations)) {
  
  setTxtProgressBar(pb, i)
  
  base_dir <- "/data/ESA99/export/2026-04-28"
  shuffle_degree <- combinations$perc[i]
  tile <- combinations$tile[i]
  
  original_rast <- rast(file.path(base_dir, "predictions", paste0(tile,"_original",".tif") )  )
  diff_rast <- rast(file.path(base_dir, "difference_rasters", paste0("DIFF_",tile,"_shuffle",shuffle_degree,".tif"))  )
  
  mean_neg <- global(mask(original_rast, diff_rast < 0, maskvalues = 0),
                     fun = "mean", na.rm = TRUE)[1, 1] |> round(digits = 1)
  
  mean_pos <- global(mask(original_rast, diff_rast > 0, maskvalues = 0),
                     fun = "mean", na.rm = TRUE)[1, 1] |> round(digits = 1)
  
  mean_diff <- mean(na.omit(values(diff_rast)))
  
  loop_results <- list(
    tile = tile,
    shuffled = shuffle_degree,
    mean_neg = mean_neg,
    mean_pos = mean_pos,
    mean_diff = mean_diff
  )
  
  mean_results[i, ] <- loop_results
  
}
close(pb)


### Vizualization

names(mean_results) <- c("Tile", "Shuffle_percent", "CH_posDiff", "CH_negDiff", "meanDiff")
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany",
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia",
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")
mean_results$Tile <- factor(mean_results$Tile, levels = names(tile_label), labels = tile_label)


plot_data <- mean_results %>%
  pivot_longer(
    cols = c(CH_negDiff, CH_posDiff, meanDiff),
    names_to = "metric",
    values_to = "value"
  )

plot_data <- plot_data %>%
  mutate(
    Shuffle_percent = factor(Shuffle_percent, levels = sort(unique(Shuffle_percent))),
    metric = factor(metric, levels = c("CH_negDiff", "CH_posDiff", "meanDiff"))
  )

ggplot(plot_data,
       aes(x = Shuffle_percent,
           y = value,
           fill = metric)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~Tile) +
  scale_fill_viridis_d(option = "D",
                       breaks = c("CH_negDiff", "CH_posDiff", "meanDiff"),
                       labels = c("avg CH in negativ Diff.","avg CH in positive Diff.", "Mean difference")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs( x = "Pixel Shuffle [%]",y = "Value [m]",  fill = "Metric" ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing = unit(0.3, "lines"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank() )
