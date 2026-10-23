# invisible(lapply(c("terra","dplyr","purrr","ggplot2","ggrepel","viridis","tidyterra", "progressr"), 
#                  require, character.only = TRUE))
invisible(lapply(pkgs <- c("terra","dplyr","purrr","ggplot2","ggrepel","viridis","tidyterra","progressr"), 
                 function(p) { if(!require(p, character.only = TRUE)) install.packages(p); library(p, character.only = TRUE) }))

##### Difference raster visualization ####

# "10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"
shuffle_degree <- "20"
tile <- "32TMT"
base_dir <- file.path("/data", "ESA99", "export", "2026-04-28")

# "/data/ESA99/export/2026-04-28/predictions/10TES_original.tif"
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

attr(mean_results, "created") <- Sys.time()
attr(mean_results, "source") <- base_dir
write.csv(mean_results,
          "R/pixel_shuffle/mean_results_2026-04-28.csv",
          row.names = FALSE)
mean_results <- read.csv("R/pixel_shuffle/mean_results_2026-04-28.csv")


## CLEANER ALTERNATIVE
base_dir <- "/data/ESA99/export/2026-04-28"
mean_results <- pmap_dfr(combinations, function(tile, perc) {
  
  original_rast <- rast(file.path(base_dir, "predictions",
                                  paste0(tile, "_original.tif")))
  
  diff_rast <- rast(file.path(base_dir, "difference_rasters",
                              paste0("DIFF_", tile, "_shuffle", perc, ".tif")))
  
  tibble(
    tile = tile,
    shuffled = perc,
    mean_neg = round(global(mask(original_rast, diff_rast < 0), "mean", na.rm = TRUE)[1,1], 1),
    mean_pos = round(global(mask(original_rast, diff_rast > 0), "mean", na.rm = TRUE)[1,1], 1),
    mean_diff = round(global(diff_rast, "mean", na.rm = TRUE)[1,1], 1)
  )
})
### WITH PROGRESS
handlers(global = TRUE)
handlers("txtprogressbar")
mean_results <- with_progress({
  p <- progressor(along = 1:nrow(combinations))
  
  pmap_dfr(combinations, function(tile, perc) {
    p()
    
    original_rast <- rast(file.path(base_dir, "predictions",
                                    paste0(tile, "_original.tif")))
    
    diff_rast <- rast(file.path(base_dir, "difference_rasters",
                                paste0("DIFF_", tile, "_shuffle", perc, ".tif")))
    
    tibble(
      tile = tile,
      shuffled = perc,
      mean_neg = global(mask(original_rast, diff_rast < 0), "mean", na.rm = TRUE)[1,1],
      mean_pos = global(mask(original_rast, diff_rast > 0), "mean", na.rm = TRUE)[1,1],
      mean_diff = global(diff_rast, "mean", na.rm = TRUE)[1,1]
    )
  })
})


### Boxplot Violin plot
mean_long <- mean_results %>%
  pivot_longer(
    cols = c(mean_neg, mean_pos),
    names_to = "type",
    values_to = "value"
  )

ggplot(mean_long, aes(x = type, y = value, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  labs(
    x = NULL,
    y = "Mean canopy height [m]",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggplot(mean_long, aes(x = type, y = value, group = tile)) +
  geom_line(alpha = 0.3, color = "grey50") +
  geom_point(aes(color = type), size = 2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2, color = "black") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3, color = "black") +
  labs(
    x = NULL,
    y = "Mean canopy height [m]"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave(paste0("plots/pixel_shuffle/",format(Sys.Date(), "%Y-%m-%d"),"_ConnectPairs_CHcomparison.png"), width = 250, height = 200, units = "mm", dpi = 300, bg = "white")



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
