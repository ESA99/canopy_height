### Plot pixel shuffle differences ###

c("terra","dplyr", "ggplot2","ggpubr","ggrepel","viridis") |>
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


d <- rast("/data/ESA99/export/2026-04-28/difference_rasters/DIFF_10TES_shuffle20.tif")
d <- rast("/data/ESA99/export/2026-04-28/difference_rasters/DIFF_17SNB_shuffle20.tif")

mean(na.omit(values(d)))
lim <- max(abs(values(d)), na.rm = TRUE)


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
