### Tree Height ridge plot ###
library(terra)
library(dplyr)
library(ggridges)
library(forcats)

### DATA ###
f <- list.files("/home/emilio/canopy_height/results/originals/", full.names = T)

tile_label <- c(
  "55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon",
  "32UQU" = "Germany", "35VML" = "Finland", "49NHC" = "Malaysia",
  "49UCP" = "Mongolia", "34UFD" = "Poland", "32TMT" = "Switzerland",
  "10TES" = "USA East", "17SNB" = "USA West"
)

# Compute densities  -------------------------------------------------

densities <- lapply(f, function(fpath) {
  
  r <- rast(fpath)
  vals <- values(r, mat = FALSE)
  vals <- vals[!is.na(vals)]
  vals <- vals[vals > 0 & vals < 60]
  
  dens <- density(
    vals,
    n = 2048,
    bw = 2
  )
  
  tile <- substr(basename(fpath), 1, 5)
  
  data.frame(
    Location    = tile_label[tile],
    Height      = dens$x,
    Density     = dens$y,
    mean_height = mean(vals),
    median_height = median(vals)
  )
}) |> bind_rows()

densities <- densities %>%
  mutate(Location = fct_reorder(Location, mean_height, .desc = TRUE))

densities <- densities %>%
  mutate(Location = fct_reorder(Location, median_height, .desc = TRUE))

ggplot(
  densities,
  aes(
    x = Height,
    y = Location,
    height = Density,
    fill = mean_height
  )
) +
  geom_density_ridges(
    stat = "identity",
    scale = 1.5,
    alpha = 0.95,
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradient(
    low = "#AED570",
    high = "#136217",
    name = "Mean height [m]"
  ) +
  labs(
    x = "Predicted tree height [m]",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(
    panel.grid.major.y = element_blank(),
    legend.title = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) + 
  guides(
    fill = guide_colorbar(
      barwidth = unit(9, "cm"),
      barheight = unit(0.5, "cm")
    )
  )
# +
#   geom_text(
#     aes(x = mean_height, y = Location, label = round(mean_height, 1)),
#     color = "black",
#     nudge_y = 0.1
#   )



# Statistics --------------------------------------------------------------

location_stats <- densities %>%
  group_by(Location) %>%
  summarise(
    mean_height   = first(mean_height),
    median_height = first(median_height)
  ) %>%
  arrange(mean_height)

location_stats

arrange(location_stats, desc(median_height))
# arrange(location_stats, desc(mean_height))


# First draft -------------------------------------------------------------


ggplot(
  densities,
  aes(
    x = Height,
    y = Location,
    height = Density,
    fill = mean_height
  )
) +
  geom_ridgeline(
    scale = 7,
    alpha = 0.95,
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradient(
    low = "#AED570",
    high = "#136217",
    name = "Mean height (m)"
  ) +
  labs(
    x = "Predicted tree height (m)",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank())

