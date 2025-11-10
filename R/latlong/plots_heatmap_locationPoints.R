# HEATMAP
library(ggplot2)
library(viridis)

results <- read.csv("/data/ESA99/lat_lon_results/global_summary_with_differences.csv")
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", 
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia", 
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")
results$Location <- factor(results$tile, levels = names(tile_label), labels = tile_label)

results$point <- as.character(paste0(results$lat,"_", results$lon))

results$point_lon <- as.character(paste0(results$lon,"_", results$lat))

head(results)



ggplot(results,
       aes(x = point, y = Location, fill = avg_differece_percent)) +
  geom_tile(color = "white") +
  # scale_fill_viridis(option = "cividis") +
  scale_fill_gradient2(
    low = "#0571B0",
    mid = "#ECEADA",
    high = "#CA0020",
    midpoint = 0
  ) +
  labs(title = "Difference to original prediction per location at each prediction point.",
       x = "Point", y = "Location", fill = "Mean rel\ndiff (%)") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(
      angle = 50,          # Rotate text
      hjust = 1,           # Right-justify so labels align neatly
      vjust = 1            # Adjust vertical alignment
    )
  )
