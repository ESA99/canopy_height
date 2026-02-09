library(terra)
library(ggplot2)
library(hexbin)

# Data
manip_deg <- "10_D"
band <- "B08"
tile <- "49NHC"
tile_names <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV")

original   <- rast(paste0("/data/ESA99/export/2026-01-29/predictions/",tile,"_original.tif"))    # tree canopy height
difference <- rast(paste0("/data/ESA99/export/2026-01-29/difference_rasters/DIFF_",tile,"_",band,"_",manip_deg,".tif"))  # pixel-wise difference

r_stack <- c(original, difference)
names(r_stack) <- c("height", "diff")

# df <- as.data.frame(r_stack, na.rm = TRUE)
df_sample <- spatSample(r_stack, size = 800000, as.df = TRUE, na.rm = TRUE)

# Original predicted tree canopy height vs. NIR2 + 20% difference to original [m]
ggplot(df_sample, aes(x = height, y = diff)) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(
    x = "Tree canopy height",
    y = "Difference",
    title = paste(tile, band, manip_deg)
  ) +
  theme_minimal()


ggplot(df_sample, aes(height, diff)) +
  geom_hex(bins = 45) +
  scale_fill_viridis_c() +
  labs(
    x = "Tree canopy height",
    y = "Difference",
    fill = "Pixel count"
  ) +
  theme_minimal()





# Multiple Manip Comparison -----------------------------------------------

band <- "B08"
tile <- "35VML"
# tile_names <- c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV")

original   <- rast(paste0("/data/ESA99/export/all/predictions/",tile,"_original.tif"))    # tree canopy height
diff_low <- rast(paste0("/data/ESA99/export/all/difference_rasters/DIFF_",tile,"_",band,"_10_I.tif"))  # pixel-wise difference
diff_med <- rast(paste0("/data/ESA99/export/all/difference_rasters/DIFF_",tile,"_",band,"_15_I.tif"))  # pixel-wise difference
diff_high <- rast(paste0("/data/ESA99/export/all/difference_rasters/DIFF_",tile,"_",band,"_20_I.tif"))  # pixel-wise difference


# stack: original + multiple difference rasters
m <- c(original, diff_low, diff_med, diff_high)
names(m) <- c("height", "low", "medium", "high")

# df <- as.data.frame(m, na.rm = TRUE)

set.seed(42)  # reproducibility
df <- spatSample(
  m,
  size   = 100000,   # number of pixels
  method = "random",
  as.df  = TRUE,
  na.rm  = TRUE
)

df_long <- tidyr::pivot_longer(
  df,
  cols = -height,
  names_to = "manipulation",
  values_to = "difference"
)

df_long$manipulation <- factor(
  df_long$manipulation,
  levels = c("low", "medium", "high")
)

# Plot
p <- ggplot(df_long, aes(height, difference)) +
  geom_density_2d(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  facet_wrap(~ manipulation) +
  labs(x = "Tree/canopy height [m]",
       y = "Difference [m]") +
  theme_minimal()



### Plots ###

# Scatter shows points
# Hexbin shows counts
# Density contours show structure

# Scatterplot cobined
ggplot(df_long, aes(height, difference, color = manipulation)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Tree canopy height",
    y = "Difference",
    color = "Manipulation level"
  ) +
  theme_minimal()

## Facetted scatterplot
ggplot(df_long, aes(height, difference)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ manipulation, ncol = 2) +
  labs(
    x = "Tree canopy height",
    y = "Difference"
  ) +
  theme_minimal()


### HEXBIN
ggplot(df_long, aes(height, difference)) +
  geom_hex(bins = 50) +
  facet_wrap(~ manipulation) +
  scale_fill_viridis_c() +
  theme_minimal()

### 2D density contours
  # Where do points concentrate in x–y space?
  # Each contour = region of equal point density
  # 
# ggplot(df_long, aes(height, difference)) +
#   geom_density_2d() +
#   facet_wrap(~ manipulation) +
#   theme_minimal()

# Publication ready
ggplot(df_long, aes(height, difference)) +
     geom_density_2d(color = "black") +
     geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
     facet_wrap(~ manipulation) +
     theme_minimal()

# ggsave(paste0("/home/emilio/canopy_height/plots/heigh_vs_diff/dens_NIR_I_10-15-25_",i,".png"),
#        plot = p,
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")



### Summarized
df_binned <- df_long |>
  dplyr::mutate(height_bin = cut(height, breaks = seq(0, max(height), by = 5))) |>
  dplyr::group_by(manipulation, height_bin) |>
  dplyr::summarise(
    median_diff = median(difference),
    .groups = "drop"
  )

ggplot(df_binned, aes(height_bin, median_diff, color = manipulation, group = manipulation)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# EX: Mulit Analysis combining tiles --------------------------------------

manip_deg <- "10_D"
band <- "B08"

tile1 <- "49NHC"
o1   <- rast(paste0("/data/ESA99/export/2026-01-29/predictions/",tile1,"_original.tif"))    # tree canopy height
d1 <- rast(paste0("/data/ESA99/export/2026-01-29/difference_rasters/DIFF_",tile1,"_",band,"_",manip_deg,".tif"))  # pixel-wise difference

s1 <- c(o1, d1)
names(s1) <- c("height", "diff")

df1 <- spatSample(s1, size = 800000, as.df = TRUE, na.rm = TRUE)


tile2 <- "32UQU"
o2   <- rast(paste0("/data/ESA99/export/2026-01-29/predictions/",tile1,"_original.tif"))    # tree canopy height
d2 <- rast(paste0("/data/ESA99/export/2026-01-29/difference_rasters/DIFF_",tile1,"_",band,"_",manip_deg,".tif"))  # pixel-wise difference

s2 <- c(o2, d2)
names(s2) <- c("height", "diff")

df2 <- spatSample(s2, size = 800000, as.df = TRUE, na.rm = TRUE)

combi <- rbind(df1,df2)

combi_long <- tidyr::pivot_longer(
  combi,
  cols = -height,
  names_to = "manipulation",
  values_to = "difference"
)

ggplot(combi_long, aes(height, difference)) +
  geom_density_2d(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  facet_wrap(~ manipulation) +
  labs(x = "Tree/canopy height [m]",
       y = "Difference [m]") +
  theme_minimal()
## DOES THIS EVEN MAKE SENSE???
