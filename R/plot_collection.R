### PLOT COLLECTION ###
# Setup -------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)

result_table <- read.csv("results/2025-10-20_merged_results_8_Bands.csv")

cbf_colors <- c("Blue"  = "#0077BB","Green" = "#009988", "Red"   = "#CC3311", "NIR"   = "#EE3377",
                "RedEdge" = "#EE7733", "NIR2" = "#33BBEE", "SWIR1" = "#BBBBBB", "SWIR2" = "#555555")

# ggsave(paste0("plots/",Sys.Date(),"_",length(unique(result_table$Location)),"T_B",band_names,
#               "_","8B_Facett_Location_relative",".png"),
#        width = 300, height = 175, units = "mm", dpi = 300, bg = "white")

# Mean Line butterfly -----------------------------------------------------

# Absolute Difference
ggplot(result_table, aes(x = increment, y = average_difference, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  scale_fill_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  labs(x = "Manipulation [%]", y = "Average Difference [m]",
       title = "Mean Average difference to original prdiction per Band") +
  theme_minimal(base_size = 14)

# Relative Difference
ggplot(result_table, aes(x = increment, y = avg_difference_percent, color = band, fill = band)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
  scale_color_manual(values = cbf_colors) +
  scale_fill_manual(values = cbf_colors) +
  labs(x = "Manipulation [%]", y = "Average Relative Difference [%]",
       title = "Mean Average Relative Difference to original prediction by Band") +
  theme_minimal(base_size = 14)



# SE Ribbon ---------------------------------------------------------------

# Absolute Difference
ggline(
  result_table, x = "increment", y = "average_difference",
  color = "band",  fill = "band", add = "mean_se",      # adds mean line + SE ribbon
  linewidth = 1.2,  alpha = 0.2,  palette = cbf_colors,
  position = position_dodge(width = 0.3) ) +            # offset lines and ribbons
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Difference [m]",
    color = "Band", fill = "Band",
    title = "Average Difference with SE Ribbon per Band") +
  theme_pubr(base_size = 14)

# Relative Difference
ggline(
  result_table, x = "increment", y = "avg_difference_percent",
  color = "band",  fill = "band", add = "mean_se",      # adds mean line + SE ribbon
  linewidth = 1.2,  alpha = 0.2,  palette = cbf_colors,
  position = position_dodge(width = 0.3) ) +            # offset lines and ribbons
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  labs(
    x = "Manipulation Degree [%]",
    y = "Average Relative Difference [%]",
    color = "Band", fill = "Band",
    title = "Average Relative Difference with SE Ribbon per Band") +
  theme_pubr(base_size = 14)


# Line Facett -------------------------------------------------------------

## RELATIVE Facetted ggpubr version plus points
ggline(result_table, x = "increment",  y = "avg_difference_percent",
  color = "band",  fill = "band",  add = "mean_se",
  linewidth = 1.2,  alpha = 0.2,  palette = cbf_colors,
  facet.by = "band",  scales = "fixed") +
  geom_point(aes(y = avg_difference_percent, color = "black"),
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  labs(x = "Manipulation Degree [%]",y = "Average Relative Difference [%]",
    title = "Average Relative Difference with SE Ribbon per Band") +
  theme_pubr(base_size = 14)


# Boxplot Facett ----------------------------------------------------------

# Absolute diff Facetted by band + points
ggboxplot(result_table, x = "increment",  y = "average_difference",
  fill = "band",  color = "black",
  palette = cbf_colors,  facet.by = "band",  scales = "fixed", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  rotate_x_text(angle = 45) +
  labs(x = "Manipulation [%]", y = "Average Difference [m]",
    title = "Distribution of Average Differences per Increment and Band") +
  theme_pubr(base_size = 14) +
  theme(legend.position = "none")


# Facett Location and Band ------------------------------------------

# Relative difference
ggplot(result_table, aes(x = abs(increment), y = avg_abs_diff_perc, color = band, fill = band)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  scale_fill_manual(values = cbf_colors, breaks=c('Blue', 'Green', 'Red', 'RedEdge', 'NIR', 'NIR2', 'SWIR1', 'SWIR2')) +
  facet_wrap(~Location) +
  labs(x = "Manipulation Degree [%]", y = "Mean Relative Difference [%]") +
  theme_minimal()

model <- lm(avg_abs_diff_perc ~ band * Location * abs_increment, data = result_table)
anova(model)
summary(model)





# Heatmaps ----------------------------------------------------------------

# Manipulation X Band
ggplot(result_table, aes(x = increment, y = band, fill = average_difference)) +
  geom_tile() +
  scale_fill_viridis(option = "cividis") +
  labs(x = "Manipulation [%]", y = "Band", fill = "Avg. Diff.") +
  theme_minimal(base_size = 14)

# Location X Band
ggplot(result_table %>%
         group_by(band, Location) %>%
         summarise(M_Rel_Diff = mean(avg_abs_diff_perc, na.rm = TRUE), .groups = "drop"),
       aes(x = Location, y = band, fill = M_Rel_Diff)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "cividis") +
  labs(title = "Band Sensitivity by Location",
       x = "Location", y = "Band", fill = "Mean rel diff (%)") +
  theme_minimal()
