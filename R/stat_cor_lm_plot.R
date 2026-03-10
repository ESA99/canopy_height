### Statistical analysis
library(terra)
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(ggrepel)
library(hexbin)
library(viridis)
library(RColorBrewer)

### DATA ###
pixel_df <- readRDS("R/data/pixel_df.rds")

df_clean <- pixel_df %>% 
  filter(!is.na(original), original != 0)

set.seed(42)
df_sample <- df_clean %>%
  slice_sample(n = 1e6)
# df_sample <- readRDS("R/data/pixel_df_sample.rds")

### Saving of plots
# ggsave(paste0("plots/statistics/",format(Sys.Date(), "%Y-%m-%d"),"_STAT_Combi_Bin_Line_SD.png"),width = 270, height = 175, units = "mm", dpi = 300, bg = "white")


# Correlation and Model ---------------------------------------------------

# Correlation models
cor_diff <- cor(df_clean$original, df_clean$difference, use = "complete.obs")
cor_pct  <- cor(df_clean$original, df_clean$percent_diff, use = "complete.obs")
cor_diff
cor_pct

# linear models for visualization / comparison
lm_diff <- lm(difference ~ original, data = df_clean)
lm_pct  <- lm(percent_diff ~ original, data = df_clean)

summary(lm_diff)
summary(lm_pct)




# Scatterplot smooth line -------------------------------------------------

ggplot(df_sample, aes(original, difference)) +
  geom_point(alpha = 0.03, size = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "red") +
  labs(
    x = "Canopy height [m]",
    y = "Prediction difference [m]"
  ) +
  theme_bw(base_size = 14)


# Combined Bin line plot --------------------------------------------------

# Bin the original heights
df_bin <- df_sample %>%
  mutate(bin = cut(original, breaks = seq(0, 60, 2))) %>%
  group_by(bin) %>%
  summarise(
    height = mean(original, na.rm = TRUE),
    diff_mean = mean(difference, na.rm = TRUE),
    diff_sd = sd(difference, na.rm = TRUE),
    percent_mean = mean(percent_diff, na.rm = TRUE),
    percent_sd = sd(percent_diff, na.rm = TRUE),
    .groups = "drop"
  )

# Compute scaling factor for secondary axis
scale_factor <- max(df_bin$diff_mean + df_bin$diff_sd, na.rm = TRUE) /
  max(df_bin$percent_mean + df_bin$percent_sd, na.rm = TRUE)

# Plot
ggplot(df_bin, aes(height)) +
  # diff_mean ± diff_sd
  geom_ribbon(aes(ymin = diff_mean - diff_sd, ymax = diff_mean + diff_sd), 
              fill = "grey80", alpha = 0.3) +
  geom_line(aes(y = diff_mean), color = "black", linewidth = 1.2, linetype = "solid") +
  
  # percent_diff ± percent_sd (scaled)
  geom_ribbon(aes(
    ymin = (percent_mean - percent_sd) * scale_factor,
    ymax = (percent_mean + percent_sd) * scale_factor
  ), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = percent_mean * scale_factor), color = "blue", linewidth = 1.2, linetype = "dashed") +
  
  # Axes
  scale_y_continuous(
    name = "Prediction difference [m]",
    sec.axis = sec_axis(~ . / scale_factor, name = "Relative difference [%]")
  ) +
  labs(x = "Canopy height [m]") +
  theme_bw(base_size = 14) +         ## Adjust all text sizes!
  theme(
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue")
  )


# Single bin line plot ----------------------------------------------------

df_sample %>%
  mutate(bin = cut(original, breaks = seq(0, 60, 2))) %>%
  group_by(bin) %>%
  summarise(
    height = mean(original),
    diff = mean(difference)
  ) %>%
  ggplot(aes(height, diff)) +
  geom_line() +
  geom_point() +
  labs(x = "Canopy height [m]",
       y = "Difference [m]"
       # y = "Relative difference [%]"
       ) +
  theme_bw(base_size = 14)



# Histogram line plot -----------------------------------------------------

df_hist <- df_sample %>%
  mutate(bin = cut(original, breaks = seq(0, 60, 2))) %>%
  group_by(bin) %>%
  summarise(
    height = mean(original),
    diff_mean = mean(difference),
    n = n()
  )

scale_factor <- max(df_hist$diff_mean) / max(df_hist$n)

ggplot(df_hist, aes(x = height)) +
  geom_col(
    aes(y = n * scale_factor),
    fill = "grey80",
    width = 1.8
  ) +
  geom_line(
    aes(y = diff_mean),
    color = "red",
    linewidth = 1.2) +
  scale_y_continuous(
    name = "Mean prediction difference [m]",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Pixel count") ) +
  labs(
    x = "Canopy height [m]") +
  theme_bw(base_size = 14)




# Log percent plot --------------------------------------------------------


df_sample <- df_sample %>%
  mutate(percent_diff_log = log(percent_diff + 1))

ggplot(df_sample, aes(original, percent_diff_log)) +
  geom_point(alpha = 0.03, size = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "red") +
  labs(
    x = "Canopy height [m]",
    y = "log(Relative prediction difference + 1)"
  ) +
  theme_bw(base_size = 14)










# PLOT TEST SECTION -------------------------------------------------------

# Average prediction difference per bin (CH original)
# Due to the very large number of observations and spatial autocorrelation among pixels, regression results should be interpreted as descriptive relationships rather than causal inference.
df_sample %>%
  mutate(bin = cut(original, breaks = seq(0, 60, 2))) %>%
  group_by(bin) %>%
  summarise(
    height = mean(original),
    diff = mean(difference)
  ) %>%
  ggplot(aes(height, diff)) +
  geom_line() +
  geom_point() +
  labs(x = "Canopy height (m)",
       y = "Prediction difference (m)") +
  theme_bw()

# Include SD
ggplot(df_bin, aes(height, diff_mean)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_ribbon(
    aes(
      ymin = diff_mean - diff_sd,
      ymax = diff_mean + diff_sd),
    alpha = 0.2,
    fill = "grey80") +
  labs(x = "Canopy height (m)",
       y = "Prediction difference (m)") +
  theme_bw()


### Binned means
df_bin <- df_sample %>%
  mutate(bin = cut(original, breaks = seq(0, 60, 2))) %>%
  group_by(bin) %>%
  summarise(
    height = mean(original),
    diff_mean = mean(difference),
    diff_sd = sd(difference),
    diff_se = diff_sd / sqrt(n()),
    n = n()
  )

ggplot(df_sample, aes(original, difference)) +
  geom_point(alpha = 0.02, size = 0.3) +
  geom_line(
    data = df_bin,
    aes(height, diff_mean),
    color = "blue",
    linewidth = 1.2 ) +
  geom_smooth(
    method = "lm",
    color = "red",
    linewidth = 1
  ) +
  theme_bw() +
  labs(
    x = "Canopy height (m)",
    y = "Prediction difference (m)")


### Smoothed (gam) scatterplot
ggplot(df_sample, aes(original, difference)) +
  geom_point(alpha = 0.03, size = 0.4) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    color = "red",
    linewidth = 1.2) +
  labs(
    x = "Canopy height (m)",
    y = "Prediction difference (m)",
    title = "Model prediction sensitivity across canopy height") +
  theme_bw()

### Quantile Envelope
df_quant <- df_sample %>%
  mutate(bin = cut(original, breaks = seq(0, 60, 2))) %>%
  group_by(bin) %>%
  summarise(
    height = mean(original),
    q10 = quantile(difference, 0.1),
    q25 = quantile(difference, 0.25),
    q50 = quantile(difference, 0.5),
    q75 = quantile(difference, 0.75),
    q90 = quantile(difference, 0.9)
  )

ggplot(df_quant, aes(height)) +
  geom_ribbon(
    aes(ymin = q10, ymax = q90),
    fill = "grey80") +
  geom_ribbon(
    aes(ymin = q25, ymax = q75),
    fill = "grey60") +
  geom_line(aes(y = q50), color = "black", linewidth = 1.2) +
  theme_bw() +
  labs(x = "Canopy height (m)",
       y = "Prediction difference (m)")


## Statistic plot export
# ggsave(paste0("plots/statistics/",format(Sys.Date(), "%Y-%m-%d"),"_STAT_diff_Bin_Line.png"), width = 270, height = 175, units = "mm", dpi = 300, bg = "white")




