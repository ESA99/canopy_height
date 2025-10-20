library(ggplot2)
library(dplyr)
library(tidyr)
library(fmsb)
library(reshape2)
library(scales)

# Effects of Manipulation degree ------------------------------------------

ggplot(result_table, aes(x = abs(increment), y = avg_abs_diff_perc, color = band, fill = band)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  facet_wrap(~Location) +
  labs(title = "Effect of Increment Size on Prediction Change per Band",
       x = "Absolute Increment", y = "Absolute Difference (%)") +
  theme_minimal()

model <- lm(avg_abs_diff_perc ~ band * Location * abs_increment, data = result_table)
anova(model)
summary(model)




# Band sensitivity
band_sensitivity <- result_table %>%
  group_by(band) %>%
  summarise(
    sensitivity = mean(avg_abs_diff_perc, na.rm = TRUE) /
      sd(avg_abs_diff_perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(sensitivity))
band_sensitivity # High values = large and consistent effects (higher relevance)



# other -------------------------------------------------------------------

# Grouped bar chart
ggplot(result_table, aes(x = band, y = avg_abs_diff_perc, fill = Location)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.8)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "Average Absolute Difference (%) by Band and Location",
       x = "Spectral Band", y = "Average Absolute Difference (%)") +
  theme_minimal() +
  theme(text = element_text(size = 12))

#Violin plot
ggplot(result_table, aes(x = band, y = avg_abs_diff_perc, fill = Location)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.3) +
  labs(title = "Distribution of Average Absolute Difference (%) per Band and Location",
       x = "Spectral Band", y = "Average Absolute Difference (%)") +
  theme_minimal()

