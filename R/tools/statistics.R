library(dplyr)

#### Geographic ####

# LM of absolute latitude by sample tile
tile_slopes <- data %>%
  group_by(location) %>%
  group_modify(~ {
    fit <- lm(reformulate("abs_lat", response = "mean_change"),
              data = .x)

    data.frame(
      slope = coef(fit)["abs_lat"],
      intercept = coef(fit)["(Intercept)"],
      r_squared = summary(fit)$r.squared,
      p_value = summary(fit)$coefficients["abs_lat", "Pr(>|t|)"],
      n = nobs(fit)
    )
  }) %>%
  ungroup()

print(tile_slopes)

write.csv(
  tile_slopes,
  file = paste0("tile_slopes_mean_change.csv"),
  row.names = FALSE
)

## Slope only
tile_slopes <- data %>%
  group_by(location) %>%
  group_modify(~ {
    fit <- lm(reformulate("abs_lat", response = "mean_change"),
              data = .x)

    data.frame(
      slope_m_deg = unname(coef(fit)["abs_lat"]),
      slope_m_1000km = unname(coef(fit)["abs_lat"]) * (1000 / 111.32),
      r_squared = summary(fit)$r.squared,
      n = nobs(fit)
    )
  }) %>%
  ungroup()

print(tile_slopes)



# Global LM: all tiles and locations combined
fit_global <- lm(
  reformulate("abs_lat", response = "mean_change"),
  data = data
)

global_stats <- data.frame(
  tile = "All combined",
  slope_m_deg = unname(coef(fit_global)["abs_lat"]),
  slope_m_1000km = unname(coef(fit_global)["abs_lat"]) * (1000 / 111.32),
  intercept = unname(coef(fit_global)["(Intercept)"]),
  r_squared = summary(fit_global)$r.squared,
  p_value = summary(fit_global)$coefficients["abs_lat", "Pr(>|t|)"],
  n = nobs(fit_global)
)

print(global_stats)
# -1.362 m/1000km shift away from equator
