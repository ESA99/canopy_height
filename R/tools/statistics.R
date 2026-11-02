library(dplyr)
library(broom)

#### Spectral ####

# Lm per band across all tiles
lm_results <- spectral_results %>%
  filter(!is.na(abs_increment), !is.na(mean_change), !is.na(colour)) %>%
  group_by(colour) %>%
  group_modify(~ {
    mod <- lm(mean_change ~ abs_increment, data = .x)

    tidy(mod, conf.int = TRUE) %>%
      filter(term == "abs_increment") %>%
      transmute(
        slope = estimate,
        SE = std.error,
        CI_lower = conf.low,
        CI_upper = conf.high,
        p_value = p.value
      )
  }) %>%
  ungroup() %>%
  mutate(
    slope = round(slope, 3),
    SE = round(SE, 3),
    CI_lower = round(CI_lower, 3),
    CI_upper = round(CI_upper, 3),
    p_value = signif(p_value, 3)
  )

lm_results



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



#### Spatial ####

shuffle_dat <- shuffle_results %>%
  filter(
    patch_size == 2,
    !is.na(shuffle_percentage),
    !is.na(mean_change)
  )

# Individual models per tile
shuffle_lm_location <- shuffle_dat %>%
  group_by(location) %>%
  group_modify(~ {

    mod <- lm(mean_change ~ shuffle_percentage, data = .x)

    tidy(mod, conf.int = TRUE) %>%
      filter(term == "shuffle_percentage") %>%
      transmute(
        slope = estimate,
        SE = std.error,
        CI_lower = conf.low,
        CI_upper = conf.high,
        p_value = p.value
      )
  }) %>%
  ungroup()

# Combined model
mod_shuffle_all <- lm(
  mean_change ~ shuffle_percentage,
  data = shuffle_dat
)

shuffle_lm_all <- tidy(
  mod_shuffle_all,
  conf.int = TRUE
) %>%
  filter(term == "shuffle_percentage") %>%
  transmute(
    location = "All combined",
    slope = estimate,
    SE = std.error,
    CI_lower = conf.low,
    CI_upper = conf.high,
    p_value = p.value
  )

# Results
shuffle_lm_results <- bind_rows(
  shuffle_lm_location,
  shuffle_lm_all
) %>%
  mutate(
    across(
      c(slope, SE, CI_lower, CI_upper),
      ~ round(.x, 3)
    ),
    p_value = signif(p_value, 3)
  )

shuffle_lm_results


mod_shuffle_location <- lm(
  mean_change ~ shuffle_percentage * location,
  data = shuffle_dat
)
