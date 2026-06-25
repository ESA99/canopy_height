# g20 <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-18_spectral_g20/results.csv")
# g21a <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-18_spectral_g21a/results.csv")
# g21b <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-18_spectral_g21b/results.csv")

# merged <- bind_rows(g20, g21a, g21b)
# write.csv(merged,"/home/emilio/canopy_height/results/2026-06-18_spectral_merge_ID2.csv")


source("R/deploy/info_tables.R")
source("R/tools/tools.R")
source("R/plot/plot_functions.R")

spectral <- read.csv("/home/emilio/canopy_height/results/2026-06-18_spectral_merge_ID2.csv")

spectral <- spectral |>
  mutate(
Location = factor(tile, levels = names(tile_label), labels = tile_label),
increment = increment*100,
abs_increment = abs(increment)
)

spectral <- add_spectral_zero(spectral, band_translation, tile_label)

plot_spectral_labels(spectral, y_var = "mean_change", y_lab = "Average Difference [m]")

plot_spectral_facets(spectral, y_var = "mean_change", y_lab = "Average Difference [m]", 5, 3)














add_zero_step_rows <- function(df, band_translation, tile_label) {

  library(dplyr)

  # Missing tile-band combinations at increment 0
  missing <- df %>%
    distinct(tile, band) %>%
    anti_join(
      df %>%
        filter(increment == 0) %>%
        distinct(tile, band),
      by = c("tile", "band")
    )

  if (nrow(missing) == 0) {
    return(df)
  }

  # Get one representative row per tile
  tile_info <- df %>%
    group_by(tile) %>%
    slice(1) %>%
    ungroup() %>%
    select(-band, -Colour)

  # Build missing baseline rows
  zero_rows <- missing %>%
    left_join(tile_info, by = "tile") %>%
    left_join(
      band_translation %>%
        select(BandName, Colour),
      by = c("band" = "BandName")
    ) %>%
    mutate(
      mode = "spectral",
      increment = 0,
      abs_increment = 0,
      decrease = "False",
      original = TRUE,
      correlation = 1,
      Location = unname(tile_label[tile]),

      mean_change = 0,
      mean_abs_change = 0,
      relative_mean_change = 0,
      relative_mean_abs_change = 0,
      mae = 0,
      rmse = 0,
      sd_change = 0,
      relative_sd_change = 0
    )

  bind_rows(df, zero_rows)
}
# spectral <- add_zero_step_rows(spectral, band_translation)


add_spectral_zero <- function(df, band_translation, tile_label) {

  message("Checking for missing baseline (increment = 0) rows...")

  missing <- df %>%
    distinct(tile, band) %>%
    anti_join(
      df %>%
        filter(increment == 0) %>%
        distinct(tile, band),
      by = c("tile", "band")
    )

  if (nrow(missing) == 0) {
    message("✓ No missing tile-band baseline rows found.")
    return(df)
  }

  message(
    sprintf(
      "Found %s missing tile-band combinations.",
      nrow(missing)
    )
  )

  print(missing)

  tile_info <- df %>%
    group_by(tile) %>%
    slice(1) %>%
    ungroup() %>%
    select(-band, -Colour)

  zero_rows <- missing %>%
    left_join(tile_info, by = "tile") %>%
    left_join(
      band_translation %>%
        select(BandName, Colour),
      by = c("band" = "BandName")
    ) %>%
    mutate(
      mode = "spectral",
      increment = 0,
      abs_increment = 0,
      decrease = "False",
      original = TRUE,
      correlation = 1,
      Location = unname(tile_label[tile]),

      mean_change = 0,
      mean_abs_change = 0,
      relative_mean_change = 0,
      relative_mean_abs_change = 0,
      mae = 0,
      rmse = 0,
      sd_change = 0,
      relative_sd_change = 0
    )

  out <- bind_rows(df, zero_rows)

  message(
    sprintf(
      "✓ Added %s baseline rows.",
      nrow(zero_rows)
    )
  )

  # verification
  remaining_missing <- out %>%
    distinct(tile, band) %>%
    anti_join(
      out %>%
        filter(increment == 0) %>%
        distinct(tile, band),
      by = c("tile", "band")
    )

  if (nrow(remaining_missing) == 0) {
    message(
      sprintf(
        "✓ Verification passed: %s tile-band combinations now have an increment=0 row.",
        nrow(out %>% distinct(tile, band))
      )
    )
  } else {
    warning(
      sprintf(
        "Verification failed: %s combinations are still missing.",
        nrow(remaining_missing)
      )
    )
    print(remaining_missing)
  }

  out
}
