# DATA Post Processing
source("R/tools/tools.R")
source("R/tools/info_tables.R")



# Merge ------------------------------------------------------------------

library(tidyverse)

files <- list.files("results", pattern = "\\.csv$", full.names = TRUE) |>
  stringr::str_subset("2026-07-01") |>
  stringr::str_subset("geographical")

df <- purrr::map_dfr(files, readr::read_csv)

df <- purrr::map_dfr(
  files,
  ~ readr::read_csv(.x) |>
    mutate(
      group = stringr::str_split(basename(.x), "_", simplify = TRUE)[, 3]
    )
)

# Spectral ---------------------------------------------------------------

spectral <- df
# spectral <- read.csv("/home/emilio/canopy_height/results/2026-06-18_spectral_merge_ID2.csv")

spectral <- spectral |>
  mutate(
    location = factor(tile, levels = names(tile_label), labels = tile_label),
    increment = if_else(
      decrease == "True",
      -increment,
      increment
    ) * 100,
    # increment = increment*100,
    abs_increment = abs(increment),
    model_ID = 2
)

spectral <- add_spectral_zero(spectral, band_translation, tile_label) %>%
  rename(colour = Colour) |>
  select(-X)

# write.csv(spectral,"/home/emilio/canopy_height/results/2026-06_spectral_main.csv", row.names = F)




# Geographical -----------------------------------------------------------

# geo <- merge_backup_files("/results/runs/2026-06-04_geographical_1/loop_backups/", F)
# geo <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-16_geographical_1/results.csv")
geo <- df

geo <- add_location_column(geo, order.by.mean = FALSE)

geo$shift_direction <- ifelse(
  grepl("_[NS]$", geo$out_name),
  sub(".*_([NS])$", "\\1", geo$out_name),
  NA_character_
)

geo <- geo %>% # Duplicate originals for N and S
  rowwise() %>%
    reframe(
  across(everything()),
  shift_direction = if (is.na(shift_direction)) c("N", "S") else shift_direction
)

geo <- geo %>%
  left_join(
    tile_coordinates,
    by = c("tile" = "Name")
  ) %>%
  mutate(
    lat_new = case_when(
      shift_direction == "N" ~ lat + shift_distance / 111.32,
      shift_direction == "S" ~ lat - shift_distance / 111.32,
      TRUE ~ lat
    ),
    eq_dist_km = abs(lat_new)  * 111.32,
    abs_lat = abs(lat_new)
  )


# write.csv(geo,"results/2026-06_geo_main.csv", row.names = F)

# geo_main <- read.csv("results/2026-06_geo_main_5000.csv")
# geo_full <- bind_rows(geo_main, geo)
# write.csv(geo_full,"results/2026-06_geo_main.csv", row.names = F)



# Shuffle ----------------------------------------------------------------
sg1 <- read.csv("results/2026-06-22_shuffle_g1_results.csv")
sg2 <- read.csv("results/2026-06-22_shuffle_g2_results.csv")
sg3 <- read.csv("results/2026-06-22_shuffle_g3_results.csv")
sg4 <- read.csv("results/2026-06-22_shuffle_g4_results.csv")
sg5 <- read.csv("results/2026-06-22_shuffle_g5_results.csv")
sg6 <- read.csv("results/2026-06-22_shuffle_g6_results.csv")
# sg6 <- merge_backup_files("/home/emilio/canopy_height/results/runs/2026-06-22_shuffle_g6/loop_backups/")
# write.csv(sg6,"/home/emilio/canopy_height/results/2026-06-22_shuffle_g6_results.csv")

shuffle_results <- bind_rows(sg1, sg2, sg3, sg4, sg5, sg6)

shuffle_results <- shuffle_results |>
  mutate(
  location = factor(tile, levels = names(tile_label), labels = tile_label)
) |>
  select(-X)

# write.csv(shuffle_results,"/home/emilio/canopy_height/results/2026-06_shuffle_main.csv", row.names = F)

