# DATA Post Processing
source("R/tools/tools.R")
source("R/deploy/info_tables.R")


# Spectral ---------------------------------------------------------------

# g20 <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-18_spectral_g20/results.csv")
# g21a <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-18_spectral_g21a/results.csv")
# g21b <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-18_spectral_g21b/results.csv")

# merged <- bind_rows(g20, g21a, g21b)
# write.csv(merged,"/home/emilio/canopy_height/results/2026-06-18_spectral_merge_ID2.csv")


spectral <- read.csv("/home/emilio/canopy_height/results/2026-06-18_spectral_merge_ID2.csv")

spectral <- spectral |>
  mutate(
location = factor(tile, levels = names(tile_label), labels = tile_label),
increment = increment*100,
abs_increment = abs(increment),
model_ID = 2
)

spectral <- add_spectral_zero(spectral, band_translation, tile_label) %>%
  rename(colour = Colour)

# write.csv(spectral,"/home/emilio/canopy_height/results/2026-06_spectral_main.csv")




# Geographical -----------------------------------------------------------

geo <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-16_geographical_1/results.csv")
# geo <- merge_backup_files("/results/runs/2026-06-04_geographical_1/loop_backups/", F)

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
# write.csv(geo,"/home/emilio/canopy_height/results/2026-06_geo_main.csv")


# Shuffle ----------------------------------------------------------------

shuffle_results <- read.csv("results/2026-05-28_shuffle_1/results.csv") |>
  mutate(
    Location = factor(tile, levels = names(tile_label), labels = tile_label)
)


# MERGE ------------------------------------------------------------------

