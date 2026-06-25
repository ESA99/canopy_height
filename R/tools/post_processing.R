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
  rename(colour = Colour) |>
  select(-X)

# write.csv(spectral,"/home/emilio/canopy_height/results/2026-06_spectral_main.csv", row.names = F)




# Geographical -----------------------------------------------------------

# geo <- merge_backup_files("/results/runs/2026-06-04_geographical_1/loop_backups/", F)
geo <- read.csv("/home/emilio/canopy_height/results/runs/2026-06-16_geographical_1/results.csv")

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

# write.csv(geo,"/home/emilio/canopy_height/results/2026-06_geo_main.csv", row.names = F)


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

