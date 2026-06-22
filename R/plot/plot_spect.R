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

add_zero_step_rows <- function(df, band_translation) {

  # 1. identify missing (tile, band) pairs for increment == 0
  missing <- df %>%
    distinct(tile, band) %>%
    anti_join(
      df %>% filter(increment == 0) %>% distinct(tile, band),
      by = c("tile", "band")
    )

  if (nrow(missing) == 0) return(df)

  # 2. build zero rows by copying structure
  zero_rows <- missing %>%
    mutate(increment = 0) %>%
    left_join(df %>% filter(increment == 0) %>% select(-increment),
              by = c("tile", "band"))

  # 3. IMPORTANT: fix Colour from band translation
  zero_rows <- zero_rows %>%
    left_join(band_translation, by = c("band" = "BandName")) %>%
    mutate(Colour = Colour.y) %>%
    select(-Colour.x, -Colour.y, BandNumber)

  # 4. ensure numeric columns are 0 where missing
  zero_rows <- zero_rows %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))

  bind_rows(df, zero_rows)
}  
spectral <- add_zero_step_rows(spectral, band_translation)

### NOT YET DONE -> Location column etc... ###

plot_spectral_labels(spectral, y_var = "mean_change", y_lab = "Average Difference [m]")
