args <- commandArgs(trailingOnly = TRUE)

manipulation <- args[1]
tile_group <- args[2]

tile_groups <- list(
  g1 = c("10TES"),
  g2 = c("32TMT"),
  g3 = c("32UQU"),
  g4 = c("17SNB", "49NHC", "55HEV"),
  g5 = c("20MMD", "35VML", "49UCP"),
  g6 = c("33NTG", "34UFD"),
  g20  = c("32TMT", "33NTG", "35VML", "49NHC"), 
  g21a = c("10TES", "17SNB", "20MMD", "49UCP"),
  g21b = c("32UQU", "55HEV", "34UFD")
)

tiles <- tile_groups[[tile_group]]
base_specs$tile <- tiles
base_specs$manipulation <- manipulation

