# Translation tables ------------------------------------------------------
tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", "35VML" = "Finland", 
  "49NHC" = "Malaysia", "49UCP" = "Mongolia","34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West"
)

band_translation <- data.frame(
  BandName = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12"), # "B10", cirrus not included
  BandNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), # B8A = 9, B09 = 10
  Colour = c("Aerosol", "Blue", "Green", "Red", "RedEdge","None","None", "NIR", "NIR2", "WaterVapour", "SWIR1", "SWIR2")
)

tile_coordinates <- data.frame(
  Name = c("10TES","17SNB","20MMD","33NTG","32TMT","32UQU","34UFD","35VML","49NHC","55HEV","49UCP"),
  lon = c(-122.285282,-80.379497,-63.405779,12.786326,8.402194,12.430884,23.294500,26.091998,114.189928,147.613916,109.045451),
  lat = c(46.455086,37.449419,-1.400945,5.831657,47.355772,48.205131,52.730829,63.527175,2.213706,-36.636058,48.239885)
)

shift_limits <- data.frame(
  tile = c("10TES", "17SNB", "20MMD", "33NTG", "32TMT",
    "32UQU", "34UFD", "35VML", "49NHC", "55HEV", "49UCP"),
  max_km_N = c(4700, 5700, 10100, 9300, 4600,
               4500, 4000, 2800, 9700, 14000, 4500),
  max_km_S = c(15100, 14100, 9800, 10600, 15200,
               15300, 15800, 17000, 10200, 5800, 15300),
  stringsAsFactors = FALSE
)
