latlong_variables <- function (tiles = c("10TES", "33NTG", "55HEV"), 
                               new_pos = NA,
                               year = "2020",
                               base_folder = "/home/emilio/canopy_height", 
                               worldcover = "2020",
                               result_dir = "/data/ESA99/lat_lon_results/") 
{
  df <- expand.grid(tile_name = tiles, 
                    new_pos = new_pos, 
                    year = year, 
                    rootDIR = base_folder,
                    result_dir = result_dir,
                    WC_year = worldcover, 
                    original = FALSE, 
                    stringsAsFactors = FALSE)
  base_folder <- normalizePath(base_folder)
  df$tile_name <- trimws(df$tile_name)
  tile_folder <- file.path(base_folder, "deploy_example", 
                           "sentinel2", year)
  df$tile_folder <- file.path(tile_folder, df$tile_name)
  
  # Split "lon lat" into separate numeric columns
  coords <- do.call(rbind, strsplit(df$new_pos, " "))
  df$Longitude <- as.numeric(coords[, 1])
  df$Latitude  <- as.numeric(coords[, 2])
  
  df$out_name <- paste0(df$tile_name, "_Lat:", df$Latitude, "_Lon:", df$Longitude)
  
  for (t in unique(tiles)) {
    extra_row <- data.frame(
      tile_name   = t,
      new_pos     = NA,
      year        = year[1],
      rootDIR     = base_folder,
      result_dir = result_dir,
      WC_year     = worldcover,
      original    = TRUE,
      tile_folder = file.path(tile_folder, t),
      Longitude   = NA,
      Latitude    = NA,
      out_name    = paste0(t, "_original"),
      stringsAsFactors = FALSE
    )
    df <- rbind(extra_row, df)
    
  }
  df$out_dir <- file.path(base_folder, "final_results")
  return(df)
}






split_values <- strsplit(env_var, " ")[[1]][1]

var1 <- split_values[1]
var2 <- split_values[2]



latlong_variables(tiles = c(tiles = "10TES", "33NTG", "55HEV",
                            new_pos = new_pos_char,
                            year = "2020",
                            base_folder = "/home/emilio/canopy_height"))