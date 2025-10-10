library(sf)
library(rnaturalearth)
library(dplyr)
library(tmap)


# All land points ---------------------------------------------------------

point_selcetion <- function(lon_grid = 20, lat_grid = 15, lon_border = c(-180,180), lat_border = c(-60,60), coast_buffer = -0.05){
  
  # Load land polygons
  land <- ne_countries(scale = "medium", returnclass = "sf")
  land <- st_transform(land, crs = 4326)
  
  land_proj <- st_transform(land, crs = "+proj=robin +datum=WGS84")
  land_buffered_proj <- st_buffer(land_proj, dist = coast_buffer * 111000) # approx meters
  land_buffered <- st_transform(land_buffered_proj, crs = 4326)
  
  # Shrink land slightly to avoid coastlines
  # land_buffered <- st_buffer(land, dist = coast_buffer)
  # land_buffered <- land_buffered[!st_is_empty(land_buffered), ]
  
  # Define grid (systematic longitudinal lines)
  n_lines <- lon_grid
  n_lat_points <- lat_grid
  # lons <- round(seq(-120, 180, length.out = n_lines), 0)
  # lats <- round(seq(-60, 50, length.out = n_lat_points), 0)
  
  lons <- round(seq(lon_border[1], lon_border[2], length.out = n_lines), digits = 0)
  lats <- round(seq(lat_border[1], lat_border[2], length.out = n_lat_points), digits = 0)
  
  all_points <- list()
  
  # Generate points along each longitude and keep only land points
  for (lon in lons) {
    line_points <- data.frame(lon = rep(lon, n_lat_points),
                              lat = lats)
    points_sf <- st_as_sf(line_points, coords = c("lon", "lat"), crs = 4326)
    
    # Keep only points on buffered land
    intersections <- st_intersects(points_sf, land_buffered)
    land_points <- points_sf[lengths(intersections) > 0, ]
    
    all_points[[as.character(lon)]] <- land_points
  }
  
  # Combine all land points
  systematic_points <- do.call(rbind, all_points)
  
  # Extract coordinates if needed
  new_positions <- st_coordinates(systematic_points)
  
  # Assuming new_pos is a matrix with columns X (lon) and Y (lat)
  new_pos_char <- apply(new_positions, 1, function(row) paste(row[1], row[2]))
  
  return(new_pos_char)
  
}


new_pos_char <- point_selcetion(lon_grid = 17, lat_grid = 12, lon_border = c(-120,180), lat_border = c(-60,50), coast_buffer = -0.05)



points_sf <- function(new_pos_char, crs = 4326) {
  # Convert "lon lat" character vector into sf points
  points_df <- do.call(rbind, strsplit(new_pos_char, " ")) |>
    as.data.frame() |>
    setNames(c("lon", "lat"))
  
  points_df$lon <- as.numeric(points_df$lon)
  points_df$lat <- as.numeric(points_df$lat)
  
  st_as_sf(points_df, coords = c("lon", "lat"), crs = crs)
}

points <- points_sf(new_pos_char)

# tm_basemap("OpenStreetMap")+
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(points) +
  tm_borders(col = "red", lwd = 5)

st_write(points, "/home/emilio/canopy_height/R/latlong/points.gpkg")

tm_shape(points) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(land_buffered) +
  tm_basemap("Esri.WorldGrayCanvas")


new_pos <- st_coordinates(points)
df <- as.data.frame(new_pos)
names(df) <- c("Lon", "Lat")
nrow(df)
unique(df$Lon)
unique(df$Lat)



# Random points onn land ---------------------------------------------------------------


# Load land polygons
land <- ne_countries(scale = "medium", returnclass = "sf")

# Make sure CRS matches (both in WGS84)
land <- st_transform(land, crs = 4326)

# Generate a finer grid of lat/lon points
lats <- seq(-60, 60, length.out = 100)   # finer grid
lons <- seq(-180, 180, length.out = 100)
grid <- expand.grid(lon = lons, lat = lats)

# Convert grid to sf points
points <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

# Check intersections: st_intersects returns a sparse matrix
intersections <- st_intersects(points, land)

# Keep only points that intersect land
land_points <- points[lengths(intersections) > 0, ]

# Sample 50 points
set.seed(3)
sampled_points <- land_points %>% slice_sample(n = 50)

# Extract coordinates
new_pos <- st_coordinates(sampled_points)
head(new_pos)


# tm_basemap("OpenStreetMap")+
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(sampled_points) +
  tm_borders(col = "red", lwd = 5)






# Systemetical 50 ---------------------------------------------------


# Load land polygons
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs = 4326)

# Shrink land slightly to avoid coastlines
land_buffered <- st_buffer(land, dist = -0.2)
land_buffered <- land_buffered[!st_is_empty(land_buffered), ]

# Define many longitudinal lines (more E-W coverage)
n_lines <- 26  # more lines
n_lat_points <- 12  # fewer points along each line

lons <- round(seq(-180, 180, length.out = n_lines), digits = 0)
lats <- round(seq(-60, 50, length.out = n_lat_points), digits = 0)

all_points <- list()

# Generate points along each longitude
for (lon in lons) {
  line_points <- data.frame(lon = rep(lon, n_lat_points),
                            lat = lats)
  points_sf <- st_as_sf(line_points, coords = c("lon", "lat"), crs = 4326)
  
  # Keep only points on buffered land
  intersections <- st_intersects(points_sf, land_buffered)
  land_points <- points_sf[lengths(intersections) > 0, ]
  
  all_points[[as.character(lon)]] <- land_points
}

# Combine all points
systematic_points <- do.call(rbind, all_points)

# If we have more than 50, thin points evenly across longitudes
total_points <- nrow(systematic_points)
if(total_points > 60){
  # Select points uniformly across the rows to keep structure
  indices <- round(seq(1, total_points, length.out = 50))
  systematic_points <- systematic_points[indices, ]
}

# Extract coordinates
new_pos <- st_coordinates(systematic_points)
new_pos


# tm_basemap("OpenStreetMap")+
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(systematic_points) +
  tm_borders(col = "red", lwd = 5)



# Assuming new_pos is a matrix with columns X (lon) and Y (lat)
new_pos_char <- apply(new_pos, 1, function(row) paste(row[1], row[2]))
head(new_pos_char)


df <- as.data.frame(new_pos)
names(df) <- c("Lon", "Lat")
unique(df$Lon)
unique(df$Lat)






