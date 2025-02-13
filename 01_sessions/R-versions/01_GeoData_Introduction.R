# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# Use pacman to load and install libraries
pacman::p_load(
  here, 
  sf,
  dplyr,
  ggplot2,
  tmap,
  tictoc,
  renv
)

# Initialize renv virtual environment
if (!file.exists("renv.lock")) {
  renv::init() # Initialize the environment and create renv.lock if not already present
} else {
  renv::restore() # Restore the environment from renv.lock if it exists
}

# Working directory
here::here("geodata_tutorials", "01_sessions", "R-versions")

# Start timing the process
tic()

# Create a list of points and a simple feature collection (equivalent to GeoDataFrame)
points <- st_sfc(st_point(c(10, 20)), st_point(c(15, 24)), st_point(c(21, 14)))
gdf <- st_sf(name = c("A", "B", "C"), geometry = points)

# Print the GeoDataFrame
print(gdf)

# Plot the points
plot(gdf$geometry, main = "Points Plot")

# Create a LineString connecting points and add it to a GeoDataFrame
line <- st_sfc(st_linestring(matrix(c(10, 20, 15, 24, 21, 14), ncol = 2, byrow = TRUE)))
gdf_line <- st_sf(name = "A", geometry = line)

# Print the GeoDataFrame with LineString
print(gdf_line)

# Plot the LineString
plot(gdf_line$geometry, main = "LineString Plot")

# Read shapefiles for Ecuador at different political levels
ecuador_4 <- st_read(here("02_data", "shapefiles", "nivel-politico-4.shp"))
ecuador_3 <- st_read(here("02_data", "shapefiles", "nivel-politico-3.shp"))
ecuador_2 <- st_read(here("02_data", "shapefiles", "nivel-politico-2.shp"))
ecuador_1 <- st_read(here("02_data", "shapefiles", "nivel-politico-1.shp"))

#Check if valid polygons and geometries 
tmap_options(check.and.fix = TRUE)
ecuador_objects <- list(ecuador_1, ecuador_2, ecuador_3, ecuador_4)

# Loop through each object in the list
for (i in 1:length(ecuador_objects)) {
  # Check for valid geometries
  valid_geometries <- sf::st_is_valid(ecuador_objects[[i]])
  
  # Print validity status for each object
  print(paste("Object ecuador_", i, " valid: ", all(valid_geometries), sep = ""))
  
  # If any geometries are invalid, make them valid
  if (any(!valid_geometries)) {
    ecuador_objects[[i]] <- sf::st_make_valid(ecuador_objects[[i]])
    print(paste("Fixed invalid geometries for ecuador_", i, sep = ""))
  }
}

# ####
# The message st_as_s2(): dropping Z and/or M coordinate typically appears when 
# working with geometries that have 3D (Z) or 4D (M) coordinates, which are being 
# dropped during certain spatial operations or transformations. 
# In this case, it suggests that your ecuador_1 object contains 3D or 4D geometries, 
# but the operation you're using (such as st_is_valid() or another transformation) 
# is ignoring the Z/M dimensions.
# ####

# check data and class
summary(ecuador_4)
class(ecuador_4)
class(ecuador_4$geometry)

# Plot each level
plot(ecuador_1$geometry, main = "Ecuador Level 1")
plot(ecuador_2$geometry, main = "Ecuador Level 2")
plot(ecuador_3$geometry, main = "Ecuador Level 3")
plot(ecuador_4$geometry, main = "Ecuador Level 4")

# Plot with boundaries
tm_shape(ecuador_1) + tm_borders()
tm_shape(ecuador_2) + tm_borders()
tm_shape(ecuador_3) + tm_borders()
tm_shape(ecuador_4) + tm_borders()

# Advanced plotting with color maps
tm_shape(ecuador_1) + tm_polygons(col = "geometry", palette = "tab20")
tm_shape(ecuador_2) + tm_polygons(col = "NAME_2", palette = "tab20")
tm_shape(ecuador_3) + tm_polygons(col = "NAME_3", palette = "tab20b_r")
tm_shape(ecuador_4) + tm_polygons(col = "NAME_4", palette = "Blues_r")

# Read population shapefile
population <- st_read('../02_data/shapefiles/pop_shape7.shp')
head(population, 3)

# Compute population change
population <- population %>%
  mutate(pop_change = (`2020` - `2001`) / `2001` * 100)

# Plot population change
tm_shape(population) +
  tm_polygons(col = "pop_change", palette = "Purples", title = "Population Change") +
  tm_layout(main.title = "Population change from 2001 to 2020")

# End timing and print elapsed time
toc()

# Take a snapshot of the environment at the end
renv::snapshot(prompt = FALSE)
