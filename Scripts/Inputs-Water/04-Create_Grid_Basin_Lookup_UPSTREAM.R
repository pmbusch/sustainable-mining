# ============================================================================ #
# Create Grid-Basin Lookup Table (Outlet-Based Mapping)
#
# Maps AWARE basins to WaterGAP grid cells using basin outlets from DDM30 stream network
# Consistent with AWARE 2.0 methodology: one outlet grid cell per basin
# This only needs to be run once and saves significant time in subsequent analyses
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# IMPORTANT - CODE NOT FINISHED
# IN ORDER TO FINISH IT I NEED TO DECIDE ON A METHOD TO AOVID DOUBLE COUNTING OF UPSTREAM WATER AT BASIN LEVEL
# BY SUBSTRACTING UPSTREAM BASIN USE

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(sf)
library(terra)

# Set paths ----
input_dir_watergap <- "Parameters/WaterGAP"
input_dir_aware <- "Inputs/AWARE"
output_dir <- "Parameters/WaterGAP"

# Load WaterGAP grid coordinates from NetCDF files ----
cat("\n=== LOADING WATERGAP GRID COORDINATES ===\n")
watergap_dir <- file.path(input_dir_aware, "WaterGAP", "downloaded")
nc_files <- list.files(watergap_dir, pattern = "\\.nc$", full.names = TRUE)

# Load first NetCDF to get grid definition
library(ncdf4)
nc <- nc_open(nc_files[1])
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)

# Create grid coordinates data frame
unique_coords <- expand.grid(lon = lon, lat = lat)

cat("WaterGAP grid cells:", nrow(unique_coords), "\n")
cat("Lon range:", range(unique_coords$lon), "\n")
cat("Lat range:", range(unique_coords$lat), "\n")

# Determine grid resolution
lon_diff <- median(diff(sort(unique(lon))))
lat_diff <- median(diff(sort(unique(lat))))

cat("Grid resolution: lon =", lon_diff, "°, lat =", lat_diff, "°\n")

# Create grid points as sf object
grid_points <- st_as_sf(unique_coords, coords = c("lon", "lat"), crs = 4326)

# Load AWARE basin polygons ----
cat("\n=== LOADING AWARE BASIN POLYGONS ===\n")
kmz_file <- file.path(input_dir_aware, "AWARE20_Native_CFs_geospatial.kmz")
temp_dir <- tempdir()
unzip(kmz_file, exdir = temp_dir)

kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)[1]
basins <- st_read(kml_file, quiet = TRUE)
unlink(temp_dir, recursive = TRUE)

# Extract Basin_ID
basins$Basin_ID <- as.numeric(str_remove(basins$Name, "CFs for Basin_ID "))

cat("Basins loaded:", nrow(basins), "\n") # 11661
cat("CRS:", st_crs(basins)$input, "\n")

# Load DDM30 stream network ----
# Source: https://zenodo.org/records/7256788
cat("\n=== LOADING DDM30 STREAM NETWORK ===\n")
stream_file <- file.path(input_dir_aware, "ddm30wlm_basarea", "ddm30wlm_basarea.shp")
streams <- st_read(stream_file, quiet = TRUE)

cat("Stream features loaded:", nrow(streams), "\n") # 55862
cat("CRS:", st_crs(streams)$input, "\n")

# Check for upstream area column
(upstream_col <- names(streams)[grepl("area|bas_area|uparea", names(streams), ignore.case = TRUE)]) # upstr_area

# Ensure same CRS
streams <- st_transform(streams, st_crs(basins))

# Find basin outlets using stream network ----
cat("\n=== FINDING BASIN OUTLETS ===\n")
cat("Processing", nrow(basins), "basins...\n")


# Step 1: Intersect all streams with all basins at once
# interesects only indices
sf::sf_use_s2(FALSE)
streams_in_basins <- st_intersects(basins, streams)
dim(streams_in_basins) # 11661 x 55862

# Step 2: For each basin, find outlet (stream with max upstream area)
cat("Finding outlets for basins with streams...\n")

best_line_id <- vapply(
  seq_along(streams_in_basins),
  function(i) {
    j <- streams_in_basins[[i]]
    if (length(j) == 0) {
      return(NA_integer_)
    }
    j[which.max(streams$upstr_area[j])]
  },
  integer(1)
)

sum(!is.na(best_line_id)) # 4540 found
sum(is.na(best_line_id)) # 7121 not found
# Map of missing basins - most of them near coastlines, with poor river network
miss <- which(lengths(streams_in_basins) == 0)
basins_miss <- basins[miss, ]
map1 <- map_data('world')
ggplot() +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  geom_sf(data = basins_miss, fill = "red", color = "grey30", linewidth = 0.1)


nearest_id <- st_nearest_feature(basins[miss, ], streams)

best_line_id2 <- best_line_id
best_line_id2[miss] <- nearest_id

# Average distance
nearest_geom <- st_nearest_points(basins[miss, ], streams[nearest_id, ], pairwise = TRUE)
dist_m <- as.numeric(st_length(st_transform(nearest_geom, 6933)))

# km
summary(dist_m / 1e3)

# Cut long tail of high distances of basin to stream
cut <- unname(quantile(dist_m, 0.95, na.rm = TRUE))
cut / 1e3 # 700 km


outlets_with_streams <- streams_in_basins %>%
  st_drop_geometry() %>%
  group_by(Basin_ID) %>%
  slice_max(!!sym(upstream_area_col), n = 1, with_ties = FALSE) %>%
  ungroup()

# Get coordinates for these outlets
cat("Extracting outlet coordinates...\n")
outlets_geom <- streams_in_basins %>%
  semi_join(outlets_with_streams, by = c("Basin_ID", upstream_area_col)) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()

basin_outlets_df <- outlets_with_streams %>%
  select(Basin_ID, !!sym(upstream_area_col)) %>%
  mutate(
    outlet_lon = outlets_geom$X,
    outlet_lat = outlets_geom$Y,
    upstream_area = !!sym(upstream_area_col),
    fallback = "none"
  ) %>%
  select(Basin_ID, outlet_lon, outlet_lat, upstream_area, fallback)

cat("Outlets found for", nrow(basin_outlets_df), "basins\n")

# Step 3: Handle basins with no stream intersections (fallback cases)
basins_with_outlets <- basin_outlets_df$Basin_ID
basins_without_outlets <- setdiff(basins$Basin_ID, basins_with_outlets)

if (length(basins_without_outlets) > 0) {
  cat("Handling", length(basins_without_outlets), "basins without stream intersections (fallback)...\n")

  fallback_outlets <- list()
  for (i in seq_along(basins_without_outlets)) {
    basin_id <- basins_without_outlets[i]
    basin <- basins[basins$Basin_ID == basin_id, ]

    # Use nearest stream location
    basin_centroid <- st_centroid(st_geometry(basin))
    nearest_idx <- st_nearest_feature(basin_centroid, streams)
    nearest_stream <- streams[nearest_idx, ]

    nearest_point <- st_centroid(st_geometry(nearest_stream))
    nearest_coords <- st_coordinates(nearest_point)

    fallback_outlets[[i]] <- data.frame(
      Basin_ID = basin_id,
      outlet_lon = nearest_coords[1, "X"],
      outlet_lat = nearest_coords[1, "Y"],
      upstream_area = nearest_stream[[upstream_area_col]],
      fallback = "nearest_stream"
    )
  }

  fallback_df <- bind_rows(fallback_outlets)
  basin_outlets_df <- bind_rows(basin_outlets_df, fallback_df)
}

cat("Total outlets found:", nrow(basin_outlets_df), "\n")
cat("Fallbacks used:", sum(basin_outlets_df$fallback != "none"), "\n")

# Map outlets to WaterGAP grid cells ----
cat("\n=== MAPPING OUTLETS TO WATERGAP GRID ===\n")

# Create sf object from outlets
outlets_sf <- st_as_sf(basin_outlets_df, coords = c("outlet_lon", "outlet_lat"), crs = st_crs(basins))

# Find nearest grid cell for each outlet
nearest_grid_idx <- st_nearest_feature(outlets_sf, grid_points)

# Extract grid coordinates
grid_coords <- st_coordinates(grid_points[nearest_grid_idx, ])

# Create final lookup table
grid_basin_lookup <- data.frame(
  Basin_ID = basin_outlets_df$Basin_ID,
  lon = grid_coords[, "X"],
  lat = grid_coords[, "Y"],
  outlet_lon = basin_outlets_df$outlet_lon,
  outlet_lat = basin_outlets_df$outlet_lat,
  upstream_area = basin_outlets_df$upstream_area,
  fallback_method = basin_outlets_df$fallback
)

# Calculate distance between outlet and grid cell (for diagnostics)
grid_basin_lookup$distance_km <- sqrt(
  (grid_basin_lookup$outlet_lon - grid_basin_lookup$lon)^2 + (grid_basin_lookup$outlet_lat - grid_basin_lookup$lat)^2
) *
  111 # Approximate km per degree

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

cat("\n=== OUTLET MAPPING COMPLETE ===\n")
cat("Duration:", round(duration, 2), "minutes\n")
cat("Lookup table rows:", nrow(grid_basin_lookup), "\n")
cat("Unique grid cells:", n_distinct(grid_basin_lookup$lon, grid_basin_lookup$lat), "\n")
cat("Unique basins:", n_distinct(grid_basin_lookup$Basin_ID), "\n")
cat("Mean outlet-to-grid distance:", round(mean(grid_basin_lookup$distance_km), 2), "km\n")

# Quality checks ----
cat("\n=== QUALITY CHECKS ===\n")

# Check that all basins have outlets
basins_with_outlets <- grid_basin_lookup %>% distinct(Basin_ID) %>% pull(Basin_ID)
all_basin_ids <- basins$Basin_ID
missing_basins <- setdiff(all_basin_ids, basins_with_outlets)

if (length(missing_basins) > 0) {
  cat("WARNING:", length(missing_basins), "basins missing outlets:\n")
  print(missing_basins)
} else {
  cat("✓ All basins have outlet mappings\n")
}

# Check for duplicate basins
duplicates <- grid_basin_lookup %>% group_by(Basin_ID) %>% summarise(n = n(), .groups = "drop") %>% filter(n > 1)

if (nrow(duplicates) > 0) {
  cat("WARNING:", nrow(duplicates), "basins have multiple outlets:\n")
  print(duplicates)
} else {
  cat("✓ Each basin maps to exactly one grid cell\n")
}

# Distance statistics
cat("\nOutlet-to-grid distance statistics (km):\n")
print(summary(grid_basin_lookup$distance_km))

# Fallback usage
fallback_summary <- table(grid_basin_lookup$fallback_method)
cat("\nFallback method usage:\n")
print(fallback_summary)

# Grid cell reuse
grid_reuse <- grid_basin_lookup %>%
  group_by(lon, lat) %>%
  summarise(n_basins = n(), .groups = "drop") %>%
  filter(n_basins > 1)

cat("\nGrid cells used by multiple basins:", nrow(grid_reuse), "\n")
if (nrow(grid_reuse) > 0) {
  cat("Max basins per grid cell:", max(grid_reuse$n_basins), "\n")
}

# Save lookup table ----
cat("\n=== SAVING LOOKUP TABLE ===\n")
csv_file <- file.path(output_dir, "grid_basin_lookup.csv")
write_csv(grid_basin_lookup, csv_file)

cat("Lookup table saved to:", csv_file, "\n")
cat("File size:", format(file.size(csv_file) / 1024, digits = 2), "KB\n")

cat("\n=== DONE ===\n")
cat("Outlet-based basin-to-grid mapping complete.\n")
cat("Each basin now maps to exactly one WaterGAP grid cell at its outlet.\n")
cat("This lookup table can be used in script 06b for basin aggregation.\n")

# EoF
