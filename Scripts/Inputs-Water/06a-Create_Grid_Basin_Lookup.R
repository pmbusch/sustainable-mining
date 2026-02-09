# ============================================================================ #
# Create Grid-Basin Lookup Table
#
# Pre-computes spatial intersection between WaterGAP grid cells and AWARE basins
# Creates a reusable lookup table mapping grid cells to basins with overlap areas
# This only needs to be run once and saves significant time in subsequent analyses
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(sf)
library(terra)

# Set paths ----
input_dir_watergap <- "Parameters/WaterGAP"
input_dir_aware <- "Inputs/AWARE"
output_dir <- "Parameters/WaterGAP"

# Load a sample ensemble file to get grid coordinates ----
# We only need lon, lat, and area_m2 - any intermediate file will work
cat("\n=== LOADING GRID COORDINATES ===\n")
intermediate_dir <- file.path(input_dir_watergap, "intermediate")
sample_files <- list.files(intermediate_dir, pattern = "^ensemble_.*\\.rds$", full.names = TRUE)

if (length(sample_files) == 0) {
  stop("No ensemble files found. Run script 05 first to generate intermediate files.")
}

# Load first file to get grid structure
sample_data <- readRDS(sample_files[1])
unique_coords <- sample_data %>% distinct(lon, lat, area_m2)

cat("Grid cells:", nrow(unique_coords), "\n")
cat("Lon range:", range(unique_coords$lon), "\n")
cat("Lat range:", range(unique_coords$lat), "\n")

# Determine grid resolution ----
lon_diff <- sample_data %>%
  arrange(lat, lon) %>%
  group_by(lat) %>%
  summarise(min_diff = min(diff(sort(unique(lon)))), .groups = "drop") %>%
  pull(min_diff) %>%
  median(na.rm = TRUE)

grid_resolution <- lon_diff / 2

cat("Grid resolution:", lon_diff, "degrees\n")

rm(sample_data)
gc()

# Load basin polygons ----
cat("\n=== LOADING BASIN POLYGONS ===\n")
kmz_file <- file.path(input_dir_aware, "AWARE20_Native_CFs_geospatial.kmz")
temp_dir <- tempdir()
unzip(kmz_file, exdir = temp_dir)

kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)[1]
basins <- st_read(kml_file, quiet = TRUE)
unlink(temp_dir, recursive = TRUE)

# Extract Basin_ID
basins$Basin_ID <- as.numeric(str_remove(basins$Name, "CFs for Basin_ID "))

cat("Basins loaded:", nrow(basins), "\n")

# Create grid cell polygons ----
cat("\n=== CREATING GRID CELL POLYGONS ===\n")
cat("This may take a few minutes for", nrow(unique_coords), "cells...\n")

grid_cells_list <- lapply(1:nrow(unique_coords), function(i) {
  if (i %% 1000 == 0) {
    cat("  Progress:", i, "/", nrow(unique_coords), "\n")
  }

  lon <- unique_coords$lon[i]
  lat <- unique_coords$lat[i]

  coords <- matrix(
    c(
      lon - grid_resolution,
      lat - grid_resolution,
      lon + grid_resolution,
      lat - grid_resolution,
      lon + grid_resolution,
      lat + grid_resolution,
      lon - grid_resolution,
      lat + grid_resolution,
      lon - grid_resolution,
      lat - grid_resolution
    ),
    ncol = 2,
    byrow = TRUE
  )

  st_polygon(list(coords))
})

grid_cells_sf <- st_sf(
  lon = unique_coords$lon,
  lat = unique_coords$lat,
  area_m2 = unique_coords$area_m2,
  geometry = st_sfc(grid_cells_list, crs = 4326)
)

cat("Grid cells created:", nrow(grid_cells_sf), "\n")

rm(grid_cells_list)
gc()

# Transform to equal-area projection ----
cat("\n=== TRANSFORMING TO EQUAL-AREA PROJECTION ===\n")
basins_aea <- st_transform(basins, crs = 6933) # World Cylindrical Equal Area
grid_cells_aea <- st_transform(grid_cells_sf, crs = 6933)

# Calculate actual grid cell areas in equal-area projection ----
cat("\n=== CALCULATING GRID CELL AREAS ===\n")
grid_cells_aea$grid_area_m2 <- as.numeric(st_area(grid_cells_aea))

cat("Grid area statistics (m²):\n")
print(summary(grid_cells_aea$grid_area_m2))
cat("Grid area statistics (km²):\n")
print(summary(grid_cells_aea$grid_area_m2 / 1e6))

# Spatial intersection ----
cat("\n=== COMPUTING SPATIAL INTERSECTION ===\n")
cat("This is the slow part - may take 10-30 minutes...\n")
cat("Start time:", format(Sys.time()), "\n")

start_time <- Sys.time()

# Do intersection in chunks to monitor progress and manage memory
chunk_size <- 5000
n_chunks <- ceiling(nrow(grid_cells_aea) / chunk_size)

overlap_chunks <- list()

for (chunk in 1:n_chunks) {
  start_idx <- (chunk - 1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, nrow(grid_cells_aea))

  cat("  Processing chunk", chunk, "/", n_chunks, "(cells", start_idx, "to", end_idx, ")\n")

  grid_chunk <- grid_cells_aea[start_idx:end_idx, ]

  overlap_chunk <- st_intersection(grid_chunk, basins_aea) %>%
    mutate(overlap_area_m2 = as.numeric(st_area(geometry)), overlap_fraction = overlap_area_m2 / grid_area_m2) %>%
    st_drop_geometry() %>%
    select(lon, lat, Basin_ID, area_m2, grid_area_m2, overlap_area_m2, overlap_fraction) %>%
    filter(overlap_area_m2 > 0) # Remove dummy connections (no overlap)

  overlap_chunks[[chunk]] <- overlap_chunk

  rm(grid_chunk, overlap_chunk)
  gc()
}

# Combine all chunks
grid_basin_lookup <- bind_rows(overlap_chunks)

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

cat("\n=== INTERSECTION COMPLETE ===\n")
cat("Duration:", round(duration, 2), "minutes\n")
cat("Lookup table rows:", nrow(grid_basin_lookup), "\n")
cat("Unique grid cells:", n_distinct(grid_basin_lookup$lon, grid_basin_lookup$lat), "\n")
cat("Unique basins:", n_distinct(grid_basin_lookup$Basin_ID), "\n")

# Quality checks ----
cat("\n=== QUALITY CHECKS ===\n")

# Check overlap fractions
cat("Overlap fraction range:", range(grid_basin_lookup$overlap_fraction), "\n")
cat("Mean overlap fraction:", mean(grid_basin_lookup$overlap_fraction), "\n")
cat("Overlap fraction summary:\n")
print(summary(grid_basin_lookup$overlap_fraction))


# Check that all basins have grid connections
basins_with_grids <- grid_basin_lookup %>% distinct(Basin_ID) %>% pull(Basin_ID)

all_basin_ids <- basins$Basin_ID
missing_basins <- setdiff(all_basin_ids, basins_with_grids) # onle one, but it is not relevant

# Save lookup table ----
cat("\n=== SAVING LOOKUP TABLE ===\n")
csv_file <- file.path(output_dir, "grid_basin_lookup.csv")
write_csv(grid_basin_lookup, csv_file)

# EoF
