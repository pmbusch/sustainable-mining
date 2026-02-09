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
library(ncdf4)

# Set paths ----
input_dir_watergap <- "Inputs/AWARE/WaterGap"
input_dir_aware <- "Inputs/AWARE"
output_dir <- "Parameters/WaterGAP"

# This file contains the area of each grid cell (needed for unit conversion)
continental_file <- file.path(input_dir_watergap, "watergap22e_gswp3-w5e5_continentalarea_histsoc_static.nc")
nc_area <- nc_open(continental_file)
# Extract area data (in km2 to m2)
area_data <- ncvar_get(nc_area, "continentalarea") * 1e6
lon <- ncvar_get(nc_area, "lon")
lat <- ncvar_get(nc_area, "lat")
nc_close(nc_area)

# Create spatial reference for the grid
unique_coords <- expand.grid(lon = lon, lat = lat) %>% mutate(area_m2 = as.vector(area_data))

cat("Grid cells:", nrow(unique_coords), "\n")
cat("Lon range:", range(unique_coords$lon), "\n")
cat("Lat range:", range(unique_coords$lat), "\n")

# Determine grid resolution from coordinate spacing (half-width for polygon creation)
grid_resolution <- median(diff(sort(unique(lon)))) / 2
cat("Grid resolution:", grid_resolution * 2, "degrees (half-width:", grid_resolution, ")\n")


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

# Check individual overlap fractions
cat("Overlap fraction per row:\n")
print(summary(grid_basin_lookup$overlap_fraction))

# Check sum of overlap fractions per grid cell (should be <= 1)
grid_alloc <- grid_basin_lookup %>%
  group_by(lon, lat) %>%
  summarise(
    total_fraction = sum(overlap_fraction),
    n_basins = n(),
    .groups = "drop"
  )

cat("\nTotal allocation per grid cell (sum of overlap fractions):\n")
print(summary(grid_alloc$total_fraction))

# Flag grid cells with allocation > 1 (tolerance for projection artifacts)
tol <- 1e-3
over_allocated <- grid_alloc %>% filter(total_fraction > 1 + tol)
if (nrow(over_allocated) > 0) {
  cat("WARNING:", nrow(over_allocated), "grid cells have total allocation > 1:\n")
  print(over_allocated %>% arrange(desc(total_fraction)) %>% head(10))
  # Cap at 1 by rescaling proportionally
  cat("Rescaling over-allocated grid cells to sum to 1...\n")
  grid_basin_lookup <- grid_basin_lookup %>%
    left_join(grid_alloc %>% select(lon, lat, total_fraction), by = c("lon", "lat")) %>%
    mutate(overlap_fraction = if_else(total_fraction > 1 + tol,
                                       overlap_fraction / total_fraction,
                                       overlap_fraction)) %>%
    select(-total_fraction)
} else {
  cat("OK: All grid cells have total allocation <= 1\n")
}

# Report under-allocated cells (fraction < 1, e.g. coastal cells partially over ocean)
under_allocated <- grid_alloc %>% filter(total_fraction < 1 - tol)
cat("Grid cells with partial allocation (< 1):", nrow(under_allocated),
    "out of", nrow(grid_alloc), "\n")

# Multi-basin cells
multi_basin <- grid_alloc %>% filter(n_basins > 1)
cat("Grid cells mapped to multiple basins:", nrow(multi_basin), "\n")
if (nrow(multi_basin) > 0) {
  cat("Max basins per grid cell:", max(multi_basin$n_basins), "\n")
}

# Check that all basins have grid connections
basins_with_grids <- grid_basin_lookup %>% distinct(Basin_ID) %>% pull(Basin_ID)
all_basin_ids <- basins$Basin_ID
missing_basins <- setdiff(all_basin_ids, basins_with_grids)
if (length(missing_basins) > 0) {
  cat("WARNING:", length(missing_basins), "basins have no grid cell overlap\n")
} else {
  cat("OK: All basins have at least one grid cell\n")
}

# Save lookup table ----
cat("\n=== SAVING LOOKUP TABLE ===\n")
csv_file <- file.path(output_dir, "grid_basin_lookup.csv")
write_csv(grid_basin_lookup, csv_file)

# EoF
