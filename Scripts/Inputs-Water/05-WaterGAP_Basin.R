# ============================================================================ #
# Aggregate WaterGAP Grid Data to Basin Level
#
# Loads raw WaterGAP .nc files for qtot and atotuse, filters 2025-2050,
# converts units, and aggregates to basin level using the grid-basin lookup.
# Loops over each scenario x climate_model combination and saves basin-level
# files for subsequent CF calculations.
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(ncdf4)
library(terra)
library(lubridate)
library(CFtime)
library(glue)

# Set paths ----
input_dir <- "Inputs/AWARE/WaterGAP"
download_dir <- file.path(input_dir, "downloaded")
output_dir <- "Parameters/WaterGAP"

# Load pre-computed grid-basin lookup table ----
cat("\n=== LOADING GRID-BASIN LOOKUP TABLE ===\n")
lookup_file <- file.path(output_dir, "grid_basin_lookup.csv")
grid_basin_lookup <- read_csv(lookup_file, show_col_types = FALSE) |>
  dplyr::select(lon, lat, Basin_ID, overlap_fraction)

cat("Lookup table loaded:", nrow(grid_basin_lookup), "rows\n")
cat("Unique grid cells:", n_distinct(grid_basin_lookup$lon, grid_basin_lookup$lat), "\n")
cat("Unique basins:", n_distinct(grid_basin_lookup$Basin_ID), "\n")

# Load continental area file (needed for unit conversion) ----
continental_file <- file.path(input_dir, "watergap22e_gswp3-w5e5_continentalarea_histsoc_static.nc")
nc_area <- nc_open(continental_file)
area_data <- ncvar_get(nc_area, "continentalarea") * 1e6 # km2 to m2
lon_grid <- ncvar_get(nc_area, "lon")
lat_grid <- ncvar_get(nc_area, "lat")
nc_close(nc_area)

area_grid <- expand.grid(lon = lon_grid, lat = lat_grid) %>% mutate(area_m2 = as.vector(area_data))

# Function to load a single .nc file, filter time, and convert to long format ----
load_watergap_file <- function(file_path, var_name, start_year = 2025, end_year = 2050) {
  nc <- nc_open(file_path)

  time <- ncvar_get(nc, "time")
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")

  # Convert time to dates
  time_units <- ncatt_get(nc, "time", "units")$value
  cal <- ncatt_get(nc, "time", "calendar")$value %||% "standard"
  ct <- CFtime(time_units, cal, time)
  dates <- as.Date(as_timestamp(ct))
  # print(range(dates)) # good ranges up, to 2100

  years <- year(dates)
  months <- month(dates)

  # Filter for desired time period
  time_idx <- which(years >= start_year & years <= end_year)

  if (length(time_idx) == 0) {
    nc_close(nc)
    return(NULL)
  }

  # Extract variable data for filtered time period
  var_data <- ncvar_get(nc, var_name, start = c(1, 1, min(time_idx)), count = c(-1, -1, length(time_idx)))
  nc_close(nc)

  # Convert to long format
  filtered_years <- years[time_idx]
  filtered_months <- months[time_idx]
  filtered_dates <- dates[time_idx]

  data_long <- expand.grid(lon = lon, lat = lat, time_idx = 1:length(time_idx)) %>%
    mutate(
      value = as.vector(var_data),
      year = filtered_years[time_idx],
      month = filtered_months[time_idx],
      date = filtered_dates[time_idx]
    ) |>
    filter(!is.na(value))

  return(data_long)
}

# Build file list for qtot and atotuse ----
cat("\n=== BUILDING FILE LIST ===\n")
nc_files <- list.files(download_dir, pattern = "\\.nc$", full.names = TRUE)

# Parse file names: watergap2-2e_{climate_model}_w5e5_{scenario}_2015soc_default_{variable}_global_monthly_*.nc
file_info <- tibble(file_path = nc_files) %>%
  mutate(
    filename = basename(file_path),
    climate_model = str_extract(filename, "(?<=watergap2-2e_)[^_]+"),
    scenario = str_extract(filename, "(?<=w5e5_)[^_]+"),
    variable = str_extract(filename, "(?<=default_)[^_]+")
  ) %>%
  # Keep only qtot and atotuse (monthly files)
  filter(variable %in% c("qtot", "atotuse", "dis"))

cat("Files to process:", nrow(file_info), "\n")
cat("Scenarios:", paste(unique(file_info$scenario), collapse = ", "), "\n")
cat("Climate models:", paste(unique(file_info$climate_model), collapse = ", "), "\n")
cat("Variables:", paste(unique(file_info$variable), collapse = ", "), "\n")

# Process each file: load, convert units, aggregate to basin level ----
cat("\n=== PROCESSING FILES ===\n")

# Convert lookup to data.table for faster joins
grid_basin_lookup_dt <- as.data.table(grid_basin_lookup)
setkey(grid_basin_lookup_dt, lon, lat)

# Convert area_grid to data.table
area_grid_dt <- as.data.table(area_grid)
setkey(area_grid_dt, lon, lat)

intermediate_dir <- file.path(output_dir, "intermediate")
if (!dir.exists(intermediate_dir)) {
  dir.create(intermediate_dir, recursive = TRUE)
}

for (i in 1:nrow(file_info)) {
  current_scenario <- file_info$scenario[i]
  current_variable <- file_info$variable[i]
  current_climate_model <- file_info$climate_model[i]
  current_file <- file_info$file_path[i]

  cat(
    "\n--- [",
    i,
    "/",
    nrow(file_info),
    "]",
    current_scenario,
    "-",
    current_variable,
    "-",
    current_climate_model,
    "---\n"
  )

  # Load and filter to 2025-2050
  grid_data <- load_watergap_file(current_file, current_variable, 2025, 2050)

  if (is.null(grid_data)) {
    cat("  SKIPPED: no data in 2025-2050 range\n")
    next
  }

  cat("  Grid rows loaded:", nrow(grid_data) / 1e6, "M\n")

  # Convert to data.table for speed
  grid_dt <- as.data.table(grid_data)
  rm(grid_data)
  setkey(grid_dt, lon, lat)

  # Join with area data for unit conversion (kg/m2/s -> m3/month)
  grid_dt <- area_grid_dt[grid_dt, on = .(lon, lat)]
  grid_dt[, seconds_in_month := days_in_month(date) * 86400]
  grid_dt[,
    value_m3_month := if (current_variable == "dis") {
      value * seconds_in_month
    } else {
      (value * area_m2 / 1000) * seconds_in_month
    }
  ]
  grid_dt[, c("value", "area_m2", "date", "time_idx", "seconds_in_month") := NULL]

  # Join with basin lookup and aggregate (weighted sum by overlap fraction)
  basin_dt <- grid_basin_lookup_dt[grid_dt, on = .(lon, lat), nomatch = NULL, allow.cartesian = TRUE]
  basin_dt[, weighted_value := value_m3_month * overlap_fraction]
  basin_dt <- basin_dt[, .(value_m3_month = sum(weighted_value, na.rm = TRUE)), by = .(Basin_ID, year, month)]

  cat("  Basin rows:", nrow(basin_dt), "\n")
  cat("  Unique basins:", uniqueN(basin_dt$Basin_ID), "\n")
  cat("  Value range:", range(basin_dt$value_m3_month, na.rm = TRUE), "\n")

  # Add metadata and save directly
  basin_dt[, `:=`(scenario = current_scenario, variable = current_variable, climate_model = current_climate_model)]
  out_file <- file.path(intermediate_dir, glue("{current_scenario}_{current_variable}_{current_climate_model}.csv"))
  fwrite(basin_dt, out_file)
  cat("  Saved:", basename(out_file), "\n")

  rm(grid_dt, basin_dt)
  gc()
}

# EoF
