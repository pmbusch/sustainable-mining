# ============================================================================ #
# WaterGAP Data Processing Script
#
# Loads downloaded WaterGAP monthly data, filters for 2025-2050,
# converts units using continental area data, and saves individual files
# for each scenario-variable-climate_model combination
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
library(data.table)

# Set paths ----
input_dir <- "Inputs/AWARE/WaterGAP"
download_dir <- file.path(input_dir, "downloaded")
output_dir <- "Parameters/WaterGAP" # save file

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load file metadata ----
file_metadata <- readRDS(file.path(input_dir, "file_metadata.rds"))

# Filter for monthly resolution files
monthly_files <- file_metadata %>%
  filter(resolution == "monthly") %>%
  mutate(file_path = file.path(download_dir, filename))

# Load continental area file ----
# This file contains the area of each grid cell (needed for unit conversion)
continental_file <- file.path(input_dir, "watergap22e_gswp3-w5e5_continentalarea_histsoc_static.nc")
nc_area <- nc_open(continental_file)

# Extract area data (in km2 to m2)
area_data <- ncvar_get(nc_area, "continentalarea") * 1e6
lon <- ncvar_get(nc_area, "lon")
lat <- ncvar_get(nc_area, "lat")
nc_close(nc_area)

# Create spatial reference for the grid
area_grid <- expand.grid(lon = lon, lat = lat) %>% mutate(area_m2 = as.vector(area_data))

# DEBUG: Check area grid ----
cat("\n=== AREA GRID CHECK ===\n")
cat("Dimensions:", nrow(area_grid), "grid cells\n")
cat("Lon range:", range(area_grid$lon), "\n")
cat("Lat range:", range(area_grid$lat), "\n")
cat("Area range (km2):", range(area_grid$area_m2 / 1e6, na.rm = TRUE), "\n")
cat("NAs in area:", sum(is.na(area_grid$area_m2)), "\n")
print(head(area_grid))

# Function to load and process a single WaterGAP file ----
load_watergap_file <- function(file_path, var_name, start_year = 2025, end_year = 2050) {
  if (!file.exists(file_path)) {
    warning(glue("File not found: {basename(file_path)}"))
    return(NULL)
  }

  nc <- nc_open(file_path)

  # Extract time, lon, lat
  time <- ncvar_get(nc, "time")
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")

  # Get time units and convert to dates
  time_units <- ncatt_get(nc, "time", "units")$value
  cal <- ncatt_get(nc, "time", "calendar")$value %||% "standard"

  ct <- CFtime(time_units, cal, time)
  dates <- as.Date(as_timestamp(ct))
  print(range(dates)) # good ranges up, to 2100

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

# Load all monthly files ----
# Filter files (avoid qtot for now)
monthly_files <- monthly_files %>% filter(variable != "qtot")

cat("\n=== CHUNKED PROCESSING ===\n")
cat("Total monthly files:", nrow(monthly_files), "\n")
cat("Files by variable:\n")
print(table(monthly_files$variable))

# Create output directory for intermediate files
intermediate_dir <- file.path(output_dir, "intermediate")
if (!dir.exists(intermediate_dir)) {
  dir.create(intermediate_dir, recursive = TRUE)
}

# Get unique scenario-variable-climate_model combinations
scenario_var_model_combos <- monthly_files %>% distinct(scenario, variable, climate_model)

cat("\nProcessing", nrow(scenario_var_model_combos), "scenario-variable-climate_model combinations\n")

# Process each scenario-variable-climate_model combination separately
# Slow to run...
for (i in 1:nrow(scenario_var_model_combos)) {
  current_scenario <- scenario_var_model_combos$scenario[i]
  current_variable <- scenario_var_model_combos$variable[i]
  current_climate_model <- scenario_var_model_combos$climate_model[i]

  cat(
    "\n--- Processing:",
    current_scenario,
    "-",
    current_variable,
    "-",
    current_climate_model,
    "(",
    i,
    "/",
    nrow(scenario_var_model_combos),
    ") ---\n"
  )

  # Filter files for this combination
  files_to_process <- monthly_files %>%
    filter(scenario == current_scenario, variable == current_variable, climate_model == current_climate_model)

  cat("Loading file...\n")

  # Load data for this scenario-variable-climate_model combination
  combo_data <- files_to_process %>%
    rowwise() %>%
    mutate(data = list(load_watergap_file(file_path, variable, start_year = 2025, end_year = 2050))) %>%
    ungroup() %>%
    filter(!map_lgl(data, is.null)) %>%
    select(climate_model, scenario, variable, data) %>%
    unnest(data)

  cat("Loaded M rows:", nrow(combo_data) / 1e6, "\n")

  # Join with area data
  combo_data <- combo_data %>% left_join(area_grid, by = c("lon", "lat"))

  cat("Missing area values:", sum(is.na(combo_data$area_m2)), "\n")

  # Convert units to m3 per month
  combo_data <- combo_data %>%
    mutate(
      days_in_month = days_in_month(date),
      seconds_in_month = days_in_month * 24 * 60 * 60,
      value_m3_month = case_when(
        variable %in% c("qtot", "atotuse") ~ (value * area_m2 / 1000) * seconds_in_month,
        variable == "dis" ~ value * seconds_in_month,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-value, -days_in_month, -seconds_in_month, -date, -time_idx)

  cat("Unit conversion complete\n")

  # Save individual file (no ensembling)
  filename <- glue("{current_scenario}_{current_variable}_{current_climate_model}.rds")
  saveRDS(combo_data, file.path(intermediate_dir, filename))
  cat("Saved:", filename, "\n")

  # Clear memory
  rm(combo_data, files_to_process)
  gc()
}

# EoF
