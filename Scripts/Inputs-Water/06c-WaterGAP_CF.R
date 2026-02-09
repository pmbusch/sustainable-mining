# ============================================================================ #
# Calculate Water Scarcity Characterization Factors
#
# Loads basin-level aggregated data from script 06b
# Joins with AWARE reference data (ActAvail, EWR)
# Calculates Available Minus Demand (AMD) and Characterization Factors (CF)
# Based on AWARE methodology
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(readxl)
library(lubridate)

# Set paths ----
input_dir_watergap <- "Parameters/WaterGAP"
input_dir_aware <- "Inputs/AWARE"
output_dir <- "Parameters/WaterGAP"

# Load basin-level intermediate data ----
cat("\n=== LOADING BASIN-LEVEL DATA ===\n")
basin_file <- file.path(input_dir_watergap, "basin_data_intermediate.csv")
basin_data <- read_csv(basin_file, show_col_types = FALSE)

cat("Basin data loaded:\n")
cat("  Rows:", nrow(basin_data), "\n")
cat("  Scenarios:", paste(unique(basin_data$scenario), collapse = ", "), "\n")
cat("  Variables:", paste(unique(basin_data$variable), collapse = ", "), "\n")
cat("  Year range:", range(basin_data$year), "\n")
cat("  Unique basins:", n_distinct(basin_data$Basin_ID), "\n")

# Pivot to wide format ----
basin_data_wide <- basin_data %>%
  pivot_wider(names_from = variable, values_from = value_m3_month, names_prefix = "var_")


# Load AWARE reference data ----
cat("\n=== LOADING AWARE REFERENCE DATA ===\n")
aware_file <- file.path(input_dir_aware, "AWARE20_Intermediate_Variables.xlsx")

# Load ActAvail (Actual Availability) 1990-2019
actavail_data <- read_excel(aware_file, sheet = "ActAvail_1990_2019")
cat("ActAvail data loaded:", nrow(actavail_data), "basins\n")

# Load EWR (Environmental Water Requirements)
ewr_data <- read_excel(aware_file, sheet = "EWR")
cat("EWR data loaded:", nrow(ewr_data), "basins\n")

area_data <- read_excel(aware_file, sheet = "basin_area")

# Prepare AWARE reference data ----
cat("\n=== PREPARING AWARE REFERENCE DATA ===\n")

# Convert to long format
actavail_monthly <- actavail_data %>%
  pivot_longer(cols = -Basin_ID, names_to = "month_name", values_to = "actavail_m3_month") %>%
  mutate(
    month = match(tolower(month_name), tolower(month.abb)),
    month = if_else(is.na(month), match(tolower(month_name), tolower(month.name)), month),
    month = if_else(is.na(month), as.integer(str_extract(month_name, "\\d+")), month)
  ) %>%
  filter(!is.na(month)) %>%
  select(Basin_ID, month, actavail_m3_month)

ewr_monthly <- ewr_data %>%
  pivot_longer(cols = -Basin_ID, names_to = "month_name", values_to = "ewr_m3_month_base") %>%
  mutate(
    month = match(tolower(month_name), tolower(month.abb)),
    month = if_else(is.na(month), match(tolower(month_name), tolower(month.name)), month),
    month = if_else(is.na(month), as.integer(str_extract(month_name, "\\d+")), month)
  ) %>%
  filter(!is.na(month)) %>%
  select(Basin_ID, month, ewr_m3_month_base)


# Join AWARE data with basin data ----
cat("\n=== JOINING AWARE DATA ===\n")

basin_cf_data <- basin_data_wide %>%
  left_join(area_data) |>
  left_join(actavail_monthly, by = c("Basin_ID", "month")) %>%
  left_join(ewr_monthly, by = c("Basin_ID", "month")) |>
  filter(!is.na(var_dis) & !is.na(actavail_m3_month) & !is.na(ewr_m3_month_base))
head(basin_cf_data)

# Check totals, pick one sceario
basin_cf_data |>
  filter(scenario == "ssp126") |>
  filter(year == 2025) |>
  summarise(
    total_dis = sum(var_dis, na.rm = TRUE),
    total_actavail = sum(actavail_m3_month, na.rm = TRUE),
    total_ewr_base = sum(ewr_m3_month_base, na.rm = TRUE),
    total_use = sum(var_atotuse, na.rm = TRUE)
  )
# From aware 2.0
# actavail_total = 1.73E14 vs dis (the scenarios have much more discharge...)
# EWR total = 5.82E13
# HWC: 1.51E12 (use)

# Scale EWR by discharge ratio ----
basin_cf_data <- basin_cf_data %>%
  mutate(
    # Calculate scale factor as actavail / dis
    scale_factor = if_else(var_dis > 0, var_dis / actavail_m3_month, 1),
    # Limit scale factor to [0.1, 10]
    scale_factor = pmax(0.1, pmin(scale_factor, 10)),
    # Scaled EWR for projected period
    ewr_m3_month = ewr_m3_month_base * scale_factor,
  )
head(basin_cf_data)

cat("Scale factor range:", range(basin_cf_data$scale_factor, na.rm = TRUE), "\n")
cat("Scaled EWR range:", range(basin_cf_data$ewr_m3_month, na.rm = TRUE), "\n")

# Discharge is much larger in the scenarios, need to scale them to AWARE data by 2025
scale_dis <- basin_cf_data %>%
  filter(year == 2025) %>%
  mutate(
    scale_dis = if_else(actavail_m3_month > 0, var_dis / actavail_m3_month, 1),
    # Limit scale factor to [0.1, 10]
    scale_dis = pmax(0.1, pmin(scale_dis, 10))
  ) |>
  dplyr::select(scenario, Basin_ID, month, scale_dis)
range(scale_dis$scale_dis, na.rm = TRUE)

# Scale things
basin_cf_data <- basin_cf_data %>%
  left_join(scale_dis, by = c("scenario", "Basin_ID", "month")) %>%
  mutate(var_dis = var_dis / scale_dis) %>%
  select(-scale_dis)


# Calculate AMD (Available Minus Demand) ----
cat("\n=== CALCULATING AMD ===\n")

basin_cf_data <- basin_cf_data %>%
  mutate(
    # AMD = (discharge - EWR - water use) / area
    # Convert to depth (m/month)
    amd_numerator = pmax(0, var_dis - ewr_m3_month - var_atotuse),
    amd_m_month = if_else(area > 0, amd_numerator / area, 0)
  )

cat("AMD range:", range(basin_cf_data$amd_m_month, na.rm = TRUE), "\n")
cat("AMD mean:", mean(basin_cf_data$amd_m_month, na.rm = TRUE), "\n") # in AWARE was 0.03266
cat("AMD median:", median(basin_cf_data$amd_m_month, na.rm = TRUE), "\n")

# Aggregate AMD to year
basin_cf_data <- basin_cf_data |>
  group_by(Basin_ID, scenario, year) %>%
  summarise(hwc = sum(var_atotuse, na.rm = TRUE), amd = sum(amd_m_month, na.rm = TRUE), .groups = "drop")

# Calculate world weighted average AMD ----
cat("\n=== CALCULATING WORLD AMD ===\n")

world_amd <- basin_cf_data %>%
  group_by(year, scenario) %>%
  summarise(
    total_weighted_amd = sum(amd * hwc, na.rm = TRUE),
    total_atotuse = sum(hwc, na.rm = TRUE),
    amd_world = if_else(total_atotuse > 0, total_weighted_amd / total_atotuse, 0),
    .groups = "drop"
  ) %>%
  select(year, scenario, amd_world)

cat("World AMD range:", range(world_amd$amd_world, na.rm = TRUE), "\n")
cat("World AMD mean:", mean(world_amd$amd_world, na.rm = TRUE), "\n")
# AWARE 2.0 world avg was 0.02410

# Calculate Characterization Factors ----
cat("\n=== CALCULATING CHARACTERIZATION FACTORS ===\n")

basin_cf_data <- basin_cf_data %>%
  left_join(world_amd, by = c("year", "scenario")) %>%
  mutate(
    # CF = AMD World / AMD
    cf_raw = if_else(amd > 0, amd_world / amd, 100),

    # Replace NaN and Inf
    cf_raw = if_else(is.finite(cf_raw), cf_raw, 100),

    # Limit CF to range [0.1, 100]
    cf = pmax(0.1, pmin(100, cf_raw))
  )

cat("CF raw range:", range(basin_cf_data$cf_raw, na.rm = TRUE), "\n")
cat("CF range (limited):", range(basin_cf_data$cf, na.rm = TRUE), "\n")
cat("CF mean:", mean(basin_cf_data$cf, na.rm = TRUE), "\n")
cat("CF median:", median(basin_cf_data$cf, na.rm = TRUE), "\n")
cat("CFs at lower limit (0.1):", sum(basin_cf_data$cf == 0.1, na.rm = TRUE) / nrow(basin_cf_data), "\n")
cat("CFs at upper limit (100):", sum(basin_cf_data$cf == 100, na.rm = TRUE) / nrow(basin_cf_data), "\n")

# Quality checks ----
cat("\n=== QUALITY CHECKS ===\n")

# CF summary by scenario
cf_by_scenario <- basin_cf_data %>%
  group_by(scenario) %>%
  summarise(
    n_rows = n(),
    cf_min = min(cf, na.rm = TRUE),
    cf_max = max(cf, na.rm = TRUE),
    cf_mean = mean(cf, na.rm = TRUE),
    cf_median = median(cf, na.rm = TRUE),
    na_count = sum(is.na(cf)),
    .groups = "drop"
  )

cat("CF summary by scenario:\n")
print(cf_by_scenario)

# Save final CF data ----
cat("\n=== SAVING CHARACTERIZATION FACTORS ===\n")

# Select relevant columns for output
output_file <- file.path(output_dir, "basin_cf_data.csv")
write_csv(basin_cf_data, output_file)

# EoF
