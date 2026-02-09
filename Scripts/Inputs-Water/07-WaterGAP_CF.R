# ============================================================================ #
# Calculate Water Scarcity Characterization Factors
#
# Loads basin-level intermediate files (qtot, atotuse) from script 06.
# Loops over scenario x climate_model, joining both variables per iteration.
# Joins with AWARE reference data (EWR),
# calculates monthly CFs, then aggregates to yearly:
#   - CF: arithmetic mean of monthly CFs
#   - availability & demand: sum of monthly values
#
# Based on AWARE 2.0 methodology
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
intermediate_dir <- file.path(input_dir_watergap, "intermediate")

# Load AWARE reference data ----
cat("\n=== LOADING AWARE REFERENCE DATA ===\n")
aware_file <- file.path(input_dir_aware, "AWARE20_Intermediate_Variables.xlsx")

ewr_data <- read_excel(aware_file, sheet = "EWR")
area_data <- read_excel(aware_file, sheet = "basin_area")

cat("EWR basins:", nrow(ewr_data), "\n")


ewr_monthly <- ewr_data %>%
  pivot_longer(cols = -Basin_ID, names_to = "month_name", values_to = "ewr_m3_month_base") %>%
  mutate(
    month = match(tolower(month_name), tolower(month.abb)),
    month = if_else(is.na(month), match(tolower(month_name), tolower(month.name)), month),
    month = if_else(is.na(month), as.integer(str_extract(month_name, "\\d+")), month)
  ) %>%
  filter(!is.na(month)) %>%
  select(Basin_ID, month, ewr_m3_month_base)

# Identify scenario x climate_model combinations from intermediate files ----
cat("\n=== IDENTIFYING COMBINATIONS ===\n")
all_files <- list.files(intermediate_dir, pattern = "\\.csv$")

combos <- tibble(filename = all_files) %>%
  mutate(
    scenario = str_extract(filename, "^[^_]+"),
    variable = str_extract(filename, "(?<=_)[^_]+(?=_)"),
    climate_model = str_remove(str_extract(filename, "[^_]+\\.csv$"), "\\.csv$")
  ) %>%
  distinct(scenario, climate_model)

cat("Combinations to process:", nrow(combos), "\n")
print(combos)

# Process each scenario x climate_model ----
cat("\n=== PROCESSING COMBINATIONS ===\n")

yearly_list <- list()

for (j in 1:nrow(combos)) {
  sc <- combos$scenario[j]
  cm <- combos$climate_model[j]
  cat("\n--- [", j, "/", nrow(combos), "]", sc, "-", cm, "---\n")

  # Load both qtot and atotuse for this combo
  qtot_file <- file.path(intermediate_dir, paste0(sc, "_qtot_", cm, ".csv"))
  atotuse_file <- file.path(intermediate_dir, paste0(sc, "_atotuse_", cm, ".csv"))

  qtot_dt <- fread(qtot_file)[, .(Basin_ID, year, month, qtot = value_m3_month)]
  atotuse_dt <- fread(atotuse_file)[, .(Basin_ID, year, month, atotuse = value_m3_month)]

  # Merge qtot and atotuse
  basin_dt <- merge(qtot_dt, atotuse_dt, by = c("Basin_ID", "year", "month"), all = TRUE)
  rm(qtot_dt, atotuse_dt)

  cat("  Basin-month rows:", nrow(basin_dt), "\n")

  # Join with AWARE reference data
  basin_df <- as_tibble(basin_dt) %>%
    left_join(area_data, by = "Basin_ID") %>%
    left_join(ewr_monthly, by = c("Basin_ID", "month")) %>%
    filter(!is.na(qtot), !is.na(ewr_m3_month_base), !is.na(area)) %>%
    mutate(atotuse = tidyr::replace_na(atotuse, 0))

  rm(basin_dt)

  cat("  After AWARE join:", nrow(basin_df), "rows\n")

  # EWR is simply constant
  basin_df <- basin_df %>% mutate(ewr_m3_month = ewr_m3_month_base)

  # Calculate monthly AMD (m/month) = max(0, qtot - EWR - demand) / area
  basin_df <- basin_df %>%
    mutate(
      deficit = (qtot - ewr_m3_month - atotuse) <= 0 | area <= 0,
      amd_m_month = if_else(deficit, 0, (qtot - ewr_m3_month - atotuse) / area)
    )

  # DEBUG
  # Check totals, pick one sceario
  # basin_df |>
  #   filter(year == 2025) |>
  #   summarise(
  #     total_qtot = sum(qtot, na.rm = TRUE),
  #     total_ewr_base = sum(ewr_m3_month_base, na.rm = TRUE),
  #     total_use = sum(atotuse, na.rm = TRUE)
  #   )
  # Aware 2.0 vs this data:
  # actavail_total = 1.73E14 vs 4.96E13 (this data)
  # EWR total = 5.82E13 (same as this data)
  # HWC: 1.51E12 (use) vs 1.48E12 (this data)

  # Calculate world weighted average AMD per year-month
  world_amd <- basin_df %>%
    group_by(year, month) %>%
    summarise(
      total_weighted_amd = sum(amd_m_month * atotuse, na.rm = TRUE),
      total_atotuse = sum(atotuse, na.rm = TRUE),
      amd_world = if_else(total_atotuse > 0, total_weighted_amd / total_atotuse, 0),
      .groups = "drop"
    ) %>%
    select(year, month, amd_world)
  world_amd |> filter(year == 2025) |> pull(amd_world) |> sum() # check world AMD in 2025, should be around 0.2 m/month

  # Calculate monthly CFs
  basin_cf_monthly <- basin_df %>%
    left_join(world_amd, by = c("year", "month")) %>%
    mutate(
      cf_raw = if_else(deficit, 100, amd_world / amd_m_month),
      cf_raw = if_else(is.finite(cf_raw), cf_raw, 100),
      cf = pmax(0.1, pmin(100, cf_raw))
    )

  cat("  Monthly CF range:", range(basin_cf_monthly$cf, na.rm = TRUE), "\n")

  # Aggregate to yearly: CF = mean, availability & demand = sum
  basin_yearly <- basin_cf_monthly %>%
    group_by(Basin_ID, year) %>%
    summarise(
      cf = mean(cf, na.rm = TRUE),
      availability_m3_yr = sum(qtot, na.rm = TRUE),
      demand_m3_yr = sum(atotuse, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = sc, climate_model = cm)

  cat("  Yearly rows:", nrow(basin_yearly), "\n")
  cat("  Yearly CF range:", range(basin_yearly$cf, na.rm = TRUE), "\n")

  yearly_list[[j]] <- basin_yearly

  rm(basin_df, basin_cf_monthly, world_amd, basin_yearly)
  gc()
}

# Combine all results ----
cat("\n=== COMBINING ALL RESULTS ===\n")
basin_cf_data <- bind_rows(yearly_list)

cat("Total rows:", nrow(basin_cf_data) / 1e6, "M\n")
cat("Scenarios:", paste(unique(basin_cf_data$scenario), collapse = ", "), "\n")
cat("Climate models:", paste(unique(basin_cf_data$climate_model), collapse = ", "), "\n")
cat("Year range:", range(basin_cf_data$year), "\n")
cat("Unique basins:", n_distinct(basin_cf_data$Basin_ID), "\n")

# Quality checks ----
cat("\n=== QUALITY CHECKS ===\n")

cf_by_scenario <- basin_cf_data %>%
  group_by(scenario) %>%
  summarise(
    n_basins = n_distinct(Basin_ID),
    cf_min = min(cf, na.rm = TRUE),
    cf_mean = mean(cf, na.rm = TRUE),
    cf_median = median(cf, na.rm = TRUE),
    cf_max = max(cf, na.rm = TRUE),
    .groups = "drop"
  )

cat("CF summary by scenario:\n")
print(cf_by_scenario)

# Save final CF data ----
cat("\n=== SAVING CHARACTERIZATION FACTORS ===\n")
output_file <- file.path(output_dir, "basin_cf_data.csv")
write_csv(basin_cf_data, output_file)
cat("Saved:", output_file, "\n")

# Smooth over 5 year period --------------------

make_period <- function(y) {
  ifelse(y < 2025 | y > 2050, NA_integer_, 2025 + 5 * floor((y - 2025) / 5))
}

basin_cf_5yr <- basin_cf_data %>%
  mutate(period_start = make_period(year)) %>%
  filter(!is.na(period_start)) %>%
  group_by(scenario, climate_model, Basin_ID, period_start) %>%
  summarise(
    cf = mean(cf, na.rm = TRUE),
    availability_m3_yr = mean(availability_m3_yr, na.rm = TRUE),
    demand_m3_yr = mean(demand_m3_yr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period_label = paste0(period_start, "–", period_start + 5))

output_file <- file.path(output_dir, "basin_cf_data_5yr.csv")
write_csv(basin_cf_5yr, output_file)
cat("Saved:", output_file, "\n")


# Plot Map of CFs  -------------

cf <- read_excel("Inputs/AWARE/AWARE20_Native_CFs.xlsx", sheet = "native_CFs") |>
  dplyr::select(Basin_ID, annual_unspecified)
demand <- read_excel("Inputs/AWARE/AWARE20_Native_CFs.xlsx", sheet = "2019_all_pHWC") |>
  dplyr::select(Basin_ID, annual_sum)
amd <- read_excel("Inputs/AWARE/AWARE20_Intermediate_Variables.xlsx", sheet = "AMD_final") # in m3/m2 month
# fmt: skip
amd$amd_sum <- amd$Jan+ amd$Feb + amd$Mar + amd$Apr + amd$May + amd$Jun + amd$Jul + amd$Aug + amd$Sep + amd$Oct + amd$Nov + amd$Dec
amd <- amd |> dplyr::select(Basin_ID, amd_sum)


years_plot <- c(2025, 2035, 2050)

cat_colors <- c(
  "0.1–0.5" = "#3880b5",
  "0.5–1" = "#81bead",
  "1–10" = "#c5e9b1",
  "10–30" = "#fdffc5",
  "30–60" = "#fbc380",
  "60–95" = "#e35b3e",
  "95-100" = "#ac211c",
  "na" = "white"
)

kmz_file <- file.path(input_dir_aware, "AWARE20_Native_CFs_geospatial.kmz")
temp_dir <- tempdir()
unzip(kmz_file, exdir = temp_dir)

kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)[1]
basins <- st_read(kml_file, quiet = TRUE)
unlink(temp_dir, recursive = TRUE)
basins$Basin_ID <- as.numeric(str_remove(basins$Name, "CFs for Basin_ID "))


pdf(file.path(output_dir, "CF_maps_all.pdf"), width = 11, height = 6.5)

for (sc in sort(unique(basin_cf_data$scenario))) {
  for (cm in sort(unique(basin_cf_data$climate_model))) {
    for (yr in years_plot) {
      cf_yr <- basin_cf_5yr %>%
        filter(scenario == sc, climate_model == cm, period_start == yr) %>%
        select(Basin_ID, cf, demand_m3_yr, availability_m3_yr)

      basins_cf <- basins %>%
        left_join(cf_yr, by = "Basin_ID") %>%
        mutate(
          cf_cat = case_when(
            cf <= 0.5 ~ "0.1–0.5",
            cf < 1 ~ "0.5–1",
            cf < 10 ~ "1–10",
            cf < 30 ~ "10–30",
            cf < 60 ~ "30–60",
            cf < 95 ~ "60–95",
            cf <= 100 ~ "95-100",
            is.na(cf) ~ "na",
            TRUE ~ "na"
          ),
          cf_cat = factor(cf_cat, levels = c("0.1–0.5", "0.5–1", "1–10", "10–30", "30–60", "60–95", "95-100", "na"))
        )

      d_for_cor <- basins_cf %>%
        st_drop_geometry() %>%
        left_join(cf, by = "Basin_ID") %>%
        left_join(demand, by = "Basin_ID") %>%
        left_join(amd, by = "Basin_ID")

      r_cf <- cor(d_for_cor$annual_unspecified, d_for_cor$cf, use = "complete.obs")
      r_dem <- cor(d_for_cor$annual_sum, d_for_cor$demand_m3_yr, use = "complete.obs")
      r_av <- cor(d_for_cor$amd_sum, d_for_cor$availability_m3_yr, use = "complete.obs")

      txt <- sprintf("cor(CF)=%.2f\ncor(Demand)=%.2f\ncor(Avail)=%.2f", r_cf, r_dem, r_av)

      p <- ggplot(basins_cf) +
        geom_sf(aes(fill = cf_cat), linewidth = 0) +
        coord_sf(crs = 4326) +
        annotate("label", x = -175, y = -55, label = txt, hjust = 0, vjust = 0, size = 4, label.size = 0.2) +
        labs(title = paste0("AWARE CF — ", sc, " — ", cm, " — ", yr), fill = paste0("CF ", yr)) +
        scale_fill_manual(values = cat_colors, drop = FALSE) +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme_minimal()

      print(p)
    }
  }
}

dev.off()

# EoF
