# Cumulative Resource Analysis by Water Stress
# Purpose: Visualize how mineral resources accumulate across water stress levels
# Shows both absolute cumulative resources and percentage of total resources
# PBH Feb 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# Load deposit data ----
deposits <- read_csv("Parameters/Deposit.csv")

# Calculate water stress ----
# Stress = water demand / (demand + available water)
deposits <- deposits |>
  mutate(gross_available = aware_available + aware_demand) |>
  mutate(water_stress = if_else(gross_available <= 0, 1, aware_demand / gross_available)) |>
  mutate(water_stress = if_else(aware_demand < 0, 0, water_stress)) |>
  mutate(water_stress = pmin(water_stress, 1)) # cap at 100%
range(deposits$water_stress, na.rm = TRUE) # check range of water stress values

# to million tons
deposits <- deposits |>
  mutate(resources_Copper = resources_Copper / 1e6) |>
  mutate(resources_Nickel = resources_Nickel / 1e6) |>
  mutate(resources_Cobalt = resources_Cobalt / 1e6) |>
  mutate(resources_Lithium = resources_Lithium / 1e6)


# Minerals to analyze
minerals <- c("Copper", "Nickel", "Cobalt", "Lithium")

deposits |>
  reframe(
    total_Copper = sum(resources_Copper, na.rm = TRUE),
    total_Nickel = sum(resources_Nickel, na.rm = TRUE),
    total_Cobalt = sum(resources_Cobalt, na.rm = TRUE),
    total_Lithium = sum(resources_Lithium, na.rm = TRUE)
  )

# Prepare data for all minerals ----
mineral_data_all <- map_dfr(unique(minerals), function(mineral) {
  col <- paste0("resources_", mineral)

  deposits |>
    filter(!is.na(.data[[col]]), !is.na(water_stress)) |>
    group_by(Basin_ID) |>
    summarise(water_stress = first(water_stress), resources = sum(.data[[col]]), .groups = "drop") |>
    arrange(water_stress) |>
    mutate(
      cumulative_resources = cumsum(resources),
      pct_resources = 100 * cumulative_resources / sum(resources),
      mineral = mineral
    )
})

mineral_data_all <- mineral_data_all |> group_by(mineral) |> arrange(water_stress, .by_group = TRUE) |> ungroup()

# Generate combined faceted plot ----
ggplot(mineral_data_all, aes(x = water_stress, ymin = 0, ymax = cumulative_resources)) +
  geom_ribbon(aes(fill = mineral), alpha = 0.7, color = "black", linewidth = 0.3) +
  facet_wrap(~mineral, scales = "free", ncol = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::comma_format(), expand = c(0, 0)) +
  scale_fill_manual(values = minerals_colors) +
  labs(x = "Water Stress", y = "Cumulative Resources (million tonnes)") +
  theme_pb_wide() +
  theme(legend.position = "none")

# Save plot ----
# fmt: skip
ggsave("Figures/Deposit/Resource_WaterStress.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

# By climate scenario -------------------------

(files <- list.files("Parameters/WaterScenarios/", pattern = "\\.csv$", full.names = TRUE))

water_scenarios <- do.call(
  rbind,
  lapply(files, function(f) {
    read.csv(f) |> transform(scenario = tools::file_path_sans_ext(basename(f)))
  })
)
unique(water_scenarios$scenario)
water_scenarios <- water_scenarios |>
  mutate(scenario = str_remove(scenario, "Deposit_")) |>
  mutate(
    climateDriver = case_when(
      str_detect(scenario, "gfdl-esm4") ~ "gfdl-esm4",
      str_detect(scenario, "ipsl-cm6a-lr") ~ "ipsl-cm6a-lr",
      str_detect(scenario, "mpi-esm1-2-hr") ~ "mpi-esm1-2-hr",
      str_detect(scenario, "mri-esm2-0") ~ "mri-esm2-0",
      str_detect(scenario, "ukesm1-0-ll") ~ "ukesm1-0-ll",
      T ~ "No Climate Scenario"
    )
  ) |>
  mutate(
    ClimateScen = case_when(
      str_detect(scenario, "picontrol") ~ "Pre-industrial baseline",
      str_detect(scenario, "ssp126") ~ "Low warming (SSP1-2.6)",
      str_detect(scenario, "ssp370") ~ "High warming (SSP3-7.0)",
      str_detect(scenario, "ssp585") ~ "Very high warming (SSP5-8.5)"
    )
  )

table(water_scenarios$ClimateScen)
table(water_scenarios$climateDriver)
names(water_scenarios)


# to long format by periond
water_long <- water_scenarios |>
  pivot_longer(
    c(-Name, -ID, -Basin_ID, -water_cons_2025, -water, -scenario, -ClimateScen, -climateDriver),
    names_to = 'metric',
    values_to = 'value'
  ) |>
  mutate(period = str_extract(metric, "2025_2030|2030_2035|2035_2040|2040_2045|2045_2050")) |>
  mutate(metric = str_remove(metric, "_2025_2030|_2030_2035|_2035_2040|_2040_2045|_2045_2050")) |>
  pivot_wider(names_from = metric, values_from = value)

# add resources data
water_long <- water_long |> left_join(deposits |> dplyr::select(ID, starts_with("resources_")), by = "ID") |> ungroup()
names(water_long)

# water_scenarios already has deposits + column `scenario` (filename)
mineral_data_all <- map_dfr(minerals, function(mineral) {
  col <- paste0("resources_", mineral)
  water_long |>
    mutate(
      gross_available = aware_available + aware_demand,
      water_stress = if_else(gross_available <= 0, 1, aware_demand / gross_available),
      water_stress = if_else(aware_demand < 0, 0, water_stress),
      water_stress = pmin(water_stress, 1)
    ) |>
    filter(!is.na(.data[[col]]), !is.na(water_stress)) |>
    group_by(ClimateScen, climateDriver, period, Basin_ID) |>
    summarise(water_stress = first(water_stress), resources = sum(.data[[col]]), .groups = "drop") |>
    arrange(ClimateScen, climateDriver, period, water_stress) |>
    group_by(ClimateScen, climateDriver, period) |>
    mutate(cumulative_resources = cumsum(resources), mineral = mineral) |>
    ungroup()
})

mineral_data_all <- mineral_data_all |>
  mutate(group_x = paste0(period, ClimateScen, climateDriver)) |>
  mutate(
    ClimateScen = factor(
      ClimateScen,
      levels = c(
        "Pre-industrial baseline",
        "Low warming (SSP1-2.6)",
        "High warming (SSP3-7.0)",
        "Very high warming (SSP5-8.5)"
      )
    )
  )


# Pick last period
mineral_data_all <- mineral_data_all |> filter(period == "2045_2050")
mineral_data_all <- mineral_data_all |> filter(climateDriver == "gfdl-esm4")


ggplot(mineral_data_all, aes(water_stress, cumulative_resources, colour = ClimateScen, group = group_x)) +
  geom_step(linewidth = 0.4) +
  facet_wrap(~mineral, scales = "free", ncol = 2) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  labs(x = "Water Stress", y = "Cumulative Resources (million tonnes)", col = "Climate Scenario") +
  theme_pb_wide() +
  theme(legend.position = c(0.8, 0.2), legend.background = element_rect(fill = "white", color = "black"))

# fmt: skip
ggsave("Figures/Deposit/Resource_WaterStress_Climate.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 2,height = 8.7 * 2)
