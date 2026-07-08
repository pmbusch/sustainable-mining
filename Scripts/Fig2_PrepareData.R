# ============================================================
# Precompute production datasets for Figure 2 and standalone production figures
# PBH Mar 2026
# ============================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# ============================================================
# 00. LOAD COMMON INPUTS -------------------------------------
# ============================================================

demand <- read.csv("Parameters/IEA_Demand.csv")
depositAll <- read.csv("Parameters/Deposit.csv")
prices <- read.csv("Parameters/MineralPrices.csv")

deposit <- depositAll |>
  dplyr::select(
    ID,
    Name,
    grade_resource_Copper,
    grade_resource_Nickel,
    grade_resource_Cobalt,
    grade_resource_Lithium,
    recovery_rate_Copper,
    recovery_rate_Nickel,
    recovery_rate_Cobalt,
    recovery_rate_Lithium,
    country,
    water,
    Basin_ID,
    OPEX_ore,
    CAPEX_opening,
    CAPEX_exp,
    share_NiCoCu,
    water_footprint
  )

# revenue shares by deposit
revShare <- deposit |>
  mutate(
    copper_revenue = grade_resource_Copper *
      recovery_rate_Copper /
      100 *
      prices[prices$Mineral == "Copper", ]$price_avg,
    nickel_revenue = grade_resource_Nickel *
      recovery_rate_Nickel /
      100 *
      prices[prices$Mineral == "Nickel", ]$price_avg,
    cobalt_revenue = grade_resource_Cobalt *
      recovery_rate_Cobalt /
      100 *
      prices[prices$Mineral == "Cobalt", ]$price_avg,
    lithium_revenue = grade_resource_Lithium *
      recovery_rate_Lithium /
      100 *
      prices[prices$Mineral == "Lithium", ]$price_avg
  ) |>
  mutate(total_revenue = copper_revenue + nickel_revenue + cobalt_revenue + lithium_revenue) |>
  mutate(
    copper_share = copper_revenue / total_revenue,
    nickel_share = nickel_revenue / total_revenue,
    cobalt_share = cobalt_revenue / total_revenue,
    lithium_share = lithium_revenue / total_revenue
  ) |>
  # mutate(copper_share = 1, nickel_share = 1, cobalt_share = 1, lithium_share = 1) |> #debug
  dplyr::select(ID, Name, copper_share, nickel_share, cobalt_share, lithium_share)

# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

(optInputs <- read.csv("Results/Optimization/DemandScenario/SPS/OptimizationInputs.csv"))
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value) |> as.numeric())
(mine_life <- optInputs |> filter(str_detect(Parameter, "Mine Life")) |> pull(Value) |> as.numeric())
(fraction_not_recovered <- optInputs |> filter(str_detect(Parameter, "not recovered")) |> pull(Value) |> as.numeric())

# ============================================================
# 01. DEMAND SCENARIO - RAW PRODUCTION + MINERAL DECOMPOSITION ----------
# ============================================================

runs_demand <- list.files("Results/Optimization/DemandScenario", recursive = TRUE, full.names = TRUE) |>
  (\(x) {
    x[
      (!str_detect(x, "SP_") &
        !str_detect(x, "Metrics") &
        !str_detect(x, "Slack") &
        !str_detect(x, "Inputs") &
        !str_detect(x, "SPS_x") &
        !str_detect(x, "NZE_x") &
        !str_detect(x, "SPS_APS") &
        !str_detect(x, "APS_NZE"))
    ]
  })()

opt_results_demand <- do.call(
  rbind,
  lapply(runs_demand, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

opt_results_demand <- opt_results_demand |>
  filter(ktons_extracted > 0 | capacity_added_ktpa > 0 | mine_opened > 0) |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  )

prod_country_demand <- opt_results_demand |>
  left_join(
    deposit |>
      dplyr::select(
        ID,
        Name,
        grade_resource_Copper,
        grade_resource_Nickel,
        grade_resource_Cobalt,
        grade_resource_Lithium,
        recovery_rate_Copper,
        recovery_rate_Nickel,
        recovery_rate_Cobalt,
        recovery_rate_Lithium,
        country,
        water,
        Basin_ID
      )
  ) |>
  # million tons
  mutate(
    Copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    Nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    Cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    Lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(Copper = sum(Copper), Nickel = sum(Nickel), Cobalt = sum(Cobalt), Lithium = sum(Lithium)) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()

head(prod_country_demand)
write.csv(prod_country_demand, "Results/Processed/prod_country_demand.csv", row.names = FALSE)

data_decomp_demand <- opt_results_demand |>
  left_join(deposit) |>
  mutate(
    Copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    Nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    Cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    Lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  ) |>
  mutate(
    years_to_end = 2050 - t,
    remaining_life = mine_life - years_to_end,
    frac = pmin(pmax(remaining_life / mine_life, 0), 1),
    CAPEX_opening_adj = CAPEX_opening - (1 - fraction_not_recovered) * CAPEX_opening * frac,
    CAPEX_exp_adj = CAPEX_exp - (1 - fraction_not_recovered) * CAPEX_exp * frac
  ) |>
  mutate(
    costs = (ktons_extracted *
      OPEX_ore /
      1e3 +
      mine_opened * CAPEX_opening_adj +
      capacity_added_ktpa * CAPEX_exp_adj / 1e3) *
      share_NiCoCu /
      1e3,
    costs = costs / (1 + r)^(t - 2025),
    water = ktons_extracted * water_footprint / 1e6 # billion m3-eq
  ) |>
  # Allocate costs at each deposit based on revenue share
  left_join(revShare) |>
  mutate(
    copper_cost = copper_share * costs,
    nickel_cost = nickel_share * costs,
    cobalt_cost = cobalt_share * costs,
    lithium_cost = lithium_share * costs,
    copper_water = copper_share * water,
    nickel_water = nickel_share * water,
    cobalt_water = cobalt_share * water,
    lithium_water = lithium_share * water
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    Copper = sum(Copper), # million tons
    Nickel = sum(Nickel),
    Cobalt = sum(Cobalt),
    Lithium = sum(Lithium),
    copper_cost = sum(copper_cost),
    nickel_cost = sum(nickel_cost),
    cobalt_cost = sum(cobalt_cost),
    lithium_cost = sum(lithium_cost),
    costs = sum(costs),
    copper_water = sum(copper_water),
    nickel_water = sum(nickel_water),
    cobalt_water = sum(cobalt_water),
    lithium_water = sum(lithium_water),
    water = sum(water)
  ) |>
  ungroup()

# Add slack costs (demand not met)
runs_slack_demand <- list.files(
  "Results/Optimization/DemandScenario",
  pattern = "Slack.*",
  recursive = TRUE,
  full.names = TRUE
) |>
  (\(x) {
    x[!str_detect(x, "SPS_x") & !str_detect(x, "NZE_x") & !str_detect(x, "SPS_APS") & !str_detect(x, "APS_NZE")]
  })()

slack_demand <- do.call(
  rbind,
  lapply(runs_slack_demand, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
) |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  )

slack_costs_demand <- slack_demand |>
  mutate(
    slack_cu_cost = slack_cu *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Copper", ]$Value) /
      (1 + r)^(t - 2025),
    slack_ni_cost = slack_ni *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Nickel", ]$Value) /
      (1 + r)^(t - 2025),
    slack_co_cost = slack_co *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Cobalt", ]$Value) /
      (1 + r)^(t - 2025),
    slack_li_cost = slack_li *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Lithium", ]$Value) /
      (1 + r)^(t - 2025)
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    slack_cu = sum(slack_cu) / 1e3, # million tons
    slack_ni = sum(slack_ni) / 1e3,
    slack_co = sum(slack_co) / 1e3,
    slack_li = sum(slack_li) / 1e3,
    slack_cu_cost = sum(slack_cu_cost) / 1e3, # billion USD
    slack_ni_cost = sum(slack_ni_cost) / 1e3,
    slack_co_cost = sum(slack_co_cost) / 1e3,
    slack_li_cost = sum(slack_li_cost) / 1e3
  ) |>
  ungroup()

# Add extra costs and demand not met
data_decomp_demand <- data_decomp_demand |>
  left_join(slack_costs_demand, by = c("Scenario", "metric")) |>
  mutate(
    Copper = Copper + slack_cu, # million tons
    Nickel = Nickel + slack_ni,
    Cobalt = Cobalt + slack_co,
    Lithium = Lithium + slack_li,
    copper_cost = copper_cost + slack_cu_cost,
    nickel_cost = nickel_cost + slack_ni_cost,
    cobalt_cost = cobalt_cost + slack_co_cost,
    lithium_cost = lithium_cost + slack_li_cost,
    costs = costs + slack_cu_cost + slack_ni_cost + slack_co_cost + slack_li_cost
  ) |>
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / Copper, # USD per ton
    Nickel_CostperTon = nickel_cost * 1e3 / Nickel,
    Cobalt_CostperTon = cobalt_cost * 1e3 / Cobalt,
    Lithium_CostperTon = lithium_cost * 1e3 / Lithium,
    Copper_WaterperTon = copper_water * 1e3 / Copper, # m3 per ton
    Nickel_WaterperTon = nickel_water * 1e3 / Nickel,
    Cobalt_WaterperTon = cobalt_water * 1e3 / Cobalt,
    Lithium_WaterperTon = lithium_water * 1e3 / Lithium
  )

head(data_decomp_demand)
write.csv(data_decomp_demand, "Results/Processed/mineral_decomp_demand.csv", row.names = FALSE)

# ============================================================
# 02. BIODIVERSITY SCENARIO - RAW PRODUCTION + MINERAL DECOMP -----------------
# ============================================================

runs_biod <- list.files("Results/Optimization/BioDScenario/NZE/", recursive = TRUE, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })()

opt_results_biod <- do.call(
  rbind,
  lapply(runs_biod, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

opt_results_biod <- opt_results_biod |>
  filter(ktons_extracted > 0 | capacity_added_ktpa > 0 | mine_opened > 0) |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  ) |>
  mutate(
    Scenario = case_when(
      Scenario == "none" ~ "All Basins",
      Scenario == "FI99" ~ "Fish Index < 99.9",
      Scenario == "FI95" ~ "Fish Index < 95",
      Scenario == "FI90" ~ "Fish Index < 90",
      Scenario == "FI85" ~ "Fish Index < 85",
      Scenario == "FI80" ~ "Fish Index < 80",
      Scenario == "FI75" ~ "Fish Index < 75",
      Scenario == "FI74" ~ "Fish Index < 74",
      Scenario == "FI73" ~ "Fish Index < 73",
      Scenario == "FI72" ~ "Fish Index < 72",
      Scenario == "FI71" ~ "Fish Index < 71",
      Scenario == "FI70" ~ "Fish Index < 70",
      Scenario == "FI65" ~ "Fish Index < 65",
      Scenario == "FI60" ~ "Fish Index < 60",
      Scenario == "FI55" ~ "Fish Index < 55",
      Scenario == "FI50" ~ "Fish Index < 50",
      TRUE ~ Scenario
    )
  )

prod_country_biod <- opt_results_biod |>
  left_join(
    deposit |>
      dplyr::select(
        ID,
        Name,
        grade_resource_Copper,
        grade_resource_Nickel,
        grade_resource_Cobalt,
        grade_resource_Lithium,
        recovery_rate_Copper,
        recovery_rate_Nickel,
        recovery_rate_Cobalt,
        recovery_rate_Lithium,
        country,
        water,
        Basin_ID
      )
  ) |>
  mutate(
    Copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    Nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    Cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    Lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(Copper = sum(Copper), Nickel = sum(Nickel), Cobalt = sum(Cobalt), Lithium = sum(Lithium)) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()

write.csv(prod_country_biod, "Results/Processed/prod_country_biod.csv", row.names = FALSE)

data_decomp_biod <- opt_results_biod |>
  left_join(deposit) |>
  mutate(
    Copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    Nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    Cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    Lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  ) |>
  mutate(
    years_to_end = 2050 - t,
    remaining_life = mine_life - years_to_end,
    frac = pmin(pmax(remaining_life / mine_life, 0), 1),
    CAPEX_opening_adj = CAPEX_opening - (1 - fraction_not_recovered) * CAPEX_opening * frac,
    CAPEX_exp_adj = CAPEX_exp - (1 - fraction_not_recovered) * CAPEX_exp * frac
  ) |>
  mutate(
    costs = (ktons_extracted *
      OPEX_ore /
      1e3 +
      mine_opened * CAPEX_opening_adj +
      capacity_added_ktpa * CAPEX_exp_adj / 1e3) *
      share_NiCoCu /
      1e3,
    costs = costs / (1 + r)^(t - 2025),
    water = ktons_extracted * water_footprint / 1e6
  ) |>
  left_join(revShare) |>
  mutate(
    copper_cost = copper_share * costs,
    nickel_cost = nickel_share * costs,
    cobalt_cost = cobalt_share * costs,
    lithium_cost = lithium_share * costs,
    copper_water = copper_share * water,
    nickel_water = nickel_share * water,
    cobalt_water = cobalt_share * water,
    lithium_water = lithium_share * water
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    Copper = sum(Copper),
    Nickel = sum(Nickel),
    Cobalt = sum(Cobalt),
    Lithium = sum(Lithium),
    copper_cost = sum(copper_cost),
    nickel_cost = sum(nickel_cost),
    cobalt_cost = sum(cobalt_cost),
    lithium_cost = sum(lithium_cost),
    costs = sum(costs),
    copper_water = sum(copper_water),
    nickel_water = sum(nickel_water),
    cobalt_water = sum(cobalt_water),
    lithium_water = sum(lithium_water),
    water = sum(water)
  ) |>
  ungroup()

runs_slack_biod <- list.files(
  "Results/Optimization/BioDScenario/NZE/",
  pattern = "Slack.*",
  recursive = TRUE,
  full.names = TRUE
)

slack_biod <- do.call(
  rbind,
  lapply(runs_slack_biod, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
) |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  ) |>
  mutate(
    Scenario = case_when(
      Scenario == "none" ~ "All Basins",
      Scenario == "FI99" ~ "Fish Index < 99.9",
      Scenario == "FI95" ~ "Fish Index < 95",
      Scenario == "FI90" ~ "Fish Index < 90",
      Scenario == "FI85" ~ "Fish Index < 85",
      Scenario == "FI80" ~ "Fish Index < 80",
      Scenario == "FI75" ~ "Fish Index < 75",
      Scenario == "FI74" ~ "Fish Index < 74",
      Scenario == "FI73" ~ "Fish Index < 73",
      Scenario == "FI72" ~ "Fish Index < 72",
      Scenario == "FI71" ~ "Fish Index < 71",
      Scenario == "FI70" ~ "Fish Index < 70",
      Scenario == "FI65" ~ "Fish Index < 65",
      Scenario == "FI60" ~ "Fish Index < 60",
      Scenario == "FI55" ~ "Fish Index < 55",
      Scenario == "FI50" ~ "Fish Index < 50",
      TRUE ~ Scenario
    )
  )

slack_costs_biod <- slack_biod |>
  mutate(
    slack_cu_cost = slack_cu *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Copper", ]$Value) /
      (1 + r)^(t - 2025),
    slack_ni_cost = slack_ni *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Nickel", ]$Value) /
      (1 + r)^(t - 2025),
    slack_co_cost = slack_co *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Cobalt", ]$Value) /
      (1 + r)^(t - 2025),
    slack_li_cost = slack_li *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Lithium", ]$Value) /
      (1 + r)^(t - 2025)
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    slack_cu = sum(slack_cu) / 1e3, # million tons
    slack_ni = sum(slack_ni) / 1e3,
    slack_co = sum(slack_co) / 1e3,
    slack_li = sum(slack_li) / 1e3,
    slack_cu_cost = sum(slack_cu_cost) / 1e3, # billion USD
    slack_ni_cost = sum(slack_ni_cost) / 1e3,
    slack_co_cost = sum(slack_co_cost) / 1e3,
    slack_li_cost = sum(slack_li_cost) / 1e3
  ) |>
  ungroup()

data_decomp_biod <- data_decomp_biod |>
  left_join(slack_costs_biod, by = c("Scenario", "metric")) |>
  mutate(
    Copper = Copper + slack_cu, # million tons
    Nickel = Nickel + slack_ni,
    Cobalt = Cobalt + slack_co,
    Lithium = Lithium + slack_li,
    copper_cost = copper_cost + slack_cu_cost,
    nickel_cost = nickel_cost + slack_ni_cost,
    cobalt_cost = cobalt_cost + slack_co_cost,
    lithium_cost = lithium_cost + slack_li_cost,
    costs = costs + slack_cu_cost + slack_ni_cost + slack_co_cost + slack_li_cost
  ) |>
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / Copper,
    Nickel_CostperTon = nickel_cost * 1e3 / Nickel,
    Cobalt_CostperTon = cobalt_cost * 1e3 / Cobalt,
    Lithium_CostperTon = lithium_cost * 1e3 / Lithium,
    Copper_WaterperTon = copper_water * 1e3 / Copper,
    Nickel_WaterperTon = nickel_water * 1e3 / Nickel,
    Cobalt_WaterperTon = cobalt_water * 1e3 / Cobalt,
    Lithium_WaterperTon = lithium_water * 1e3 / Lithium
  )

write.csv(data_decomp_biod, "Results/Processed/mineral_decomp_biod.csv", row.names = FALSE)

# ============================================================
# 03. COST DESALINATION SCENARIO - RAW PRODUCTION + MINERAL DECOMP -----------------
# ============================================================

runs_CostDes <- list.files("Results/Optimization/DesCostScenario/NZE/FI100/", recursive = TRUE, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })()

opt_results_CostDes <- do.call(
  rbind,
  lapply(runs_CostDes, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

table(opt_results_CostDes$Scenario)
opt_results_CostDes <- opt_results_CostDes |>
  filter(ktons_extracted > 0 | capacity_added_ktpa > 0 | mine_opened > 0) |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  ) |>
  mutate(
    Scenario = case_when(
      Scenario == "DC025" ~ "$0.25/m3",
      Scenario == "DC05" ~ "$0.5/m3",
      Scenario == "DC1" ~ "$1.0/m3",
      Scenario == "DC15" ~ "$1.5/m3",
      Scenario == "DC2" ~ "$2.0/m3",
      Scenario == "DC25" ~ "$2.5/m3",
      Scenario == "DC5" ~ "$5.0/m3",
      Scenario == "DC10" ~ "$10/m3",
      TRUE ~ Scenario
    )
  )
table(opt_results_CostDes$Scenario)

prod_country_CostDes <- opt_results_CostDes |>
  left_join(
    deposit |>
      dplyr::select(
        ID,
        Name,
        grade_resource_Copper,
        grade_resource_Nickel,
        grade_resource_Cobalt,
        grade_resource_Lithium,
        recovery_rate_Copper,
        recovery_rate_Nickel,
        recovery_rate_Cobalt,
        recovery_rate_Lithium,
        country,
        water,
        Basin_ID
      )
  ) |>
  mutate(
    Copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    Nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    Cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    Lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(Copper = sum(Copper), Nickel = sum(Nickel), Cobalt = sum(Cobalt), Lithium = sum(Lithium)) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()

head(prod_country_CostDes)
write.csv(prod_country_CostDes, "Results/Processed/prod_country_CostDes.csv", row.names = FALSE)

data_decomp_CostDes <- opt_results_CostDes |>
  left_join(deposit) |>
  mutate(
    Copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    Nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    Cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    Lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  ) |>
  mutate(
    years_to_end = 2050 - t,
    remaining_life = mine_life - years_to_end,
    frac = pmin(pmax(remaining_life / mine_life, 0), 1),
    CAPEX_opening_adj = CAPEX_opening - (1 - fraction_not_recovered) * CAPEX_opening * frac,
    CAPEX_exp_adj = CAPEX_exp - (1 - fraction_not_recovered) * CAPEX_exp * frac
  ) |>
  mutate(
    costs = (ktons_extracted *
      OPEX_ore /
      1e3 +
      mine_opened * CAPEX_opening_adj +
      capacity_added_ktpa * CAPEX_exp_adj / 1e3) *
      share_NiCoCu /
      1e3,
    costs = costs / (1 + r)^(t - 2025),
    water = ktons_extracted * water_footprint / 1e6
  ) |>
  left_join(revShare) |>
  mutate(
    copper_cost = copper_share * costs,
    nickel_cost = nickel_share * costs,
    cobalt_cost = cobalt_share * costs,
    lithium_cost = lithium_share * costs,
    copper_water = copper_share * water,
    nickel_water = nickel_share * water,
    cobalt_water = cobalt_share * water,
    lithium_water = lithium_share * water
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    Copper = sum(Copper),
    Nickel = sum(Nickel),
    Cobalt = sum(Cobalt),
    Lithium = sum(Lithium),
    copper_cost = sum(copper_cost),
    nickel_cost = sum(nickel_cost),
    cobalt_cost = sum(cobalt_cost),
    lithium_cost = sum(lithium_cost),
    costs = sum(costs),
    copper_water = sum(copper_water),
    nickel_water = sum(nickel_water),
    cobalt_water = sum(cobalt_water),
    lithium_water = sum(lithium_water),
    water = sum(water)
  ) |>
  ungroup()

runs_slack_CostDes <- list.files(
  "Results/Optimization/DesCostScenario/NZE/FI100/",
  pattern = "Slack.*",
  recursive = TRUE,
  full.names = TRUE
)

slack_CostDes <- do.call(
  rbind,
  lapply(runs_slack_CostDes, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
) |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  ) |>
  mutate(
    Scenario = case_when(
      Scenario == "DC025" ~ "$0.25/m3",
      Scenario == "DC05" ~ "$0.5/m3",
      Scenario == "DC1" ~ "$1.0/m3",
      Scenario == "DC15" ~ "$1.5/m3",
      Scenario == "DC2" ~ "$2.0/m3",
      Scenario == "DC25" ~ "$2.5/m3",
      Scenario == "DC5" ~ "$5.0/m3",
      Scenario == "DC10" ~ "$10/m3",
      TRUE ~ Scenario
    )
  )
table(slack_CostDes$Scenario)

slack_costs_CostDes <- slack_CostDes |>
  mutate(
    slack_cu_cost = slack_cu *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Copper", ]$Value) /
      (1 + r)^(t - 2025),
    slack_ni_cost = slack_ni *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Nickel", ]$Value) /
      (1 + r)^(t - 2025),
    slack_co_cost = slack_co *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Cobalt", ]$Value) /
      (1 + r)^(t - 2025),
    slack_li_cost = slack_li *
      as.numeric(optInputs[optInputs$Parameter == "Slack cost Lithium", ]$Value) /
      (1 + r)^(t - 2025)
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    slack_cu = sum(slack_cu) / 1e3, # million tons
    slack_ni = sum(slack_ni) / 1e3,
    slack_co = sum(slack_co) / 1e3,
    slack_li = sum(slack_li) / 1e3,
    slack_cu_cost = sum(slack_cu_cost) / 1e3, # billion USD
    slack_ni_cost = sum(slack_ni_cost) / 1e3,
    slack_co_cost = sum(slack_co_cost) / 1e3,
    slack_li_cost = sum(slack_li_cost) / 1e3
  ) |>
  ungroup()

data_decomp_CostDes <- data_decomp_CostDes |>
  left_join(slack_costs_CostDes, by = c("Scenario", "metric")) |>
  mutate(
    Copper = Copper + slack_cu, # million tons
    Nickel = Nickel + slack_ni,
    Cobalt = Cobalt + slack_co,
    Lithium = Lithium + slack_li,
    copper_cost = copper_cost + slack_cu_cost,
    nickel_cost = nickel_cost + slack_ni_cost,
    cobalt_cost = cobalt_cost + slack_co_cost,
    lithium_cost = lithium_cost + slack_li_cost,
    costs = costs + slack_cu_cost + slack_ni_cost + slack_co_cost + slack_li_cost
  ) |>
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / Copper,
    Nickel_CostperTon = nickel_cost * 1e3 / Nickel,
    Cobalt_CostperTon = cobalt_cost * 1e3 / Cobalt,
    Lithium_CostperTon = lithium_cost * 1e3 / Lithium,
    Copper_WaterperTon = copper_water * 1e3 / Copper,
    Nickel_WaterperTon = nickel_water * 1e3 / Nickel,
    Cobalt_WaterperTon = cobalt_water * 1e3 / Cobalt,
    Lithium_WaterperTon = lithium_water * 1e3 / Lithium
  )

head(data_decomp_CostDes)
write.csv(data_decomp_CostDes, "Results/Processed/mineral_decomp_CostDes.csv", row.names = FALSE)

# EoF
