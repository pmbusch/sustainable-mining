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

# ============================================================
# 00b. DEMAND-CAPPED CO-PRODUCT ALLOCATION --------------------
# Co-mined by-products (esp. cobalt riding on Cu/Ni ore) can be
# produced far beyond what demand requires. Excess tonnage above
# cumulative 2025-2050 demand is a by-product of another metal's
# extraction and should not be charged against that mineral's
# cost/water intensity: the worst (highest cost+water) tail of
# deposits has that mineral's allocation share zeroed and
# redistributed to the deposit's other Cu/Ni/Co co-products.
# ============================================================

# Cumulative 2025-2050 demand (Mt) per mineral per demand scenario
demand_mt <- demand |>
  dplyr::select(Scenario, Year, Copper, Nickel, Lithium, Cobalt) |>
  pivot_longer(c(-Scenario, -Year), names_to = "Mineral", values_to = "Demand") |>
  group_by(Scenario, Mineral) |>
  reframe(Demand_Mt = sum(Demand) / 1e3) |>
  ungroup()

# Wide version keyed by demand Scenario (SPS/APS/NZE), used as the per-ton denominator
demand_mt_wide <- demand_mt |>
  pivot_wider(names_from = Mineral, values_from = Demand_Mt) |>
  dplyr::rename(Copper_Dem = Copper, Nickel_Dem = Nickel, Cobalt_Dem = Cobalt, Lithium_Dem = Lithium)

# Biodiversity and desalination-cost runs all sit on top of the fixed NZE demand trajectory
nze_dem <- demand_mt_wide |> filter(Scenario == "NZE")

# For each mineral, within each (Scenario, metric) run, cap attributed production at
# demand: rank deposits with nonzero production of that mineral by the average of their
# percentile rank on cost-per-ton and water-per-ton (baseline shares), and keep only the
# best-ranked deposits up to the one where cumulative production first reaches demand.
cap_mineral_tail <- function(dep_totals, mineral, demand_mt) {
  share_col <- c(Copper = "copper_share", Nickel = "nickel_share", Cobalt = "cobalt_share", Lithium = "lithium_share")[[
    mineral
  ]]

  dep_totals |>
    dplyr::select(
      Scenario,
      metric,
      DemandScenario,
      ID,
      prod = all_of(mineral),
      costs,
      water,
      share = all_of(share_col)
    ) |>
    mutate(cost_pt = share * costs / prod, water_pt = share * water / prod) |>
    left_join(
      demand_mt |> filter(Mineral == mineral) |> dplyr::select(DemandScenario = Scenario, Demand_Mt),
      by = "DemandScenario"
    ) |>
    group_by(Scenario, metric) |>
    group_modify(function(df, key) {
      total_prod <- sum(df$prod, na.rm = TRUE)
      dem <- unique(df$Demand_Mt)[1]
      if (is.na(dem) || total_prod <= dem) {
        df$keep <- TRUE
        return(df)
      }
      ranked <- df |>
        filter(prod > 0) |>
        mutate(
          rank_cost = percent_rank(cost_pt),
          rank_water = percent_rank(water_pt),
          rank_comb = (rank_cost + rank_water) / 2
        ) |>
        arrange(rank_comb) |>
        mutate(cum_prod = cumsum(prod))
      cutoff <- which(ranked$cum_prod >= dem)[1]
      if (is.na(cutoff)) {
        cutoff <- nrow(ranked)
      }
      keep_ids <- ranked$ID[seq_len(cutoff)]
      df$keep <- df$ID %in% keep_ids | df$prod == 0
      df
    }) |>
    ungroup() |>
    dplyr::select(Scenario, metric, ID, keep)
}

# Zero the flagged (Scenario, metric, ID, mineral) shares and redistribute each deposit's
# zeroed share mass proportionally across its remaining Cu/Ni/Co shares (never to Lithium)
cap_and_redistribute_shares <- function(dep_totals, demand_mt) {
  keep_flags <- dep_totals |> dplyr::select(Scenario, metric, ID)
  for (m in c("Copper", "Nickel", "Cobalt", "Lithium")) {
    flag <- cap_mineral_tail(dep_totals, m, demand_mt)
    names(flag)[names(flag) == "keep"] <- paste0("keep_", m)
    keep_flags <- keep_flags |> left_join(flag, by = c("Scenario", "metric", "ID"))
  }

  dep_totals |>
    left_join(keep_flags, by = c("Scenario", "metric", "ID")) |>
    mutate(
      z_copper = if_else(keep_Copper, copper_share, 0),
      z_nickel = if_else(keep_Nickel, nickel_share, 0),
      z_cobalt = if_else(keep_Cobalt, cobalt_share, 0),
      excess = if_else(keep_Copper, 0, copper_share) +
        if_else(keep_Nickel, 0, nickel_share) +
        if_else(keep_Cobalt, 0, cobalt_share) +
        if_else(keep_Lithium, 0, lithium_share),
      pool = z_copper + z_nickel + z_cobalt,
      copper_share_adj = z_copper + if_else(pool > 0, z_copper / pool * excess, 0),
      nickel_share_adj = z_nickel + if_else(pool > 0, z_nickel / pool * excess, 0),
      cobalt_share_adj = z_cobalt + if_else(pool > 0, z_cobalt / pool * excess, 0),
      lithium_share_adj = if_else(keep_Lithium, lithium_share, 0)
    ) |>
    dplyr::select(-z_copper, -z_nickel, -z_cobalt, -excess, -pool, -starts_with("keep_"))
}

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
  # Aggregate to deposit level (Scenario x metric x ID) across the full horizon,
  # then cap each mineral's allocation share at demand before splitting costs/water
  group_by(Scenario, metric, ID) |>
  reframe(
    Copper = sum(Copper),
    Nickel = sum(Nickel),
    Cobalt = sum(Cobalt),
    Lithium = sum(Lithium),
    costs = sum(costs),
    water = sum(water)
  ) |>
  ungroup() |>
  left_join(revShare) |>
  mutate(DemandScenario = Scenario) |>
  cap_and_redistribute_shares(demand_mt) |>
  mutate(
    copper_cost = copper_share_adj * costs,
    nickel_cost = nickel_share_adj * costs,
    cobalt_cost = cobalt_share_adj * costs,
    lithium_cost = lithium_share_adj * costs,
    copper_water = copper_share_adj * water,
    nickel_water = nickel_share_adj * water,
    cobalt_water = cobalt_share_adj * water,
    lithium_water = lithium_share_adj * water
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
  # Per-ton metrics use cumulative demand (not production) as the denominator,
  # so by-product tonnage nobody needs doesn't dilute the per-ton cost/water
  left_join(demand_mt_wide, by = "Scenario") |>
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / Copper_Dem, # USD per ton of demand
    Nickel_CostperTon = nickel_cost * 1e3 / Nickel_Dem,
    Cobalt_CostperTon = cobalt_cost * 1e3 / Cobalt_Dem,
    Lithium_CostperTon = lithium_cost * 1e3 / Lithium_Dem,
    Copper_WaterperTon = copper_water * 1e3 / Copper_Dem, # m3 per ton of demand
    Nickel_WaterperTon = nickel_water * 1e3 / Nickel_Dem,
    Cobalt_WaterperTon = cobalt_water * 1e3 / Cobalt_Dem,
    Lithium_WaterperTon = lithium_water * 1e3 / Lithium_Dem
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
  # Aggregate to deposit level, then cap each mineral's allocation share at NZE demand
  group_by(Scenario, metric, ID) |>
  reframe(
    Copper = sum(Copper),
    Nickel = sum(Nickel),
    Cobalt = sum(Cobalt),
    Lithium = sum(Lithium),
    costs = sum(costs),
    water = sum(water)
  ) |>
  ungroup() |>
  left_join(revShare) |>
  mutate(DemandScenario = "NZE") |>
  cap_and_redistribute_shares(demand_mt) |>
  mutate(
    copper_cost = copper_share_adj * costs,
    nickel_cost = nickel_share_adj * costs,
    cobalt_cost = cobalt_share_adj * costs,
    lithium_cost = lithium_share_adj * costs,
    copper_water = copper_share_adj * water,
    nickel_water = nickel_share_adj * water,
    cobalt_water = cobalt_share_adj * water,
    lithium_water = lithium_share_adj * water
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
  # Per-ton metrics use cumulative NZE demand (not production) as the denominator
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / nze_dem$Copper_Dem,
    Nickel_CostperTon = nickel_cost * 1e3 / nze_dem$Nickel_Dem,
    Cobalt_CostperTon = cobalt_cost * 1e3 / nze_dem$Cobalt_Dem,
    Lithium_CostperTon = lithium_cost * 1e3 / nze_dem$Lithium_Dem,
    Copper_WaterperTon = copper_water * 1e3 / nze_dem$Copper_Dem,
    Nickel_WaterperTon = nickel_water * 1e3 / nze_dem$Nickel_Dem,
    Cobalt_WaterperTon = cobalt_water * 1e3 / nze_dem$Cobalt_Dem,
    Lithium_WaterperTon = lithium_water * 1e3 / nze_dem$Lithium_Dem
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
  # Aggregate to deposit level, then cap each mineral's allocation share at NZE demand
  group_by(Scenario, metric, ID) |>
  reframe(
    Copper = sum(Copper),
    Nickel = sum(Nickel),
    Cobalt = sum(Cobalt),
    Lithium = sum(Lithium),
    costs = sum(costs),
    water = sum(water)
  ) |>
  ungroup() |>
  left_join(revShare) |>
  mutate(DemandScenario = "NZE") |>
  cap_and_redistribute_shares(demand_mt) |>
  mutate(
    copper_cost = copper_share_adj * costs,
    nickel_cost = nickel_share_adj * costs,
    cobalt_cost = cobalt_share_adj * costs,
    lithium_cost = lithium_share_adj * costs,
    copper_water = copper_share_adj * water,
    nickel_water = nickel_share_adj * water,
    cobalt_water = cobalt_share_adj * water,
    lithium_water = lithium_share_adj * water
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
  # Per-ton metrics use cumulative NZE demand (not production) as the denominator
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / nze_dem$Copper_Dem,
    Nickel_CostperTon = nickel_cost * 1e3 / nze_dem$Nickel_Dem,
    Cobalt_CostperTon = cobalt_cost * 1e3 / nze_dem$Cobalt_Dem,
    Lithium_CostperTon = lithium_cost * 1e3 / nze_dem$Lithium_Dem,
    Copper_WaterperTon = copper_water * 1e3 / nze_dem$Copper_Dem,
    Nickel_WaterperTon = nickel_water * 1e3 / nze_dem$Nickel_Dem,
    Cobalt_WaterperTon = cobalt_water * 1e3 / nze_dem$Cobalt_Dem,
    Lithium_WaterperTon = lithium_water * 1e3 / nze_dem$Lithium_Dem
  )

head(data_decomp_CostDes)
write.csv(data_decomp_CostDes, "Results/Processed/mineral_decomp_CostDes.csv", row.names = FALSE)

# EoF
