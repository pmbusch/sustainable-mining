# =============================================================================
# Calculate total mineral demand per sample
# Uses demand_level and share_LFP draws to reconstruct demand time series
# =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
demand_raw <- read.csv("Parameters/IEA_Demand.csv")
samples <- read_csv("Parameters/samples.csv", show_col_types = FALSE)

# Filter scenarios
dem_sps <- demand_raw %>% filter(Scenario == "SPS") %>% arrange(Year)
dem_aps <- demand_raw %>% filter(Scenario == "APS") %>% arrange(Year)
dem_nze <- demand_raw %>% filter(Scenario == "NZE") %>% arrange(Year)

# Compute total demand per sample using a function per row
compute_demand <- function(demand_level, share_LFP) {
  # Blend columns
  blend <- function(col) {
    if (demand_level <= 1.0) {
      (1 - demand_level) * dem_sps[[col]] + demand_level * dem_aps[[col]]
    } else {
      w <- demand_level - 1.0
      (1 - w) * dem_aps[[col]] + w * dem_nze[[col]]
    }
  }

  # LFP scale factor (vector across years)
  lfp_scale <- blend("share_LFP") / max(share_LFP, 0.01)
  ev_Nickel_scaled <- sum(blend("ev_Nickel") * lfp_scale)
  ev_Cobalt_scaled <- sum(blend("ev_Cobalt") * lfp_scale)

  # Demand in million tons
  tibble(
    demand_Copper = sum(blend("Copper")) / 1e3,
    demand_Nickel = (sum(blend("Nickel")) - sum(blend("ev_Nickel")) + ev_Nickel_scaled) / 1e3,
    demand_Cobalt = (sum(blend("Cobalt")) - sum(blend("ev_Cobalt")) + ev_Cobalt_scaled) / 1e3,
    demand_Lithium = sum(blend("Lithium")) / 1e3
  ) |>
    mutate(mineral_demand = demand_Copper + demand_Nickel + demand_Cobalt + demand_Lithium)
}

demand_per_sample <- samples %>%
  rowwise() %>%
  reframe(compute_demand(demand_level, share_LFP)) %>%
  bind_cols(samples %>% select(sample_id))

write_csv(demand_per_sample, "Parameters/demand_per_sample.csv")
