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
compute_demand <- function(demand_level, share_LFP, ni_co_ratio) {
  blend <- function(col) {
    if (demand_level < 0) {
      (1 + demand_level) * dem_sps[[col]]
    } else if (demand_level <= 1.0) {
      (1 - demand_level) * dem_sps[[col]] + demand_level * dem_aps[[col]]
    } else if (demand_level <= 2.0) {
      w <- demand_level - 1.0
      (1 - w) * dem_aps[[col]] + w * dem_nze[[col]]
    } else {
      (1 + (demand_level - 2)) * dem_nze[[col]]
    }
  }

  # 2a. Blend (handled by blend())

  # 2b. LFP scale - applied to blended EV series
  ev_Nickel_blended <- blend("ev_Nickel")
  ev_Cobalt_blended <- blend("ev_Cobalt")
  lfp_scale <- blend("share_LFP") / max(share_LFP, 0.01)
  ev_Nickel_scaled <- ev_Nickel_blended * lfp_scale
  ev_Cobalt_scaled <- ev_Cobalt_blended * lfp_scale

  # 2c. Redistribute EV pool by ni_co_ratio
  ev_NiCo_total <- ev_Nickel_scaled + ev_Cobalt_scaled
  ev_Nickel_new <- ev_NiCo_total * ni_co_ratio / (ni_co_ratio + 1.0)
  ev_Cobalt_new <- ev_NiCo_total * 1.0 / (ni_co_ratio + 1.0)

  # 2d. Non-EV uses pre-scale blended EV (matching Julia)
  non_ev_Nickel <- blend("Nickel") - ev_Nickel_blended
  non_ev_Cobalt <- blend("Cobalt") - ev_Cobalt_blended

  # Sum over years and return
  tibble(
    demand_Copper = sum(blend("Copper")) / 1e3,
    demand_Nickel = sum(non_ev_Nickel + ev_Nickel_new) / 1e3,
    demand_Cobalt = sum(non_ev_Cobalt + ev_Cobalt_new) / 1e3,
    demand_Lithium = sum(blend("Lithium")) / 1e3
  ) |>
    mutate(mineral_demand = demand_Copper + demand_Nickel + demand_Cobalt + demand_Lithium)
}

demand_per_sample <- samples %>%
  rowwise() %>%
  reframe(compute_demand(demand_level, share_LFP, ni_co_ratio)) %>%
  bind_cols(samples %>% select(sample_id))


write_csv(demand_per_sample, "Parameters/demand_per_sample.csv")
