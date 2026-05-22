# Mine Cost Curve — 4 Minerals (Copper, Nickel, Cobalt, Lithium)
# PBH May 2026
# Source: Parameters/Deposit.csv + Parameters/MineralPrices.csv

source("Scripts/00-Libraries.R", encoding = "UTF-8")

## Data ----
dep <- read.csv("Parameters/Deposit.csv")
prices <- read.csv("Parameters/MineralPrices.csv")
demand <- read.csv("Parameters/IEA_Demand.csv")

## Parameters ----
p_cu <- prices$price_avg[prices$Mineral == "Copper"]
p_ni <- prices$price_avg[prices$Mineral == "Nickel"]
p_co <- prices$price_avg[prices$Mineral == "Cobalt"]

r <- 0.07
min_rec_M <- 0.05 # minimum recoverable resources to include [million tons]

# Y-axis upper limits per mineral [USD/ton] — adjust to clip outliers
ylim_mineral <- c(Copper = 25000, Nickel = 50000, Cobalt = 200000, Lithium = 100000)

## NZE cumulative demand 2025–2050 [million tons] ----
nze_demand <- demand |>
  filter(Scenario == "NZE") |>
  summarize(
    Copper = sum(Copper) / 1e3, # kton → million tons
    Nickel = sum(Nickel) / 1e3,
    Cobalt = sum(Cobalt) / 1e3,
    Lithium = sum(Lithium) / 1e3
  ) |>
  pivot_longer(everything(), names_to = "mineral", values_to = "cum_demand") |>
  mutate(mineral = factor(mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

## Revenue-based allocation and production setup ----
dep <- dep |>
  mutate(
    # Revenue per ton of ore [USD/ton ore] — used for intra-NiCoCu cost allocation
    rev_cu = p_cu * (grade_resource_Copper / 100) * recovery_rate_Copper,
    rev_ni = p_ni * (grade_resource_Nickel / 100) * recovery_rate_Nickel,
    rev_co = p_co * (grade_resource_Cobalt / 100) * recovery_rate_Cobalt,
    total_rev = rev_cu + rev_ni + rev_co,
    # Allocation fractions: Cu + Ni + Co sum to share_NiCoCu; Li gets 100%
    alloc_cu = if_else(total_rev > 0, rev_cu / total_rev * share_NiCoCu, share_NiCoCu),
    alloc_ni = if_else(total_rev > 0, rev_ni / total_rev * share_NiCoCu, 0),
    alloc_co = if_else(total_rev > 0, rev_co / total_rev * share_NiCoCu, 0),
    alloc_li = 1,
    # Mine life = 1 / max_depletion_rate (mine operates at max rate until resources exhausted)
    mine_life_i = 1 / max_depletion_rate,
    annuity_i = (1 - (1 + r)^(-mine_life_i)) / r,
    # Ore throughput at max depletion rate [tons ore/year]
    prod_ore = resources_ore * max_depletion_rate,
    # Existing capacity (cap2025) is cost-free; only incremental capacity above it has CAPEX_exp
    add_cap_ore = pmax(0, prod_ore - cap2025),
    # Status grouping
    status_open = factor(
      if_else(status == "Production", "Producing (in 2025)", "In development,\nevaluation or exploration"),
      levels = c("Producing (in 2025)", "In development,\nevaluation or exploration")
    )
  )

## Levelized cost computation — one call per mineral ----
# OPEX_ore [USD/ton ore], CAPEX_opening [million USD], CAPEX_exp [USD/(ton ore/yr capacity)]
# water_footprint [m3 world-eq / ton ore]; grade columns in %; recovery columns as fractions
make_curve <- function(d, res_col, grade_col, rec_col, alloc_col, label) {
  d |>
    filter(.data[[res_col]] > 0) |>
    mutate(
      g = .data[[grade_col]] / 100, # grade fraction
      rr = .data[[rec_col]], # recovery fraction
      al = .data[[alloc_col]], # cost/water allocation fraction
      # Recoverable resources [million tons metal] — x-axis bar width
      rec_M = .data[[res_col]] * rr / 1e6,
      # Discounted metal production over mine life [tons metal]; annuity varies by mine
      metal_prod = prod_ore * g * rr,
      disc_prod = metal_prod * annuity_i,
      # OPEX per ton of metal [USD/ton]: allocated ore-cost / yield
      opex_per_ton = if_else(g * rr > 0, OPEX_ore * al / (g * rr), NA_real_),
      # Allocated CAPEX [USD]: fixed opening + expansion for capacity above cap2025
      capex_alloc = (CAPEX_opening * 1e6 + CAPEX_exp * add_cap_ore) * al,
      # Levelized extraction cost [USD/ton metal]
      level_cost = opex_per_ton + if_else(disc_prod > 0, capex_alloc / disc_prod, NA_real_),
      # Scarce water use [m3 world-eq / ton metal]: water_footprint is per ton ore
      wf_per_ton = if_else(g * rr > 0, water_footprint * al / (g * rr), NA_real_),
      mineral = label
    ) |>
    filter(rec_M > min_rec_M, is.finite(level_cost), level_cost > 0) |>
    select(Name, country, mineral, status_open, level_cost, wf_per_ton, rec_M)
}

curve_data <- bind_rows(
  make_curve(dep, "resources_Copper", "grade_resource_Copper", "recovery_rate_Copper", "alloc_cu", "Copper"),
  make_curve(dep, "resources_Nickel", "grade_resource_Nickel", "recovery_rate_Nickel", "alloc_ni", "Nickel"),
  make_curve(dep, "resources_Cobalt", "grade_resource_Cobalt", "recovery_rate_Cobalt", "alloc_co", "Cobalt"),
  make_curve(dep, "resources_Lithium", "grade_resource_Lithium", "recovery_rate_Lithium", "alloc_li", "Lithium")
) |>
  mutate(mineral = factor(mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

## Sort bars, compute cumulative x positions, and cap at y-limits ----
# Producing mines appear first (left), sorted by cost within each group
curve_data <- curve_data |>
  arrange(mineral, status_open, level_cost) |>
  group_by(mineral) |>
  mutate(xmin = lag(cumsum(rec_M), default = 0), xmax = xmin + rec_M) |>
  ungroup() |>
  mutate(level_cost = pmin(level_cost, ylim_mineral[as.character(mineral)]))

## Label and separator positions ----
open_boundary <- curve_data |>
  filter(status_open == "Producing (in 2025)") |>
  group_by(mineral) |>
  summarize(boundary_x = max(xmax), open_mid = max(xmax) / 2, .groups = "drop")

rest_mids <- curve_data |>
  filter(status_open == "In development,\nevaluation or exploration") |>
  group_by(mineral) |>
  summarize(rest_mid = (min(xmin) + max(xmax)) * 0.4, .groups = "drop")

# Status labels shown only in Lithium panel
label_data <- bind_rows(
  open_boundary |> transmute(mineral, label = "Producing (in 2025)", x = open_mid),
  rest_mids |> transmute(mineral, label = "In development,\nevaluation\nor exploration", x = rest_mid)
) |>
  filter(mineral == "Lithium")

## Plot ----
range(curve_data$wf_per_ton)
p <- ggplot(curve_data) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = level_cost, fill = wf_per_ton),
    color = "grey30",
    linewidth = 0.01
  ) +
  # Open/Rest separator (grey dashed)
  # geom_vline(
  #   data = open_boundary,
  #   aes(xintercept = boundary_x),
  #   linetype = "dashed",
  #   color = "grey60",
  #   linewidth = 0.3
  # ) +
  # NZE cumulative demand line (black dashed) with rotated label
  geom_vline(data = nze_demand, aes(xintercept = cum_demand), linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_text(
    data = filter(nze_demand,mineral=="Copper"),
    aes(x = cum_demand, y = Inf, label = "NZE Demand 2025-2050"),
    angle = 90, hjust = 1.05, vjust = -0.5,
    size = 8 * 5 / 14 * 0.8, color = "#525252", inherit.aes = FALSE
  ) +
  # Status labels (Lithium panel only)
  geom_text(
    data = label_data,
    aes(x = x, y = Inf, label = label),
    vjust = 1.3, size = 8 * 5 / 14 * 0.8, color = "#525252", inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(8, "Spectral")),
    trans = "log10",
    labels = scales::label_comma(),
    name = expression("Scarce Water Use [" ~ m^3 * " world-eq / ton mineral]")
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$"),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.02))) +
  facet_wrap(~mineral, scales = "free", ncol = 2) +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Recoverable resources [million mineral tons]", y = "Levelized Extraction Cost [USD / ton mineral]") +
  theme_pb_large() +
  guides(fill = guide_colorbar(barwidth = unit(12, "cm"), title.position = "top")) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    legend.title = element_text(size = 8, hjust = 0.5),
    legend.text = element_text(size = 7)
  )

# fmt: skip
ggsave("Figures/ExtData-Figures/mineral_cost_curves.png", p, units = "cm", dpi = 600, width = 18, height = 18)
# fmt: skip
ggsave("Figures/ExtData-Figures/mineral_cost_curves.svg", p, units = "cm", dpi = 600, width = 18, height = 18)

# EoF
