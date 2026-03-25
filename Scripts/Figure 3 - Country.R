# Country variation analysis
# Variables that change across scenarios: Mineral Extraction (revenue), Water Impact
# Scenarios: Demand, Fish biodiversity, Cost degradation
# Other: Share of 2025 revenue of metals of GDP
# PBH March 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')


# LOAD --------------

## Supply Results --------------

# Files with results
deposit <- read.csv("Parameters/Deposit.csv")

file_path <- "Results/Optimization/DemandScenario"
# file_path <- "Results/Optimization/BioDScenario/NZE/"
# file_path <- "Results/Optimization/DesCostScenario/NZE/FI100/DC05"
(runs <- list.files(file_path, recursive = T, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })())
runs <- runs[str_detect(runs, "NZE\\/")] # NZE for now
# runs <- runs[str_detect(runs, "FI70|none")]

# Selected runs to compare
runs <- c(
  "Results/Optimization/DemandScenario/NZE/Base.csv", # Reference
  "Results/Optimization/DemandScenario/NZE/Water_Eps05.csv",
  "Results/Optimization/DesCostScenario/NZE/FI100/DC05/Water_Eps05.csv",
  "Results/Optimization/BioDScenario/NZE/FI70/Water_Eps05.csv",
  "Results/Optimization/DesCostScenario/NZE/FI70/DC05/Water_Eps05.csv"
)


opt_results <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)


opt_results <- opt_results |> filter(ktons_extracted > 0 | capacity_added_ktpa > 0 | mine_opened > 0) # reduce size
head(opt_results)
unique(opt_results$file_name)
opt_results <- opt_results |>
  mutate(
    Scenario = case_when(
      str_detect(folder, "Base") ~ "Reference",
      str_detect(folder, "FI100\\/DC05") ~ "Water Desalination $0.5/m3",
      str_detect(folder, "FI70\\/DC05") ~ "Water Desalination $0.5/m3 + Fish Biodiversity < 70",
      str_detect(folder, "FI70") ~ "Fish Biodiversity < 70",
      str_detect(folder, "Water_Eps05") ~ "5% Cost Increase",
      T ~ "NA"
    )
  )
table(opt_results$Scenario)

depositAll <- read.csv("Parameters/Deposit.csv")

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
    water_footprint,
    aware_cf,
    Basin_ID,
    OPEX_ore,
    CAPEX_opening,
    CAPEX_exp
  )

# Cost parameters
optInputs <- read.csv("Results/Optimization/DemandScenario/NZE/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value) |> as.numeric()) # 7%
# residual value
(mine_life <- optInputs |> filter(str_detect(Parameter, "Mine Life")) |> pull(Value) |> as.numeric())
(fraction_not_recovered <- optInputs |> filter(str_detect(Parameter, "Fraction")) |> pull(Value) |> as.numeric())

prod <- opt_results |>
  left_join(deposit) |>
  # adjust costs due to remaining life
  mutate(
    years_to_end = 2050 - t,
    remaining_life = mine_life - years_to_end,
    frac = pmin(pmax(remaining_life / mine_life, 0), 1),
    CAPEX_opening_adj = CAPEX_opening - (1 - fraction_not_recovered) * CAPEX_opening * frac,
    CAPEX_exp_adj = CAPEX_exp - (1 - fraction_not_recovered) * CAPEX_exp * frac
  ) |>
  mutate(
    # fmt: skip
    costs = (ktons_extracted * OPEX_ore / 1e3 + mine_opened * CAPEX_opening_adj + capacity_added_ktpa * CAPEX_exp_adj / 1e3)/1e3, # billion USD
    costs = costs / (1 + r)^(t - 2025), # discount them
    # in million tons
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100,
    # substract desalinated water (no water impact)
    water_impact = ktons_extracted * water_footprint / 1e6 - water_desalinated_million_m3 * aware_cf / 1e3 # billion m3
  ) |>
  group_by(Scenario, country) |>
  reframe(
    Copper = sum(copper),
    Nickel = sum(nickel),
    Cobalt = sum(cobalt),
    Lithium = sum(lithium),
    water_impact = sum(water_impact),
    costs = sum(costs)
  ) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()


# GDP share --------------
share_gdp <- read.csv("Parameters/GDP_Share_BatteryMinerals.csv")

prices <- read.csv("Parameters/MineralPrices2025.csv") # USD per ton
prices <- prices |> pivot_wider(names_from = Mineral, values_from = price_avg)
prod <- prod |>
  mutate(
    revenue = Copper * prices$Copper + Nickel * prices$Nickel + Cobalt * prices$Cobalt + Lithium * prices$Lithium,
    revenue = revenue / 1e3, # billion USD
    profit = revenue - costs
  )


# Figure ----------------------

## Cost optimal (reference) vs other scenarios ------------
data_fig <- prod %>%
  left_join(
    prod %>%
      filter(Scenario == "Reference") %>%
      select(country, ref_water = water_impact, ref_revenue = revenue, ref_profit = profit),
    by = "country"
  ) %>%
  mutate(
    delta_water = water_impact - ref_water,
    delta_revenue = revenue - ref_revenue,
    delta_profit = profit - ref_profit
  )

data_fig <- data_fig |>
  dplyr::select(Scenario, country, delta_water, delta_profit, delta_revenue) |>
  left_join(share_gdp)

# remove countries with zero change
data_fig <- data_fig |> filter(abs(delta_water) > 1e-3 | abs(delta_profit) > 1e-3)

data_fig <- data_fig |>
  mutate(
    share_gdp_bin = cut(
      gdp_share,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, 0.20, Inf),
      labels = c("<0.1%", "0.1–1%", "1–5%", "5–10%", "10–20%", ">20%"),
      right = FALSE
    )
  )

# Total change check
# minus 3000 billion m3
data_fig |> group_by(Scenario) |> reframe(x = sum(delta_water))

data_fig$Scenario <- recode(
  data_fig$Scenario,
  "5% Cost Increase" = "bold('5% Cost Increase')",
  "Fish Biodiversity < 70" = "bold(Fish~Index~'< 70')",
  "Water Desalination $0.5/m3" = "bold(Water~Desalination~'$0.5/m'^3)",
  "Water Desalination $0.5/m3 + Fish Biodiversity < 70" = "bold(Desal.~'$0.5/m'^3~'+'~Fish~Index~'< 70')"
)
data_fig$Scenario <- factor(
  data_fig$Scenario,
  levels = c(
    "bold('5% Cost Increase')",
    "bold(Fish~Index~'< 70')",
    "bold(Water~Desalination~'$0.5/m'^3)",
    "bold(Desal.~'$0.5/m'^3~'+'~Fish~Index~'< 70')"
  )
)
table(data_fig$Scenario)

# No text
data_fig_text <- data_fig |>
  filter(
    ((abs(delta_water) > 2 | abs(delta_water) > 4) & gdp_share > 0.01) | (abs(delta_water) > 3 | abs(delta_water) > 15)
  )

# Ranges scales
range(data_fig$delta_water)
range(data_fig$delta_water)
pseudo_log_breaks <- function(base = 10, symmetric = TRUE) {
  function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) {
      return(c(0))
    }

    r <- range(x, na.rm = TRUE)
    m <- if (symmetric) max(abs(r)) else max(abs(x))
    if (m <= 0) {
      return(c(0))
    }

    p <- floor(log(m, base = base))
    vals <- c(1, 3) * rep(base^(0:p), each = 2)
    b <- sort(unique(c(-vals, 0, vals)))

    if (symmetric) {
      b[b >= -m & b <= m]
    } else {
      b[b >= r[1] & b <= r[2]]
    }
  }
}

# global change
global_pt <- data_fig |>
  group_by(Scenario) |>
  summarise(water = sum(delta_water, na.rm = TRUE), profit = sum(delta_profit, na.rm = TRUE))


# Manual breaks
xb <- pseudo_log_breaks(symmetric = TRUE)(data_fig$delta_water)
yb <- pseudo_log_breaks(symmetric = TRUE)(data_fig$delta_profit)
xr <- range(data_fig$delta_water)
yr <- range(data_fig$delta_profit)

dx <- 0.1 # tick half-length (x units)
dy <- 0.1 # tick half-length (y units)


panel_labels <- data.frame(label = c("a", "b", "c", "d"), Scenario = unique(data_fig$Scenario))


text_font <- 7
ggplot(data_fig, aes(x = delta_water, y = delta_profit)) +
  facet_wrap(~Scenario, labeller = label_parsed) +
  geom_vline(xintercept = 0, col = "black", linewidth = 0.2) +
  # fmt: skip
  geom_segment(linewidth=0.2,data = data.frame(y = yb), aes(x = -dx, xend = dx, y = y, yend = y), inherit.aes = FALSE) +
  geom_text(
  data = data.frame(y = yb),
  aes(x = -2*dx, y = y,
      label = scales::dollar(y, accuracy = 1, big.mark = ",")),
  hjust = 1,
  size = text_font * 5 / 14 * 0.8,
  inherit.aes = FALSE
) +
  # fmt: skip
  annotate("text",x = max(abs(xr)),y = 3,label = "Delta~Freshwater~Impact",parse = T,hjust = 0.8,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  # fmt: skip
  annotate("text",x = max(abs(xr)),y = 1.5,label = "(billion~m^3*-eq)",parse = T,hjust = 0.8,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  geom_hline(yintercept = 0, col = "black", linewidth = 0.2) +
  # fmt: skip
  geom_segment(linewidth=0.2,data = data.frame(x = xb), aes(y = -dy, yend = dy, x = x, xend = x), inherit.aes = FALSE) +
  geom_text(
  data = data.frame(x = xb),
  aes(y = -2*dy, x = x,
      label = scales::comma(x)),
  vjust = 1,
  size = text_font * 5 / 14 * 0.8,
  inherit.aes = FALSE
) +
  # fmt: skip
  annotate("text",x = 0.5,y = -max(abs(yr)),label = "Delta~Profit",parse = T,angle = 90,hjust = 0,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  # fmt: skip
  annotate("text",x = 1.5,y = -max(abs(yr)),label = "(billion~USD)",parse = T,angle = 90,hjust = 0.2,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  geom_point(aes(size = gdp_share,fill=delta_profit),alpha=0.7,shape = 21, color = "black", stroke = 0.2) +
  geom_text_repel(
    data = data_fig_text,
    aes(label = country),
    size = text_font * 5 / 14 * 0.8,
    box.padding = 0.15,
    point.padding = 0.1,
    segment.size = 0.2,
    min.segment.length = 0.05,
    max.overlaps = Inf
  ) +
  geom_point(data = global_pt,aes(x = water, y = profit),inherit.aes = FALSE,color = "black",size = 2) +
  # fmt: skip
  geom_text(data = global_pt,aes(x = water, y = profit, label = "\u2206 Global"),nudge_y = c(0.2,0.2,-0.2,-0.2),hjust=0.2,inherit.aes = FALSE,size = text_font * 5 / 14 * 0.8) +
  # arrows
  # fmt: skip
  annotate("segment",col="#525252", x = 0, y = 600, xend = 1, yend = 600, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="#525252", x = 0, y = 600, xend = -1, yend = 600, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = 1.5, y = 600, label = "More water\nimpact", col="#525252",size = text_font * 5 / 14 * 0.8, hjust = 0) +
  # fmt: skip
  annotate("text", x = -1.5, y = 600, label = "Less water\nimpact", col="#525252",size = text_font * 5 / 14 * 0.8, hjust = 1) +
  # fmt: skip
  annotate("segment",col="#525252", x = -2100, y = 0, xend = -2100, yend = 1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="#525252", x = -2100, y = 0, xend = -2100, yend = -1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = -2100, y = 1.5, label = "More profit", col="#525252",size = text_font * 5 / 14 * 0.8, vjust = 0) +
  # fmt: skip
  annotate("text", x = -2100, y = -1.5, label = "Less profit", col="#525252",size = text_font * 5 / 14 * 0.8, vjust = 1) +
  # fmt: skip
  geom_text(data = panel_labels, aes(label = label), 
  x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2,
  fontface = "bold", size = 14 * 5 / 14 * 0.8,
  colour = "black", inherit.aes = F) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = pseudo_log_breaks(symmetric = T),
    limits = function(x) c(-max(abs(x)), max(abs(x))),
    labels = dollar_format(big.mark = ",", prefix = "$", accuracy = 1)
  ) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    limits = function(x) c(-max(abs(x)), max(abs(x))),
    breaks = pseudo_log_breaks(symmetric = T),
    labels = scales::label_comma()
  ) +
  scale_size_continuous(range = c(0.1, 8), breaks = c(0.01, 0.05, 0.1, 0.2), labels = scales::percent) +
  scale_fill_gradientn(
    colours = c(
      "#693829FF",
      "#894B33FF",
      "#A56A3EFF",
      "#CFB267FF",
      "#D9C5B6FF",
      "#9CA9BAFF",
      "#5480B5FF",
      "#3D619DFF",
      "#405A95FF",
      "#345084FF"
    ),
    limits = c(min(data_fig$delta_profit), max(data_fig$delta_profit)),
    values = scales::rescale(c(min(data_fig$delta_profit), 0, max(data_fig$delta_profit))),
  ) +
  # scale_fill_manual(values = rev(c("#171513ff", "#B45921FF", "#F49D63FF", "#FDC57AFF", "#FEEECFFF", "#F6F6F6FF"))) +
  labs(x = "", y = "", size = "Battery Minerals\nGDP share") +
  coord_cartesian(clip = "on") +
  theme_pb_large() +
  guides(fill = "none") +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.key.size = unit(0.3, "lines"),
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.line = element_blank(), # removes default bottom/left axes
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    # plot.background = element_blank(),
    panel.background = element_blank()
  )

# fmt: skip
ggsave("Figures/Fig3_Country.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2.1, height = 8.7*2.1)
ggsave("Figures/Fig3_Country.svg", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2.1, height = 8.7 * 2.1)
# ggsave("Figures/Fig3_Country_fish.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/Fig3_Country_fish_cost.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/Fig3_Country_Des.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
