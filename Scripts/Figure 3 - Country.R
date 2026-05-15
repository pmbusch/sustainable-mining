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

dict_regions <- read.csv("Inputs/Dictionaries/Dict_Countries_Fig3.csv")

region_colors_broad <- c(
  "Latin America" = "#6a3d9a",
  "North America" = "#33a02c",
  "Europe" = "#1f78b4",
  "Asia & Oceania" = "#d74c5a",
  "Middle East & Africa" = "#8b4513",
  "World" = "#252525"
)

data_fig <- data_fig |>
  dplyr::select(Scenario, country, delta_water, delta_profit, delta_revenue) |>
  left_join(share_gdp) |>
  left_join(dict_regions) |>
  # fix some country names endocidng
  mutate(
    Region = case_when(
      !is.na(Region) ~ Region,
      str_detect(country, "Ivoire") ~ "Middle East & Africa",
      str_detect(country, "rkiye") ~ "Europe",
      T ~ "NA"
    )
  )

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
  "Fish Biodiversity < 70" = "bold('+5% Cost + Protect Fish-Rich Basins > 70')",
  "Water Desalination $0.5/m3" = "bold('+5% Cost + Water Desalination at $0.5/m'^3)",
  "Water Desalination $0.5/m3 + Fish Biodiversity < 70" = "bold('+5% Cost + Desal. $0.5/m'^3~'+ Protect Fish > 70')"
)

data_fig$Scenario <- factor(
  data_fig$Scenario,
  levels = c(
    "bold('5% Cost Increase')",
    "bold('+5% Cost + Protect Fish-Rich Basins > 70')",
    "bold('+5% Cost + Water Desalination at $0.5/m'^3)",
    "bold('+5% Cost + Desal. $0.5/m'^3~'+ Protect Fish > 70')"
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
range(data_fig$delta_profit)
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

# regional and global aggregates for arrows
region_agg <- data_fig |>
  group_by(Scenario, Region) |>
  summarise(water = sum(delta_water, na.rm = TRUE), profit = sum(delta_profit, na.rm = TRUE), .groups = "drop")
global_agg <- data_fig |>
  group_by(Scenario) |>
  summarise(water = sum(delta_water, na.rm = TRUE), profit = sum(delta_profit, na.rm = TRUE), .groups = "drop")
region_agg <- rbind(region_agg, global_agg |> mutate(Region = "World"))

# text placement
region_agg <- region_agg %>%
  mutate(
    label_vjust = case_when(
      str_detect(Scenario, "5% Cost") & Region %in% c("Middle East & Africa", "Europe", "World") ~ 1,
      str_detect(Scenario, "Fish-Rich") & Region %in% c("Latin America", "North America", "Asia & Oceania") ~ 1,
      str_detect(Scenario, "Desalination") & Region %in% c("Middle East & Africa", "Latin America") ~ 1,
      str_detect(Scenario, "Desal\\.") & Region %in% c("Middle East & Africa", "Latin America") ~ 1,
      TRUE ~ 0
    )
  )


# Manual breaks
xb <- pseudo_log_breaks(symmetric = TRUE)(data_fig$delta_water)
yb <- pseudo_log_breaks(symmetric = TRUE)(data_fig$delta_profit)
xr <- range(data_fig$delta_water)
yr <- range(data_fig$delta_profit)

dx <- 0.1 # tick half-length (x units)
dy <- 0.1 # tick half-length (y units)


scen_levels <- c(
  "bold('5% Cost Increase')",
  "bold('+5% Cost + Protect Fish-Rich Basins > 70')",
  "bold('+5% Cost + Water Desalination at $0.5/m'^3)",
  "bold('+5% Cost + Desal. $0.5/m'^3~'+ Protect Fish > 70')"
)
data_fig$Scenario <- factor(data_fig$Scenario, levels = scen_levels)
region_agg$Scenario <- factor(region_agg$Scenario, levels = scen_levels)

panel_labels <- data.frame(label = c("a", "b", "c", "d"), Scenario = scen_levels)

text_font <- 7
ggplot(data_fig, aes(x = delta_water, y = delta_profit)) +
  facet_wrap(~Scenario, labeller = label_parsed, ncol = 2, dir = "br") +
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
  annotate("text",x = max(abs(xr)),y = 3,label = "Delta~Scarce~Water~Use",parse = T,hjust = 0.7,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  # fmt: skip
  annotate("text",x = max(abs(xr)),y = 1.5,label = "(billion~m^3*-eq)",parse = T,hjust = 0.7,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
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
  annotate("text",x = 0.5,y = -max(abs(yr)),label = "Delta~Profit",parse = T,angle = 90,hjust = 0.3,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  # fmt: skip
  annotate("text",x = 1.5,y = -max(abs(yr)),label = "(billion~USD)",parse = T,angle = 90,hjust = 0.4,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  geom_point(aes(size = gdp_share, fill = Region), alpha = 0.7, shape = 21, color = "black", stroke = 0.2) +
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
  geom_segment(
    data = region_agg,
    aes(x = 0, y = 0, xend = water, yend = profit, color = Region),
    alpha = 0.5,
    linewidth = 0.35,
    arrow = arrow(length = unit(0.08, "cm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  geomtextpath::geom_textsegment(
    data = region_agg,
    aes(x = 0, y = 0, xend = water, yend = profit, color = Region, label = as.character(Region), vjust = label_vjust),
    text_only = TRUE,
    size = text_font * 5 / 14 * 0.8,
    hjust = 0.85, # 1 = end, adjust to taste
    inherit.aes = FALSE
  ) +
  # arrows
  # fmt: skip
  annotate("segment",col="#525252", x = 0, y = 600, xend = 1, yend = 600, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="#525252", x = 0, y = 600, xend = -1, yend = 600, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = 1.5, y = 600, label = "More water\nfootprint", col="#525252",size = text_font * 5 / 14 * 0.8, hjust = 0) +
  # fmt: skip
  annotate("text", x = -1.5, y = 600, label = "Less water\nfootprint", col="#525252",size = text_font * 5 / 14 * 0.8, hjust = 1) +
  # fmt: skip
  annotate("segment",col="#525252", x = -2000, y = 0, xend = -2000, yend = 1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="#525252", x = -2000, y = 0, xend = -2000, yend = -1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = -2000, y = 1.5, label = "More profit", col="#525252",size = text_font * 5 / 14 * 0.8, vjust = 0) +
  # fmt: skip
  annotate("text", x = -2000, y = -1.5, label = "Less profit", col="#525252",size = text_font * 5 / 14 * 0.8, vjust = 1) +
  # fmt: skip
  geom_text(data = panel_labels, aes(label = label),
  x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,
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
  scale_fill_manual(values = region_colors_broad, na.value = "#808080") +
  scale_color_manual(values = region_colors_broad, na.value = "#808080", guide = "none") +
  labs(x = "", y = "", size = "Battery Minerals\nGDP share", fill = "Region") +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  guides(fill = "none", color = "none", size = guide_legend()) +
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
ggsave("Figures/Figure3.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 18, height = 18)
# fmt: skip
ggsave("Figures/Figure3.svg", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 18, height = 18)

# Option B - equal size ------------------------

data_fig <- data_fig |> mutate(alpha_mapped = pmin(gdp_share, 0.30))
p2 <- ggplot(data_fig, aes(x = delta_water, y = delta_profit)) +
  facet_wrap(~Scenario, labeller = label_parsed, ncol = 2, dir = "br") +
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
  annotate("text",x = max(abs(xr)),y = 3,label = "Delta~Scarce~Water~Use",parse = T,hjust = 0.7,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  # fmt: skip
  annotate("text",x = max(abs(xr)),y = 1.5,label = "(billion~m^3*-eq)",parse = T,hjust = 0.7,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
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
  annotate("text",x = 0.5,y = -max(abs(yr)),label = "Delta~Profit",parse = T,angle = 90,hjust = 0.3,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  # fmt: skip
  annotate("text",x = 1.5,y = -max(abs(yr)),label = "(billion~USD)",parse = T,angle = 90,hjust = 0.4,vjust = 1,size = (text_font+1) * 5 / 14 * 0.8) +
  geom_point(aes(fill=Region,alpha = alpha_mapped), shape = 21, color = "black", stroke = 0.2) +
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
  geom_segment(
    data = region_agg,
    aes(x = 0, y = 0, xend = water, yend = profit, color = Region),
    alpha = 0.5,
    linewidth = 0.35,
    arrow = arrow(length = unit(0.08, "cm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  geomtextpath::geom_textsegment(
    data = region_agg,
    aes(x = 0, y = 0, xend = water, yend = profit, color = Region, label = as.character(Region), vjust = label_vjust),
    text_only = TRUE,
    size = text_font * 5 / 14 * 0.8,
    hjust = 0.85, # 1 = end, adjust to taste
    inherit.aes = FALSE
  ) +
  # arrows
  # fmt: skip
  annotate("segment",col="#525252", x = 0, y = 600, xend = 1, yend = 600, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="#525252", x = 0, y = 600, xend = -1, yend = 600, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = 1.5, y = 600, label = "More water\nfootprint", col="#525252",size = text_font * 5 / 14 * 0.8, hjust = 0) +
  # fmt: skip
  annotate("text", x = -1.5, y = 600, label = "Less water\nfootprint", col="#525252",size = text_font * 5 / 14 * 0.8, hjust = 1) +
  # fmt: skip
  annotate("segment",col="#525252", x = -2000, y = 0, xend = -2000, yend = 1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="#525252", x = -2000, y = 0, xend = -2000, yend = -1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = -2000, y = 1.5, label = "More profit", col="#525252",size = text_font * 5 / 14 * 0.8, vjust = 0) +
  # fmt: skip
  annotate("text", x = -2000, y = -1.5, label = "Less profit", col="#525252",size = text_font * 5 / 14 * 0.8, vjust = 1) +
  # fmt: skip
  geom_text(data = panel_labels, aes(label = label),
  x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,
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
  scale_fill_manual(values = region_colors_broad, na.value = "#808080") +
  scale_alpha_continuous(range = c(1, 0.1), labels = scales::percent, trans = scales::exp_trans(0.8)) +
  scale_color_manual(values = region_colors_broad, na.value = "#808080", guide = "none") +
  labs(x = "", y = "", size = "Battery Minerals\nGDP share", fill = "Region") +
  coord_cartesian(clip = "off") +
  theme_pb_large() +
  guides(color = "none", alpha = guide_legend()) +
  theme(
    legend.position = "none",
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
p2

# custom legend
library(patchwork)

# Create continuous gradient data
region_levels <- names(region_colors_broad)[1:5]

legend_data <- expand.grid(
  Region = factor(region_levels, levels = region_levels),
  gdp_share = seq(0.001, 0.30, length.out = 200)
) |>
  mutate(region_int = as.integer(Region)) |>
  group_by(Region) |>
  mutate(xmin = gdp_share, xmax = lead(gdp_share, default = 0.40)) |>
  ungroup()

outline_data <- data.frame(
  Region = factor(region_levels, levels = region_levels),
  region_int = seq_along(region_levels)
)

legend_plot <- ggplot() +
  geom_rect(
    data = legend_data,
    aes(xmin = xmin, xmax = xmax, ymin = region_int - 0.5, ymax = region_int + 0.5, fill = Region, alpha = gdp_share),
    color = NA
  ) +
  geom_rect(
    data = outline_data,
    aes(ymin = region_int - 0.5, ymax = region_int + 0.5),
    xmin = 0,
    xmax = 0.40,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    data = outline_data,
    aes(
      x     = 0.01,
      y     = region_int,
      label = Region,
      color = Region
    ),
    hjust  = 0,
    size   = 7 * 5 / 14 * 0.8,
    fontface = "bold"
  ) +
  scale_fill_manual(values = region_colors_broad) +
  scale_color_manual(values = region_colors_broad) +
  scale_alpha_continuous(range = c(1, 0.1), trans = scales::exp_trans(0.8), labels = scales::percent) +
  scale_x_continuous(
    labels = scales::percent,
    breaks = c(0, 0.10, 0.20, 0.30),
    limits = c(0, 0.30),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Battery Minerals GDP Share", y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x.top = element_text(size = 7, margin = margin(b = -1)),
    axis.title.x.top = element_text(size = 7),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.top = element_line(size = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid = element_blank()
  )


p2 + inset_element(legend_plot, left = 0.8, bottom = 0.0, right = 1.0, top = 0.2)

# fmt: skip
ggsave("Figures/Figure3_option2.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 18, height = 18)
# fmt: skip
ggsave("Figures/Figure3_option2.svg", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 18, height = 18)
