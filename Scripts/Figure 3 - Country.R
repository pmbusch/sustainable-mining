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
(runs <- list.files(file_path, recursive = T, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })())


# NZE for now
runs <- runs[str_detect(runs, "NZE\\/")]
# runs <- runs[str_detect(runs, "FI70|none")]

opt_results <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)

opt_results <- opt_results |> filter(ktons_extracted > 0 | capacity_added > 0 | mine_opened > 0) # reduce size
head(opt_results)
unique(opt_results$file_name)
opt_results <- opt_results |>
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = rev(c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")))
  )
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
    Basin_ID,
    OPEX_ore,
    CAPEX_opening,
    CAPEX_exp
  )

# Cost parameters
optInputs <- read.csv("Results/Optimization/DemandScenario/NZE/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value)) # 7%
# residual value
mine_life <- 15
fraction_not_recovered <- 0.2

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
    costs = (ktons_extracted * OPEX_ore / 1e3 + mine_opened * CAPEX_opening_adj + capacity_added * CAPEX_exp_adj / 1e3)/1e3, # billion USD
    costs = costs / (1 + r)^(t - 2025), # discount them
    # in million tons
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100,
    water_impact = ktons_extracted * water_footprint / 1e6 # billion m3
  ) |>
  group_by(Scenario, metric, country) |>
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


## Mineral prices (for revenue) ------
prices <- read.csv("Parameters/MineralPrices.csv") # USD per ton
prices <- prices |> pivot_wider(names_from = Mineral, values_from = price_avg)
prod <- prod |>
  mutate(
    revenue = Copper * prices$Copper + Nickel * prices$Nickel + Cobalt * prices$Cobalt + Lithium * prices$Lithium,
    revenue = revenue / 1e3, # billion USD
    profit = revenue - costs
  )

## GDP Share of revenue by country -------------
names(depositAll)
# ore_processed = tons ore processed in 2025
prod2025 <- depositAll |>
  mutate(
    revenue = 0 +
      prod2025_Copper * prices$Copper +
      prod2025_Nickel * prices$Nickel +
      prod2025_Cobalt * prices$Cobalt +
      prod2025_Lithium * prices$Lithium
  ) |>
  group_by(country) |>
  reframe(
    revenue_2025 = sum(revenue),
    prod2025_Copper = sum(prod2025_Copper),
    prod2025_Nickel = sum(prod2025_Nickel),
    prod2025_Cobalt = sum(prod2025_Cobalt),
    prod2025_Lithium = sum(prod2025_Lithium)
  ) |>
  ungroup()
head(prod2025)

# Check to USGS numbers
sum(prod2025$prod2025_Copper) / 1e6 # 20.17 Mt, USGS 23 Mt
sum(prod2025$prod2025_Nickel) / 1e6 # 1.1 Mt, USGS 3.9 Mt (difference as only considering class 1 nickel)
sum(prod2025$prod2025_Cobalt) / 1e6 # 0.22 Mt, USGS 0.31 Mt
sum(prod2025$prod2025_Lithium) / 1e6 # 0.24 Mt, USGS 0.29 Mt

## GDP data by country
gdp_raw <- readxl::read_xls("Inputs/Worldbank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_3.xls", sheet = "Data", skip = 3)

# in USD
gdp <- gdp_raw |>
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "GDP") |>
  mutate(Year = as.numeric(Year)) |>
  group_by(`Country Name`) |>
  filter(!is.na(GDP)) |>
  filter(Year == max(Year[Year <= 2025], na.rm = TRUE)) |>
  ungroup() |>
  select(country = `Country Name`, GDP)

setdiff(prod2025$country, gdp$country) # check if all countries in prod2025 are in gdp
# fix
gdp <- gdp %>%
  mutate(
    country = recode(
      country,
      "Bosnia and Herzegovina" = "Bosnia & Herzegovina",
      "Cote d'Ivoire" = "Côte d'Ivoire",
      "Congo, Dem. Rep." = "Dem. Rep. Congo",
      "Iran, Islamic Rep." = "Iran",
      "Kyrgyz Republic" = "Kyrgyzstan",
      "Lao PDR" = "Laos",
      "Russian Federation" = "Russia",
      "Turkiye" = "Türkiye",
      "United States" = "USA",
      "Venezuela, RB" = "Venezuela",
      "Viet Nam" = "Vietnam"
    )
  )

# Share
share_gdp <- prod2025 |> left_join(gdp) |> mutate(gdp_share = revenue_2025 / GDP)
share_gdp <- share_gdp |> dplyr::select(country, gdp_share)

# Figure ----------------------

## Cost optimal vs 5% cost degradation ------------
data_fig <- prod |>
  # filter(metric == "0%") |>
  # filter((Scenario == "none" & metric == "0%") | (Scenario == "FI70" & metric == "5%")) |>
  dplyr::select(metric, country, water_impact, profit, revenue) |>
  filter(metric %in% c("0%", "5%")) |>
  pivot_wider(names_from = metric, values_from = c("water_impact", "profit", "revenue"), values_fill = 0) |>
  mutate(
    water_impact_diff = `water_impact_5%` - `water_impact_0%`,
    profit_diff = `profit_5%` - `profit_0%`,
    revenue_diff = `revenue_5%` - `revenue_0%`
  )
# filter(Scenario %in% c("FI70", "none")) |>
# pivot_wider(names_from = Scenario, values_from = c("water_impact", "revenue"), values_fill = 0) |>
# mutate(water_impact_diff = water_impact_FI70 - water_impact_none, revenue_diff = revenue_FI70 - revenue_none)

data_fig <- data_fig |> dplyr::select(country, water_impact_diff, profit_diff, revenue_diff) |> left_join(share_gdp)

# remove countries with zero change
data_fig <- data_fig |> filter(abs(water_impact_diff) > 1e-3 | abs(profit_diff) > 1e-3)

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
sum(data_fig$water_impact_diff) # minus 3000 billion m3

# No text
data_fig_text <- data_fig |>
  filter(
    ((abs(water_impact_diff) > 2 | abs(profit_diff) > 4) & gdp_share > 0.01) |
      (abs(water_impact_diff) > 3 | abs(profit_diff) > 15)
  )

# Ranges scales
range(data_fig$profit_diff)
range(data_fig$water_impact_diff)
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

# global average change
global_pt <- data_fig |>
  summarise(water = mean(water_impact_diff, na.rm = TRUE), profit = mean(profit_diff, na.rm = TRUE))

ggplot(data_fig, aes(x = water_impact_diff, y = profit_diff)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgrey", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "darkgrey", linewidth = 0.3) +
  geom_point(aes(size = gdp_share,fill=profit_diff),alpha=0.7,
             shape = 21, color = "black", stroke = 0.2) +
  geom_text_repel(
    data = data_fig_text,
    aes(label = country),
    size = 6 * 5 / 14 * 0.8,
    box.padding = 0.15,
    point.padding = 0.1,
    segment.size = 0.2,
    min.segment.length = 0.05,
    max.overlaps = Inf
  ) +
  geom_point(data = global_pt,aes(x = water, y = profit),inherit.aes = FALSE,color = "black",size = 1) +
  geom_text_repel(
    data = global_pt,
    aes(x = water, y = profit, label = "Global avg."),
    inherit.aes = FALSE,
    fill = "white",
    label.size = 0.2,
    size = 6 * 5 / 14 * 0.8
  ) +
  # arrows
  # fmt: skip
  annotate("segment",col="darkgrey", x = 0, y = -300, xend = 1, yend = -300, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="darkgrey", x = 0, y = -300, xend = -1, yend = -300, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = 1.5, y = -300, label = "More water\nimpact", col="darkgrey",size = 6 * 5 / 14 * 0.8, hjust = 0) +
  # fmt: skip
  annotate("text", x = -1.5, y = -300, label = "Less water\nimpact", col="darkgrey",size = 6 * 5 / 14 * 0.8, hjust = 1) +
  # fmt: skip
  annotate("segment",col="darkgrey", x = -1000, y = 0, xend = -1000, yend = 1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("segment",col="darkgrey", x = -1000, y = 0, xend = -1000, yend = -1, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = -1000, y = 1.5, label = "More profit", col="darkgrey",size = 6 * 5 / 14 * 0.8, vjust = 0) +
  # fmt: skip
  annotate("text", x = -1000, y = -1.5, label = "Less profit", col="darkgrey",size = 6 * 5 / 14 * 0.8, vjust = 1) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = pseudo_log_breaks(symmetric = F),
    # limits = function(x) c(-max(abs(x)), max(abs(x))),
    labels = dollar_format(big.mark = ",", prefix = "$", accuracy = 1)
  ) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    # limits = function(x) c(-max(abs(x)), max(abs(x))),
    breaks = pseudo_log_breaks(symmetric = F),
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
    values = scales::rescale(c(min(data_fig$profit_diff), 0, max(data_fig$profit_diff))),
  ) +
  # scale_fill_manual(values = rev(c("#171513ff", "#B45921FF", "#F49D63FF", "#FDC57AFF", "#FEEECFFF", "#F6F6F6FF"))) +
  labs(
    y = "Change in Profit 2025-2050 (billion USD)",
    x = expression("Change in Freshwater Impact 2025-2050 (billion " ~ m^3 * "-eq)"),
    size = "Battery Minerals\nGDP share",
    title = "Cost Optimal vs 5% Cost Increase",
    # title = "Cost optimal vs Basins Fish Index < 70 & 5% Cost Increase"
  ) +
  theme_pb_wide() +
  guides(fill = "none") +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.key.size = unit(0.3, "lines"),
    plot.title = element_text(size = 8, hjust = 0.5)
  )

# fmt: skip
ggsave("Figures/Fig3_Country.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/Fig3_Country_fish.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/Fig3_Country_fish_cost.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
