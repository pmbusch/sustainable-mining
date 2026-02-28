# Country equity analysis
# Extraction, Water Impact, Demand
# PBH Jan 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = scens_selected, name = name_abbr))


# LOAD --------------

## Supply Results --------------

# Files with results
deposit <- read.csv("Parameters/Cu_Deposit.csv")
url_results <- "Results/Optimization/DemandScenario"
url_results <- "Results/Optimization/Copper"

(runs <- list.files(url_results, recursive = T, full.names = TRUE) |>
  keep(~ str_detect(basename(.x), "^(Base.*|Water.*)$") && !str_detect(basename(.x), "_")))


source("Scripts/01-LoadOptimizationResults.R", encoding = "UTF-8")

## Demand ----------

demand <- read.csv("Inputs/Busch2025/Cu_Demand_Duex_Region.csv")
demand <- demand |> filter(t >= 2025 & t <= 2050)
unique(demand$Region)
dict_region <- read.csv("Inputs/Dictionaries/Dict_Regions.csv")


# JOIN ---------
# aggregate by cpuntry and scneario - run , total extraction and total freshwater impact
supply <- df_results %>%
  left_join(deposit) %>%
  filter(t < 2051) %>%
  mutate(
    water_m3 = ktons_extracted * water_footprint / 1e3 # million m3
  ) |>
  mutate(name = Scenario) |>
  group_by(name, cost_deg, country) %>%
  reframe(mtons = sum(ktons_extracted) / 1e3, water_m3 = sum(water_m3) / 1e3) |> # water to billion
  ungroup() |>
  filter(mtons > 0)

# top c
supply |>
  filter(name == "Ambitious-Baseline-Baseline-Baseline-Baseline", cost_deg == 0) |>
  arrange(desc(water_m3)) |>
  pull(country) |>
  unique()

# add demand and agg to regions
supply <- supply |>
  mutate(country = recode(country, "Türkiye" = "Turkey")) |>
  left_join(dict_region) |>
  group_by(name, cost_deg, Region) |>
  reframe(mtons = sum(mtons), water_m3 = sum(water_m3)) |>
  ungroup() |>
  pivot_longer(c(mtons, water_m3), names_to = 'metric', values_to = 'value')

demand <- demand |>
  rename(name = Scenario) |>
  group_by(name, Region) |>
  reframe(value = sum(Demand)) |>
  ungroup() |>
  mutate(cost_deg = 0, metric = 'demand')

data_fig <- rbind(supply, demand)

# Pick key scenarios
unique(data_fig$name)

dict <- tibble(
  name = c(
    "Ambitious-Baseline-Baseline-Baseline-Baseline",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline",
    "Ambitious-Baseline-Baseline-Baseline-Enhanced recycling"
  ),
  abbr = c("Baseline", "High Demand", "Low Demand")
)

data_fig <- data_fig |>
  left_join(dict) |>
  filter(!is.na(abbr)) |>
  mutate(key = paste0(cost_deg, "_", abbr)) |>
  # filter(key %in% c("Ref._0", "Large LIB_0", "Small LIB_0", "Ref._1", "Ref._5"))
  # filter(name %in% c("Ref.", "Large LIB", "Small LIB"), cost_deg %in% c(0, 1, 5))
  filter(cost_deg %in% c(0, 1, 5))


# order
c_order <- data_fig |> filter(key == "0_Baseline", metric == "demand") |> arrange((value)) |> pull(Region) |> unique()
# add missing countries
c_order <- c(unique(demand$Region)[!(unique(demand$Region) %in% c_order)], c_order)
data_fig <- data_fig |> mutate(Region = factor(Region, levels = c_order))

# Bar Figure -----------

# Prepare for figure
data_fig2 <- data_fig |>
  filter(key %in% c("0_Baseline", "5_Baseline")) |>
  mutate(key = paste0(metric, "_", key)) |>
  filter(key != "demand_5_Baseline") |>
  # Labels for for countries
  mutate(
    countries = case_when(
      Region == "Other Latin America and Caribbean" & key == "water_m3_5_Baseline" ~ "Chile/Peru/Argentina",
      Region == "Other Asia Pacific" & key == "water_m3_5_Baseline" ~ "Kazakhstan/Mongolia/Uzbekistan",
      Region == "Mexico" & key == "water_m3_5_Baseline" ~ "Mex",
      Region == "China" & key == "demand_0_Baseline" ~ "China",
      Region == "United States" & key == "demand_0_Baseline" ~ "USA",
      Region == "European Union" & key == "demand_0_Baseline" ~ "European\nUnion",
      Region == "ASEAN" & key == "demand_0_Baseline" ~ "ASEAN",
      Region == "Africa" & key == "mtons_5_Baseline" ~ "Africa",
      T ~ ""
    )
  ) |>
  mutate(color_text = if_else(str_detect(countries, "Chile|Mongolia"), "special", "normal")) |>
  mutate(
    key = recode(
      key,
      "mtons_0_Baseline" = "Copper Extraction\n0% Cost Deg.",
      "mtons_5_Baseline" = "Copper Extraction\n5% Cost Deg.",
      "water_m3_0_Baseline" = "Freshwater Impact\n0% Cost Deg.",
      "water_m3_5_Baseline" = "Freshwater Impact\n5% Cost Deg.",
      "demand_0_Baseline" = "Copper Demand"
    ) |>
      factor(
        levels = rev(c(
          "Copper Demand",
          "Copper Extraction\n0% Cost Deg.",
          "Copper Extraction\n5% Cost Deg.",
          "Freshwater Impact\n0% Cost Deg.",
          "Freshwater Impact\n5% Cost Deg."
        ))
      )
  )


ggplot(data_fig2, aes(key, value, fill = Region)) +
  geom_col(position = "fill",col="black",linewidth=0.1) +
  # scale_color_manual(values = c("Baseline" = "#000000", "High Demand" = "#D62728", "Low Demand" = "#2CA02C")) +
  geom_text(aes(label = countries,col=color_text), position = position_fill(vjust = 0.5), size = 6 * 5 / 14 * 0.8) +
  coord_flip(expand = F) +
  scale_fill_manual(values = region_colors) +
  scale_color_manual(values = c("special" = "white", "normal" = "black"), guide = 'none') +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 3, byrow = T)) +
  labs(x = "", y = "Share by Region [%]", col = "", fill = "") +
  theme_bw(8) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    legend.text = element_text(size = 6)
  )

ggsave("Figures/CountryEquity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)

# Scatter Figure ------------

# Calculate shares
data_fig3 <- data_fig |>
  filter(key %in% c("0_Baseline", "5_Baseline")) |>
  filter(metric %in% c("water_m3", "demand")) |>
  group_by(metric, cost_deg) |>
  mutate(share = value / sum(value)) |>
  ungroup() |>
  dplyr::select(cost_deg, Region, metric, share) |>
  pivot_wider(names_from = metric, values_from = share, ) |>
  mutate(cost_deg = factor(cost_deg)) |>
  mutate(water_m3 = if_else(is.na(water_m3), 0, water_m3))

# fill demand
demand_fill <- data_fig3 |> filter(!is.na(demand)) |> dplyr::select(Region, demand) |> rename(demand_aux = demand)
data_fig3 <- data_fig3 |> left_join(demand_fill) |> mutate(demand = if_else(is.na(demand), demand_aux, demand))

ggplot(data_fig3, aes(water_m3, demand)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.3) +
  geom_line(aes(group = Region), col = "grey", linewidth = .2, alpha = .7) +
  geom_point(aes(col = cost_deg)) +
  geom_text_repel(data = filter(data_fig3, cost_deg == "0"), aes(label = Region), size = 7 * 5 / 14 * 0.8) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = c("5" = "#1f78b4", "0" = "#e31a1c")) +
  # fmt: skip
  annotate("text", x = 0.7, y = 0.06, label = "Cost Optimal Solution", size = 7 * 5 / 14 * 0.8, col="#e31a1c",hjust = 0) +
  # fmt: skip
  annotate("text", x = 0.5, y = 0.09, label = "Water Impact Minimization", size = 7 * 5 / 14 * 0.8, col="#1f78b4",hjust = 0) +
  labs(x = "Share of Global Freshwater Impact [%]", y = "Share of Global Demand[%]", col = "Cost Degradation (%)") +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("Figures/CountryEquity_points.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
