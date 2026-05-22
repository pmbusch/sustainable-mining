# ============================================================
# Standalone production composition figure - Demand scenario
# NZE only
# PBH Mar 2026
# ============================================================

### --------------------
# LOAD ------------------
# -----------------------

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

demand <- read.csv("Parameters/IEA_Demand.csv")
dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")
prod <- read.csv("Results/Processed/prod_country_demand.csv")

# --------------------------------------------
# NZE DEMAND -------------------------------------------------------------
# ---------------------------------

runs <- list.files("Results/Optimization/DemandScenario", pattern = "Metrics.*", recursive = TRUE, full.names = TRUE)

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

obj <- obj |>
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

impact <- obj |>
  dplyr::select(-Units) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(Water_Impact = `Water impact` / 1e3) |>
  group_by(Scenario, metric) |>
  reframe(Water_Impact = mean(Water_Impact)) |>
  ungroup()

data_fig <- prod |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario == "NZE", metric != "No Water Constraint") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "total_metal") |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

demand_long <- demand |>
  filter(Scenario == "NZE") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "total_demand") |>
  group_by(Mineral) |>
  reframe(total_demand = sum(total_demand) / 1e3) |>
  ungroup()

unmet <- data_fig |>
  group_by(Scenario, metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  left_join(demand_long, by = "Mineral") |>
  mutate(unmet_demand = total_demand - total_metal) |>
  dplyr::select(-total_metal, -total_demand) |>
  filter(unmet_demand > 1e-3) |>
  rename(total_metal = unmet_demand) |>
  mutate(country = "Unmet Demand")

data_fig <- data_fig |>
  rbind(unmet) |>
  left_join(dict_region, by = c("country")) |>
  mutate(
    region = case_when(
      country == "Unmet Demand" ~ "Unmet Demand",
      Mineral == "Copper" &
        country %in%
          c("Chile", "Peru", "Indonesia", "Russia", "USA", "China", "Mongolia", "Dem. Rep. Congo", "Mexico") ~ ISO3,
      Mineral == "Nickel" &
        country %in% c("Indonesia", "Philippines", "Russia", "Australia", "New Caledonia", "Brazil") ~ ISO3,
      Mineral == "Cobalt" & country %in% c("Dem. Rep. Congo", "Indonesia") ~ ISO3,
      Mineral == "Lithium" &
        country %in% c("Chile", "Dem. Rep. Congo", "Argentina", "USA", "Australia", "Brazil") ~ ISO3,
      TRUE ~ "RoW"
    ) |>
      str_replace("COD", "DRC")
  ) |>
  group_by(Scenario, metric, region, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric, Mineral) |>
  mutate(share = total_metal / sum(total_metal)) |>
  ungroup() |>
  left_join(impact, by = c("Scenario", "metric"))

region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CAN" = "#e91a1c",
  "CHN" = "#d74c5a",
  "ARG" = "#ff7f00",
  "MNG" = "#e31a1c",
  "PHL" = "#1f78b4",
  "BRA" = "#33a02c",
  "NCL" = "#b15928",
  "AUS" = "#fb9a99",
  "Europe" = "#2b8cbe",
  "MEX" = "#66c2a5",
  "KAZ" = "#8c510a",
  "RoW" = "#4d4d4d",
  "Unmet Demand" = "#67000D80"
)

data_fig <- data_fig |>
  mutate(region = factor(region, levels = rev(names(region_colors)))) |>
  mutate(region_label = if_else(share > 0.01, as.character(region), ""))

order_col <- data_fig |>
  mutate(order_col = paste0(Mineral, region)) |>
  arrange(desc(share)) |>
  pull(order_col) |>
  unique()

order_col <- c(order_col[!str_detect(order_col, "RoW")], order_col[str_detect(order_col, "RoW")])
order_col <- c(order_col[!str_detect(order_col, "Unmet Demand")], order_col[str_detect(order_col, "Unmet Demand")])

data_fig <- data_fig |> mutate(order_col = paste0(Mineral, region) |> factor(levels = rev(order_col)))

write.csv(data_fig, "Figures/Data_Figures/FigProd_Demand.csv", row.names = FALSE)

p_prod_demand <- ggplot(data_fig, aes(Water_Impact, total_metal, fill = region, group = order_col)) +
  geom_area(col = "black", linewidth = 0.1) +
  geom_text(
    data = filter(data_fig, metric == "0%"),
    aes(x = Water_Impact - 100, label = region_label),
    hjust = 1,
    position = position_stack(vjust = 0.5),
    size = 7 * 5 / 14 * 0.8,
    col = "white"
  ) +
  facet_wrap(~Mineral, ncol = 2, scales = "free") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = ~ scales::comma(. / 1e3)) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = expression("Scarce Water Use (trillion " ~ m^3 * "-eq)"),
    y = "Production 2025-2050 (million tons)",
    subtitle = "Net Zero Emissions Demand Scenario"
  ) +
  theme_pb_wide() +
  theme(legend.position = "none", panel.spacing.x = unit(1.2, "lines"))
p_prod_demand

# fmt: skip
ggsave("Figures/ExtData-Figures/Fig_Production_Demand.png",p_prod_demand,units = "cm",dpi = 600,width = 8.7,height = 8.7)
# fmt: skip
ggsave("Figures/ExtData-Figures/Fig_Production_Demand.svg",p_prod_demand,units = "cm",dpi = 600,width = 8.7,height = 8.7)


# ============================================================
# FISH INDEX ---------------------------------
# Standalone production composition figure - Biodiversity
# Fish Index < 70 only
# PBH Mar 2026
# ============================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")


# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

demand <- read.csv("Parameters/IEA_Demand.csv")
dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")
prod <- read.csv("Results/Processed/prod_country_biod.csv")

runs <- list.files("Results/Optimization/BioDScenario/NZE/", pattern = "Metrics.*", recursive = TRUE, full.names = TRUE)

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

obj <- obj |>
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

impact <- obj |>
  dplyr::select(-Units) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(Water_Impact = `Water impact` / 1e3) |>
  group_by(Scenario, metric) |>
  reframe(Water_Impact = mean(Water_Impact)) |>
  ungroup()

data_fig <- prod |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario == "Fish Index < 70", metric != "No Water Constraint") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "total_metal") |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

demand_long <- demand |>
  filter(Scenario == "NZE") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "total_demand") |>
  group_by(Mineral) |>
  reframe(total_demand = sum(total_demand) / 1e3) |>
  ungroup()

unmet <- data_fig |>
  group_by(Scenario, metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  left_join(demand_long, by = "Mineral") |>
  mutate(unmet_demand = total_demand - total_metal) |>
  dplyr::select(-total_metal, -total_demand) |>
  filter(unmet_demand > 1e-3) |>
  rename(total_metal = unmet_demand) |>
  mutate(country = "Unmet Demand")

data_fig <- data_fig |>
  rbind(unmet) |>
  left_join(dict_region, by = c("country")) |>
  mutate(
    region = case_when(
      country == "Unmet Demand" ~ "Unmet Demand",
      Mineral == "Copper" &
        country %in%
          c(
            "Chile",
            "Peru",
            "Indonesia",
            "Russia",
            "USA",
            "China",
            "Mongolia",
            "Dem. Rep. Congo",
            "Mexico",
            "Kazakhstan"
          ) ~ ISO3,
      Mineral == "Nickel" &
        country %in%
          c("Indonesia", "Philippines", "Russia", "Australia", "New Caledonia", "Brazil", "USA", "Canada") ~ ISO3,
      Mineral == "Cobalt" & country %in% c("Dem. Rep. Congo", "Indonesia", "Australia", "USA", "Philippines") ~ ISO3,
      Mineral == "Lithium" &
        country %in%
          c("Chile", "Dem. Rep. Congo", "Argentina", "USA", "Australia", "Brazil", "Canada", "Mexico") ~ ISO3,
      TRUE ~ "RoW"
    ) |>
      str_replace("COD", "DRC")
  ) |>
  group_by(Scenario, metric, region, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric, Mineral) |>
  mutate(share = total_metal / sum(total_metal)) |>
  ungroup() |>
  left_join(impact, by = c("Scenario", "metric"))

region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CAN" = "#e91a1c",
  "CHN" = "#d74c5a",
  "ARG" = "#ff7f00",
  "MNG" = "#e31a1c",
  "PHL" = "#1f78b4",
  "BRA" = "#33a02c",
  "NCL" = "#b15928",
  "AUS" = "#fb9a99",
  "Europe" = "#2b8cbe",
  "MEX" = "#66c2a5",
  "KAZ" = "#8c510a",
  "RoW" = "#4d4d4d",
  "Unmet Demand" = "#67000D80"
)

data_fig <- data_fig |>
  mutate(region = factor(region, levels = rev(names(region_colors)))) |>
  mutate(region_label = if_else(share > 0.01, as.character(region), ""))

order_col <- data_fig |>
  mutate(order_col = paste0(Mineral, region)) |>
  arrange(desc(share)) |>
  pull(order_col) |>
  unique()

order_col <- c(order_col[!str_detect(order_col, "RoW")], order_col[str_detect(order_col, "RoW")])
order_col <- c(order_col[!str_detect(order_col, "Unmet Demand")], order_col[str_detect(order_col, "Unmet Demand")])

data_fig <- data_fig |> mutate(order_col = paste0(Mineral, region) |> factor(levels = rev(order_col)))

write.csv(data_fig, "Figures/Data_Figures/FigProd_Biodiversity.csv", row.names = FALSE)

p_prod_biod <- ggplot(data_fig, aes(Water_Impact, total_metal, fill = region, group = order_col)) +
  geom_area(col = "black", linewidth = 0.1) +
  geom_text(
    data = filter(data_fig, metric == "0%"),
    aes(x = Water_Impact - 100, label = region_label),
    hjust = 1,
    position = position_stack(vjust = 0.5),
    size = 7 * 5 / 14 * 0.8,
    col = "white"
  ) +
  facet_wrap(~Mineral, ncol = 2, scales = "free") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = ~ scales::comma(. / 1e3)) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = expression("Scarce Water Use (trillion " ~ m^3 * "-eq)"),
    y = "Production 2025-2050 (million tons)",
    subtitle = "Net Zero Emissions Demand Scenario\nIncluding only basins with Fish Index < 70"
  ) +
  theme_pb_wide() +
  theme(legend.position = "none", panel.spacing.x = unit(1.2, "lines"))
p_prod_biod

# fmt: skip
ggsave("Figures/ExtData-Figures/Fig_Production_Biodiversity.png", p_prod_biod, units = "cm", dpi = 600, width = 8.7, height = 8.7)
# fmt: skip
ggsave("Figures/ExtData-Figures/Fig_Production_Biodiversity.svg", p_prod_biod, units = "cm", dpi = 600, width = 8.7, height = 8.7)


# COST DESALINATION ---------------------------------
# Standalone production composition figure - Cost desalination
# Cost desalination at $0.5/m3
# PBH Mar 2026
# ============================================================

# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

demand <- read.csv("Parameters/IEA_Demand.csv")
dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")
prod <- read.csv("Results/Processed/prod_country_CostDes.csv")

runs <- list.files(
  "Results/Optimization/DesCostScenario/NZE/FI100",
  pattern = "Metrics.*",
  recursive = TRUE,
  full.names = TRUE
)

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

obj <- obj |>
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
table(obj$Scenario)

impact <- obj |>
  dplyr::select(-Units) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(Water_Impact = `Water impact` / 1e3) |>
  group_by(Scenario, metric) |>
  reframe(Water_Impact = mean(Water_Impact)) |>
  ungroup()

data_fig <- prod |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario == "$0.5/m3", metric != "No Water Constraint") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "total_metal") |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

demand_long <- demand |>
  filter(Scenario == "NZE") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "total_demand") |>
  group_by(Mineral) |>
  reframe(total_demand = sum(total_demand) / 1e3) |>
  ungroup()

unmet <- data_fig |>
  group_by(Scenario, metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  left_join(demand_long, by = "Mineral") |>
  mutate(unmet_demand = total_demand - total_metal) |>
  dplyr::select(-total_metal, -total_demand) |>
  filter(unmet_demand > 1e-3) |>
  rename(total_metal = unmet_demand) |>
  mutate(country = "Unmet Demand")

data_fig <- data_fig |>
  rbind(unmet) |>
  left_join(dict_region, by = c("country")) |>
  mutate(
    region = case_when(
      country == "Unmet Demand" ~ "Unmet Demand",
      Mineral == "Copper" &
        country %in%
          c(
            "Chile",
            "Peru",
            "Indonesia",
            "Russia",
            "USA",
            "China",
            "Mongolia",
            "Dem. Rep. Congo",
            "Mexico",
            "Kazakhstan"
          ) ~ ISO3,
      Mineral == "Nickel" &
        country %in%
          c("Indonesia", "Philippines", "Russia", "Australia", "New Caledonia", "Brazil", "USA", "Canada") ~ ISO3,
      Mineral == "Cobalt" & country %in% c("Dem. Rep. Congo", "Indonesia", "Australia", "USA", "Philippines") ~ ISO3,
      Mineral == "Lithium" &
        country %in%
          c("Chile", "Dem. Rep. Congo", "Argentina", "USA", "Australia", "Brazil", "Canada", "Mexico") ~ ISO3,
      TRUE ~ "RoW"
    ) |>
      str_replace("COD", "DRC")
  ) |>
  group_by(Scenario, metric, region, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric, Mineral) |>
  mutate(share = total_metal / sum(total_metal)) |>
  ungroup()

data_fig <- data_fig |> left_join(impact, by = c("Scenario", "metric"))

region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CAN" = "#e91a1c",
  "CHN" = "#d74c5a",
  "ARG" = "#ff7f00",
  "MNG" = "#e31a1c",
  "PHL" = "#1f78b4",
  "BRA" = "#33a02c",
  "NCL" = "#b15928",
  "AUS" = "#fb9a99",
  "Europe" = "#2b8cbe",
  "MEX" = "#66c2a5",
  "KAZ" = "#8c510a",
  "RoW" = "#4d4d4d",
  "Unmet Demand" = "#67000D80"
)

data_fig <- data_fig |>
  mutate(region = factor(region, levels = rev(names(region_colors)))) |>
  mutate(region_label = if_else(share > 0.01, as.character(region), ""))

order_col <- data_fig |>
  mutate(order_col = paste0(Mineral, region)) |>
  arrange(desc(share)) |>
  pull(order_col) |>
  unique()

order_col <- c(order_col[!str_detect(order_col, "RoW")], order_col[str_detect(order_col, "RoW")])
order_col <- c(order_col[!str_detect(order_col, "Unmet Demand")], order_col[str_detect(order_col, "Unmet Demand")])

data_fig <- data_fig |> mutate(order_col = paste0(Mineral, region) |> factor(levels = rev(order_col)))

write.csv(data_fig, "Figures/Data_Figures/FigProd_CostDes.csv", row.names = FALSE)

p_prod_CostDes <- ggplot(data_fig, aes(Water_Impact, total_metal, fill = region, group = order_col)) +
  geom_area(col = "black", linewidth = 0.1) +
  geom_text(
    data = filter(data_fig, metric == "0%"),
    aes(x = Water_Impact - 100, label = region_label),
    hjust = 1,
    position = position_stack(vjust = 0.5),
    size = 7 * 5 / 14 * 0.8,
    col = "white"
  ) +
  facet_wrap(~Mineral, ncol = 2, scales = "free") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma, breaks = seq(2000, 5000, 1000)) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = expression("Total Freshwater Impact (billion " ~ m^3 * "-eq)"),
    y = "",
    title = "2025-2050 Metal production, in million tons",
    subtitle = bquote("Water desalination available at $0.5 per" ~ m^3 * "")
  ) +
  theme_pb_wide() +
  theme(legend.position = "none", panel.spacing.x = unit(1.2, "lines"))
p_prod_CostDes

ggsave("Figures/SI/Fig_Production_CostDes.png", p_prod_CostDes, units = "cm", dpi = 600, width = 8.7, height = 8.7)
