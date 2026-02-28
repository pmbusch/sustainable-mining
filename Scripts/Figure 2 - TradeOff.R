# Trade off cost and water
# PBH Nov 2025

# Load -------------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = Scenario, name = scen_name))


demand <- read.csv("Parameters/IEA_Demand.csv")
deposit <- read.csv("Parameters/Deposit.csv")

# Demand totals
(dem_tot <- demand |>
  pivot_longer(c(-Scenario, -Year), names_to = 'Mineral', values_to = 'Demand') |>
  group_by(Scenario, Mineral) |>
  reframe(mtons = sum(Demand) / 1e3) |>
  ungroup() |>
  mutate(label_dem = paste0(Scenario, " ", round(mtons, 0), " Mt")))


# Demand Curve --------------------
# Store results from optimization run
(runs <- list.files("Results/Optimization/DemandScenario", pattern = "Metrics.*", recursive = T, full.names = TRUE))
# (runs <- list.files(
#   "Results/Optimization/ClimateScenario/APS/Deposit_ssp370_gfdl-esm4",
#   pattern = "Metrics.*",
#   recursive = T,
#   full.names = TRUE
# ))

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)
head(obj)
unique(obj$file_name)
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
  )

table(obj$Scenario)
table(obj$metric)
table(obj$Parameter)

df <- obj |>
  filter(!is.na(Scenario)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  # left_join(dem_tot) |>
  mutate(
    Water_cons = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world equiv.
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  ) |>
  mutate(`Water impact` = NULL)

range(df$Water_Impact)


# note that slope is USD/m3 of water saved
# fit logaritmic model to data
# Y=a+b*log(X)
# Slope of Y: dY/dX = b/X

library(broom)
fits <- df %>%
  group_by(Scenario) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Cost ~ log(Water_Impact), data = .x)), coef = map(model, tidy))

summary(lm(Cost ~ log(Water_Impact), data = filter(df, Scenario == "SPS")))

params <- fits %>%
  unnest(coef) %>%
  select(Scenario, term, estimate) %>%
  tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
  rename(a = `(Intercept)`, b = `log(Water_Impact)`)

# Slope at defined points
df <- df %>%
  left_join(params, by = "Scenario") %>%
  mutate(cost_fitted = a + b * log(Water_Impact), slope = -b / Water_Impact) |> # negative slope: cost increases as Water_Impact decreases
  mutate(label_slope = if_else(metric %in% c("0%", "4%", "15%"), paste0("", round(slope, 2), " * ' USD/m'^3"), ""))

# Better get slope as difference between points
df <- df |>
  arrange(Scenario, metric) |>
  mutate(slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact))) |>
  mutate(label_slope = if_else(metric %in% c("0.5%", "4%", "15%"), paste0("", round(slope, 2), " * ' USD/m'^3"), ""))

# Get % of water red between first point
base_water <- df |> filter(metric == "0%") |> rename(water_base = Water_Impact) |> dplyr::select(Scenario, water_base)
df <- df |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water) |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(
    label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0("", round(water_red_pct), "%"))
  )


# Slope for demand reduction
df |>
  dplyr::select(Scenario, metric, Cost, Water_Impact) |>
  rename(Water = Water_Impact) |>
  pivot_wider(names_from = Scenario, values_from = c(Cost, Water)) |>
  mutate(
    slope_High = (Cost_NZE - Cost_APS) / (Water_NZE - Water_APS),
    slope_Low = (Cost_APS - Cost_SPS) / (Water_APS - Water_SPS),
    slope_HighLow = (Cost_SPS - Cost_NZE) / (Water_SPS - Water_NZE)
  )


# per ton of
# df <- df |>
#   mutate(
#     Water = Water * 1e3 / mtons, # m3 per ton
#     Cost = Cost * 1e3 / mtons # USD per ton
#   )

# FIGURE - Pareto curves: non-dominated solutions

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope
df_close <- df |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1)


data_fig <- df |> filter(!str_detect(metric, "Water"))
range(data_fig$Water_Impact)
range(data_fig$Cost)

ggplot(data_fig, aes(Water_Impact, Cost, col = Scenario)) +
  geom_line() +
  geom_point(size=0.5) +
  # fmt: skip
  geom_text(data = filter(data_fig, metric == "0%"),aes(label = Scenario),nudge_y = -50,nudge_x=-200,size = 7 * 5 / 14 * 0.8,hjust = 1) +
  # fmt: skip
  geom_text(data=filter(data_fig,str_detect(Scenario,"NZE")), aes(label=lab_metric),col="#4d4d4d",nudge_y=45*c(-1,1,1,rep(1,11)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0,fontface="italic") +
  # fmt: skip
  geom_text(data=filter(data_fig,str_detect(Scenario,"SPS")), aes(label=label_slope),col="#6c8364",nudge_y=45, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  geom_segment(data=df_close,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close$Water_Impact-1000/desalination_cost/2,y=df_close$Cost-1000/2,yend=df_close$Cost+1000/2), linetype="dashed", color="#0072B2") +
  geom_point(data=df_close,col="#0072B2",size=1) +
  # fmt: skip
  annotate("text", x = df_close$Water_Impact[2]-400, y = df_close$Cost[2]+400, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "#0072B2", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 ~ "-eq)"), y = "Total Cost (billion USD)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(1700, 8100)) +
  scale_color_manual(values = demand_colors) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("Figures/Fig2_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/TradeOff_perTon.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)

# sketch ideas
# ggplot(df, aes(mtons, Water, col = Cost)) +
#   geom_point() +
#   geom_text(data=filter(df,str_detect(Scenario,"SPS")), aes(label=lab_metric),col="darkgrey",nudge_x=2, size=7*5/14*0.8,hjust=0) +
#   scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(8, "Spectral")), labels = scales::label_comma()) +
#   labs(
#     y = expression("Total Freshwater Impact (billion " ~ m^3 ~ ")"),
#     col = "Total Cost (billion USD)",
#     x = "Cumulative Demand [million tons Cu]"
#   ) +
#   guides(color = guide_colorbar(barwidth = unit(10, "cm"))) +
#   theme_bw(8) +
#   theme(legend.position = "bottom", panel.grid = element_blank())

# ggsave("Figures/Cu_TradeOff_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)

# Add supply production by country for each point of the curve -----------------

# Store results from optimization run
(runs <- list.files("Results/Optimization/DemandScenario", recursive = T, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })())


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
    Basin_ID
  )

prod <- opt_results |>
  left_join(deposit) |>
  mutate(
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100,
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(Copper = sum(copper), Nickel = sum(nickel), Cobalt = sum(cobalt), Lithium = sum(lithium), ) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()

## parenthesis - get fish index basin level (normalize from 0 to 100)
fish <- read.csv("Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.csv")
water <- opt_results |>
  left_join(deposit) |>
  mutate(water = ktons_extracted * water / 1e3) |> # million m3
  group_by(Scenario, metric, Basin_ID) |>
  reframe(water = sum(water)) |>
  filter(water > 0) |>
  ungroup()
# global weighted by water cons fish index
water |>
  left_join(fish) |>
  group_by(Scenario, metric) |>
  reframe(avg_fish = weighted.mean(fish_index, water)) |>
  ungroup() |>
  ggplot(aes(metric, avg_fish, col = Scenario, group = Scenario)) +
  geom_line() +
  theme_pb_wide()

##

data_fig <- prod |> filter(Scenario == "SPS") |> filter(metric != "No Water Constraint")


# Long format
data_fig <- data_fig |>
  dplyr::select(Scenario, metric, country, Copper, Nickel, Cobalt, Lithium) |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = 'Mineral', values_to = 'total_metal') |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

# Total production by mineral and metric - COBALT is basically a co-product of Ni and Cu, not an active constraint
data_fig |>
  group_by(metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  pivot_wider(names_from = Mineral, values_from = total_metal)

# Key countries by mineral
data_fig |>
  filter(metric == "0%") |>
  group_by(Mineral) |>
  slice_max(order_by = total_metal, n = 10, with_ties = FALSE) |>
  summarise(Top10_countries = paste(country, collapse = ", "), .groups = "drop")


dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")
data_fig <- data_fig |>
  left_join(dict_region) |>
  mutate(
    region = case_when(
      Mineral == "Copper" &
        country %in%
          c("Chile", "Peru", "Indonesia", "Russia", "USA", "China", "Mongolia", "Dem. Rep. Congo", "Mexico") ~ ISO3,
      Mineral == "Nickel" &
        country %in% c("Indonesia", "Philippines", "Russia", "Australia", "New Caledonia", "Brazil") ~ ISO3,
      Mineral == "Cobalt" & country %in% c("Dem. Rep. Congo", "Indonesia") ~ ISO3,
      Mineral == "Lithium" &
        country %in% c("Chile", "Dem. Rep. Congo", "Argentina", "USA", "Australia", "Brazil") ~ ISO3,
      T ~ "RoW"
    ) |>
      str_replace("COD", "DRC")
  ) |>
  group_by(Scenario, metric, region, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric, Mineral) |>
  mutate(share = total_metal / sum(total_metal), ) |>
  ungroup()

unique(data_fig$region)
region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CHN" = "#d74c5a",
  "ARG" = "#ff7f00",
  "MNG" = "#e31a1c",
  "PHL" = "#1f78b4",
  "BRA" = "#33a02c",
  "NCL" = "#b15928",
  "AUS" = "#fb9a99",
  "Europe" = "#2b8cbe",
  "MEX" = "#66c2a5",
  "RoW" = "#4d4d4d"
)

data_fig <- data_fig |> mutate(region = factor(region, levels = rev(names(region_colors))))

# add total water impact per scenario
impact <- df |> group_by(Scenario, metric) |> reframe(Water_Impact = mean(Water_Impact)) |> ungroup()

data_fig <- data_fig |> left_join(impact)


data_fig <- data_fig |> mutate(region_label = if_else(share > 0.01, as.character(region), ""))

# order by share region
order_col <- data_fig |>
  mutate(order_col = paste0(Mineral, region)) |>
  arrange(desc(share)) |>
  pull(order_col) |>
  unique()
# put RoW at the end
order_col <- c(order_col[!str_detect(order_col, "RoW")], order_col[str_detect(order_col, "RoW")])
data_fig <- data_fig |> mutate(order_col = paste0(Mineral, region) |> factor(levels = rev(order_col)))

ggplot(data_fig, aes(Water_Impact, total_metal, fill = region, group = order_col)) +
  geom_area(col="black",linewidth=0.1) +
  # geom_col(col="black",linewidth=0.1) +
  geom_text(data=filter(data_fig, metric == "0%"), aes(x=Water_Impact-100,label = region_label),hjust=1,, position = position_stack(vjust = 0.5), size = 7 * 5 / 14 * 0.8, col="white") +
  facet_wrap(~Mineral, ncol = 2, scales = "free") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(expand = F) +
  labs(
    x = expression("Total Freshwater Impact (billion " ~ m^3 ~ "-eq)"),
    y = "",
    title = "2025-2050 Metal production, in million tons",
  ) +
  theme_pb_wide() +
  theme(legend.position = "none", panel.spacing.x = unit(1.2, "lines"))

# fmt: skip
ggsave("Figures/Fig2_prod.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# Trade off curves by climate scenarios -----

# Store results from optimization run
(runs <- list.files(
  "Results/Optimization/ClimateScenario/SPS/",
  pattern = "Metrics.*",
  recursive = T,
  full.names = TRUE
))

# filter by driver gfdl-esm4
# runs <- runs[str_detect(runs, "gfdl-esm4")]

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
head(obj)
unique(obj$file_name)
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
  ) |>
  mutate(
    Scenario = case_when(
      str_detect(folder_path, "picontrol") ~ "Pre-industrial control",
      str_detect(folder_path, "ssp126") ~ "SSP1-2.6",
      str_detect(folder_path, "ssp370") ~ "SSP3-7.0",
      str_detect(folder_path, "ssp585") ~ "SSP5-8.5",
      TRUE ~ "No Climate Scenario"
    )
  ) |>
  mutate(
    climateDriver = case_when(
      str_detect(folder_path, "gfdl-esm4") ~ "gfdl-esm4",
      str_detect(folder_path, "ipsl-cm6a-lr") ~ "ipsl-cm6a-lr",
      str_detect(folder_path, "mpi-esm1-2-hr") ~ "mpi-esm1-2-hr",
      str_detect(folder_path, "mri-esm2-0") ~ "mri-esm2-0",
      str_detect(folder_path, "ukesm1-0-ll") ~ "ukesm1-0-ll",
      T ~ "No Climate Scenario"
    )
  )

table(obj$Scenario)
table(obj$climateDriver)
table(obj$metric)
table(obj$Parameter)

df <- obj |>
  filter(!is.na(Scenario)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  # left_join(dem_tot) |>
  mutate(
    Water_cons = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world equiv.
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  ) |>
  mutate(`Water impact` = NULL)
head(df)
range(df$Water_Impact)


# Slope as difference between points
df <- df |>
  arrange(Scenario, metric) |>
  mutate(slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact))) |>
  mutate(label_slope = if_else(metric %in% c("0.5%", "4%", "15%"), paste0("", round(slope, 2), " * ' USD/m'^3"), ""))

# Get % of water red between first point
base_water <- df |>
  filter(metric == "0%") |>
  rename(water_base = Water_Impact) |>
  dplyr::select(Scenario, climateDriver, water_base)
df <- df |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water) |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(
    label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0("", round(water_red_pct), "%"))
  )

# FIGURE - Pareto curves: non-dominated solutions

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope
df_close <- df |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1)


data_fig <- df |> filter(!str_detect(metric, "Water")) |> mutate(group_key = paste0(Scenario, climateDriver))
range(data_fig$Water_Impact)
range(data_fig$Cost)

data_fig <- data_fig |> filter(Scenario != "Pre-industrial control")

ggplot(data_fig, aes(Water_Impact, Cost, col = Scenario, group = group_key)) +
  geom_line() +
  geom_point(size=0.5) +
  # fmt: skip
  geom_text_repel(data = filter(data_fig, metric == "0%",climateDriver=="gfdl-esm4"),aes(label = Scenario),size = 7 * 5 / 14 * 0.8,hjust = 0, direction = "y",
  nudge_x = 200) +
  # fmt: skip
  # geom_text(data=filter(data_fig,str_detect(Scenario,"SSP3-7.0")), aes(label=lab_metric),col="#4d4d4d",nudge_y=45*c(-1,1,1,rep(1,9)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  # geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0,fontface="italic") +
  # fmt: skip
  # geom_text(data=filter(data_fig,str_detect(Scenario,"SSP3-7.0")), aes(label=label_slope),col="#6c8364",nudge_y=45, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  # geom_segment(data=df_close,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close$Water_Impact-1000/desalination_cost/2,y=df_close$Cost-1000/2,yend=df_close$Cost+1000/2), linetype="dashed", color="#bc80bd") +
  # fmt: skip
  # annotate("text", x = df_close$Water_Impact[2]-200, y = df_close$Cost[2]+400, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "#bc80bd", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 ~ "-eq)"), y = "Total Cost (billion USD)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(2100, 9500)) +
  # scale_color_manual(values = c("#378bc9", "#d94253", "#6c8364")) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("Figures/Fig2_Climate.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/TradeOff_perTon.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)

# Curve by Mineral ----------------------
# Need to split costs and water allocation at deposit level, based on revenue share at each deposit

# Get total mineral production - total metal production previously loaded

# First get share of costs per deposit
prices <- read.csv("Parameters/MineralPrices.csv") # 2021-2025 avg price per ton

# revenue shares (depends only on grade, recovery rate and price) - NO RESULT FROM THE model
revShare <- deposit |>
  mutate(
    # fmt: skip
    copper_revenue = grade_resource_Copper * recovery_rate_Copper / 100 * prices[prices$Mineral == "Copper", ]$price_avg,
    # fmt: skip
    nickel_revenue = grade_resource_Nickel * recovery_rate_Nickel / 100 * prices[prices$Mineral == "Nickel", ]$price_avg,
    # fmt: skip
    cobalt_revenue = grade_resource_Cobalt * recovery_rate_Cobalt / 100 * prices[prices$Mineral == "Cobalt", ]$price_avg,
    # fmt: skip
    lithium_revenue = grade_resource_Lithium * recovery_rate_Lithium / 100 * prices[prices$Mineral == "Lithium", ]$price_avg
  ) |>
  mutate(total_revenue = copper_revenue + nickel_revenue + cobalt_revenue + lithium_revenue) |>
  mutate(
    copper_share = copper_revenue / total_revenue,
    nickel_share = nickel_revenue / total_revenue,
    cobalt_share = cobalt_revenue / total_revenue,
    lithium_share = lithium_revenue / total_revenue
  ) |>
  dplyr::select(ID, Name, copper_share, nickel_share, cobalt_share, lithium_share)

# get total production by deposit
data_fig_d <- opt_results |>
  left_join(deposit) |>
  mutate(
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  )

# calculate costs and water impacts
optInputs <- read.csv("Results/Optimization/DemandScenario/SPS/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value)) # 7%
depositCosts <- depositAll |> dplyr::select(ID, Name, OPEX_ore, CAPEX_opening, CAPEX_exp, share_NiCoCu, water_footprint)

# residual value
mine_life <- 15
fraction_not_recovered <- 0.2

data_fig_d <- data_fig_d |>
  left_join(depositCosts) |>
  # ALL THIS CODE JUST TO GET REMAINING FRACTION
  mutate(
    years_to_end = 2050 - t,
    remaining_life = mine_life - years_to_end,
    frac = pmin(pmax(remaining_life / mine_life, 0), 1),
    CAPEX_opening_adj = CAPEX_opening - (1 - fraction_not_recovered) * CAPEX_opening * frac,
    CAPEX_exp_adj = CAPEX_exp - (1 - fraction_not_recovered) * CAPEX_exp * frac
  ) |>
  mutate(
    # fmt: skip
    costs = (ktons_extracted * OPEX_ore / 1e3 + mine_opened * CAPEX_opening_adj + capacity_added * CAPEX_exp_adj / 1e3) * share_NiCoCu/1e3, # billion USD
    costs = costs / (1 + r)^(t - 2025), # discount them
    water = ktons_extracted * water_footprint / 1e6 # to billion m3
  ) |>
  select(-years_to_end, -remaining_life, -frac) |>
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
    Copper = sum(copper), # million tons
    Nickel = sum(nickel),
    Cobalt = sum(cobalt),
    Lithium = sum(lithium),
    copper_cost = sum(copper_cost), # billion USD
    nickel_cost = sum(nickel_cost),
    cobalt_cost = sum(cobalt_cost),
    lithium_cost = sum(lithium_cost),
    costs = sum(costs), # billion USD
    copper_water = sum(copper_water), # billion m3
    nickel_water = sum(nickel_water),
    cobalt_water = sum(cobalt_water),
    lithium_water = sum(lithium_water),
    water = sum(water) # billion m3
  ) |>
  ungroup()


# Add slack costs
(runs_slack <- list.files("Results/Optimization/DemandScenario", pattern = "Slack.*", recursive = T, full.names = TRUE))
slack <- do.call(
  rbind,
  lapply(runs_slack, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)

slack <- slack |>
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
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
  )

slack_costs <- slack |>
  mutate(
    slack_cu = slack_cu * optInputs[optInputs$Parameter == "Slack cost Copper", ]$Value / (1 + r)^(t - 2025),
    slack_ni = slack_ni * optInputs[optInputs$Parameter == "Slack cost Nickel", ]$Value / (1 + r)^(t - 2025),
    slack_co = slack_co * optInputs[optInputs$Parameter == "Slack cost Cobalt", ]$Value / (1 + r)^(t - 2025),
    slack_li = slack_li * optInputs[optInputs$Parameter == "Slack cost Lithium", ]$Value / (1 + r)^(t - 2025)
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    slack_cu = sum(slack_cu) / 1e3, # billion USD
    slack_ni = sum(slack_ni) / 1e3,
    slack_co = sum(slack_co) / 1e3,
    slack_li = sum(slack_li) / 1e3
  ) |>
  ungroup()

# add to data
data_fig_d <- data_fig_d |>
  left_join(slack_costs) |>
  mutate(
    copper_cost = copper_cost + slack_cu,
    nickel_cost = nickel_cost + slack_ni,
    cobalt_cost = cobalt_cost + slack_co,
    lithium_cost = lithium_cost + slack_li,
    costs = costs + slack_cu + slack_ni + slack_co + slack_li
  )

# Get values per ton
data_fig_d <- data_fig_d |>
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / (Copper), # USD per ton
    Nickel_CostperTon = nickel_cost * 1e3 / (Nickel),
    Cobalt_CostperTon = cobalt_cost * 1e3 / (Cobalt),
    Lithium_CostperTon = lithium_cost * 1e3 / (Lithium),
    Copper_WaterperTon = copper_water * 1e3 / (Copper), # m3 per ton
    Nickel_WaterperTon = nickel_water * 1e3 / (Nickel),
    Cobalt_WaterperTon = cobalt_water * 1e3 / (Cobalt),
    Lithium_WaterperTon = lithium_water * 1e3 / (Lithium)
  )

# Gather for figure
data_fig_d_long <- data_fig_d |>
  dplyr::select(Scenario, metric, contains("perTon")) |>
  pivot_longer(cols = -c(Scenario, metric), names_to = c("Mineral", "Type"), names_sep = "_", values_to = "Value") |>
  pivot_wider(names_from = Type, values_from = Value) |>
  mutate(
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  )
names(data_fig_d_long) <- names(data_fig_d_long) |> str_remove("perTon")


data_fig_d_long <- data_fig_d_long |> filter(Scenario == "SPS") |> filter(metric != "No Water Constraint")
table(data_fig_d_long$metric)

ggplot(data_fig_d_long, aes(Water, Cost, col = Mineral)) +
  geom_line() +
  geom_point(size=0.5) +
  facet_wrap(~Mineral, scales = "free") +
  # fmt: skip
  # geom_text(data = filter(data_fig_d_long, metric == "0%"),aes(label = Mineral),nudge_y = -50,nudge_x=-200,size = 7 * 5 / 14 * 0.8,hjust = 1) +
  # fmt: skip
  geom_text(data=filter(data_fig_d_long,str_detect(Scenario,"SPS")), aes(label=lab_metric),col="#4d4d4d", size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  # geom_segment(data=df_close,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close$Water_Impact-1000/desalination_cost/2,y=df_close$Cost-1000/2,yend=df_close$Cost+1000/2), linetype="dashed", color="#0072B2") +
  # geom_point(data=df_close,col="#0072B2",size=1) +
  # fmt: skip
  # annotate("text", x = df_close$Water_Impact[2]-400, y = df_close$Cost[2]+400, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "#0072B2", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Freshwater Impact (" ~ m^3 ~ "-eq per ton)"), y = "Cost (USD per ton)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  # coord_cartesian(xlim = c(1700, 8100)) +
  # scale_color_manual(values = demand_colors) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

# fmt: skip
ggsave("Figures/Fig2_Mineral.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*1.5, height = 8.7)

# EoF
