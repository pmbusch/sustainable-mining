# Trade off cost and water
# PBH Nov 2025

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


# Store results from optimization run
# (runs <- list.files("Results/Optimization/DemandScenario", pattern = "Metrics.*", recursive = T, full.names = TRUE))
(runs <- list.files(
  "Results/Optimization/ClimateScenario/APS/Deposit_ssp370_gfdl-esm4",
  pattern = "Metrics.*",
  recursive = T,
  full.names = TRUE
))

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
    ) |>
      factor(
        levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%")
      )
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
  geom_text(data = filter(data_fig, metric == "0%"),aes(label = Scenario),nudge_y = 100,nudge_x=100,size = 7 * 5 / 14 * 0.8,hjust = 0) +
  # fmt: skip
  geom_text(data=filter(data_fig,str_detect(Scenario,"NZE")), aes(label=lab_metric),col="#4d4d4d",nudge_y=45*c(-1,1,1,rep(1,9)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  geom_text(data=filter(data_fig,str_detect(Scenario,"SPS")), aes(label=label_slope),col="#6c8364",nudge_y=45, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  geom_segment(data=df_close,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close$Water_Impact-1000/desalination_cost/2,y=df_close$Cost-1000/2,yend=df_close$Cost+1000/2), linetype="dashed", color="#bc80bd") +
  # fmt: skip
  annotate("text", x = df_close$Water_Impact[2]+200, y = df_close$Cost[2]+200, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "#bc80bd", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 ~ ")"), y = "Total Cost (billion USD)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  # coord_cartesian(xlim = c(1700, 8000)) +
  scale_color_manual(values = c("#378bc9", "#d94253", "#6c8364")) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

# ggsave("Figures/TradeOff.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/TradeOff.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)
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
opt_results <- opt_results |> filter(ktons_extracted > 0)
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
    ) |>
      factor(
        levels = rev(c(
          "No Water Constraint",
          "0%",
          "0.5%",
          "1%",
          "2%",
          "3%",
          "4%",
          "5%",
          "6%",
          "8%",
          "10%",
          "12%",
          "15%"
        ))
      )
  )
deposit <- read.csv("Parameters/Deposit.csv")

deposit <- deposit |>
  dplyr::select(
    ID,
    Name,
    grade_resource_Copper,
    grade_resource_Nickel,
    grade_resource_Cobalt,
    grade_resource_Lithium,
    country
  )

data_fig <- opt_results |>
  left_join(deposit) |>
  mutate(
    total_metal = ktons_extracted /
      1000 *
      (grade_resource_Copper + grade_resource_Nickel + grade_resource_Cobalt + grade_resource_Lithium) /
      100
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(total_metal = sum(total_metal)) |>
  group_by(Scenario, metric) |>
  mutate(share = total_metal / sum(total_metal)) |>
  ungroup()

data_fig <- data_fig |> filter(Scenario == "SPS")

# Key countries
dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")

data_fig |> group_by(country) |> reframe(total_metal = sum(total_metal)) |> arrange(desc(total_metal)) |> head(15)
data_fig <- data_fig |>
  left_join(dict_region) |>
  mutate(
    region = case_when(
      country %in% c("Chile", "Peru", "Indonesia", "Russia", "USA", "China", "Canada") ~ ISO3,
      str_detect(country, "Congo") ~ "DRC",
      Continent %in% c("Europe", "Asia") ~ Continent,
      T ~ "RoW"
    )
  ) |>
  group_by(Scenario, metric, region) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric) |>
  mutate(share = total_metal / sum(total_metal)) |>
  ungroup()


region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CHN" = "#d74c5a",
  "CAN" = "#ff7f00",
  "Europe" = "#2b8cbe",
  "Asia" = "#66c2a5",
  "RoW" = "#4d4d4d"
)

data_fig <- data_fig |> mutate(region = factor(region, levels = rev(names(region_colors))))


# add total water impact per scenario
impact <- df |> group_by(Scenario, metric) |> reframe(Water_Impact = mean(Water_Impact)) |> ungroup()

data_fig <- data_fig |> left_join(impact)

ggplot(data_fig, aes(Water_Impact, share, fill = region)) +
  geom_area(col="black",linewidth=0.1) +
  # geom_col(col="black",linewidth=0.1) +
  geom_text(data=filter(data_fig, metric == "0%"), aes(label = region),hjust=1, position = position_stack(vjust = 0.5), size = 7 * 5 / 14 * 0.8, col="white") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(expand = F) +
  labs(
    x = expression("Total Freshwater Impact (billion " ~ m^3 ~ ")"),
    y = "",
    title = "Share of metal production",
    fill = "Country"
  ) +
  theme_pb_wide() +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/Fig3_prod.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# EoF
