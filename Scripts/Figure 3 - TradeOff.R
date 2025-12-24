# Trade off, based only on results
# PBH Nov 2025

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = scens_selected, name = name_abbr))

dict <- tibble(
  Scenario = c(
    "Ambitious-Baseline-Baseline-Baseline-Baseline",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline",
    "Ambitious-Baseline-Baseline-Baseline-Enhanced recycling"
  ),
  name = c("Baseline", "High Demand", "Low Demand")
)


demand <- read.csv("Parameters/Cu_Demand.csv")
deposit <- read.csv("Parameters/Cu_Deposit.csv")


# Store results from optimization run
(runs <- list.files("Results/Optimization/DemandScenario", pattern = "^Metric.*", recursive = T, full.names = TRUE))
(runs <- list.files("Results/Optimization/Copper", pattern = "^Metric.*", recursive = T, full.names = TRUE))

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      metric = basename(folder_path) |> str_remove_all("Metrics|\\.csv") |> as.numeric(),
      Scenario = basename(dirname(folder_path))
    )
  })
) |>
  mutate(metric = if_else(is.na(metric), 0, metric)) |> # full cost
  left_join(dict)
# mutate(name = factor(name, name_abbr))

table(obj$Scenario)
table(obj$name)
table(obj$metric)
table(obj$Parameter)

df <- obj |>
  filter(!is.na(name)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(
    Water = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    lab_metric = if_else(metric == 0, "Cost degradation 0%", paste0("", metric, "%"))
  )

range(df$Water)

ggplot(df, aes(Water, Cost, col = name)) +
  geom_line() +
  geom_point(col="darkgrey") +
  geom_text_repel(
    data = filter(df, metric == 0),
    aes(label = name),
    nudge_y = -10,
    size = 7 * 5 / 14 * 0.8,
    hjust = 0
  ) +
  geom_text(data=filter(df,str_detect(name,"High")), aes(label=lab_metric),col="darkgrey",nudge_y=15, size=7*5/14*0.8) +
  labs(x = expression("Total Water Footprint (billion " ~ m^3 ~ ")"), y = "Total Cost (billion USD)", col = "") +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma(), limits = c(800, 4500)) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

# ggsave("Figures/TradeOff.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/Cu_TradeOff.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)

# EoF
