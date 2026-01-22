# Copper Demand
# Simplified for now: https://github.com/sduex/Copper_Paper

library(tidyverse)

df <- read.csv("Inputs/Busch2025/Cu_Demand_Duex.csv")
range(df$Year)
table(df$Mineral)
table(df$Scenario)

df <- df |> filter(Year >= 2025 & Year <= 2050)
range(df$Year)

write.csv(df, "Parameters/Cu_Demand.csv", row.names = F)

names(df)

dict <- tibble(
  Scenario = c(
    "Ambitious-Baseline-Baseline-Baseline-Baseline",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline_OtherLow",
    "Aggressive_Recycling"
  ),
  name = c("Baseline Demand", "High Demand", "Low Demand")
)


data_fig <- df |> left_join(dict) |> filter(!is.na(name))

ggplot(data_fig, aes(Year, Demand / 1e3, col = name)) +
  geom_line() +
  geom_text(data=filter(data_fig,Year==2050), aes(label=name),nudge_x=.2, size=7*5/14*0.8,hjust=0) +
  labs(y = "", title = "Copper Demand (Million tonnes)", x = "", col = "") +
  coord_cartesian(expand = F, ylim = c(0, 40), xlim = c(2025, 2053)) +
  theme(legend.position = "none")

# fmt: skip
ggsave("Figures/CuDemand.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

# Eof
