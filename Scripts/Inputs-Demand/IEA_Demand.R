# Battery minerals demand from IEA 2025 Critical Minerals Explorer
# https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset
# Data is for 2024 to 2050 in 5-year intervals

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Load IEA Demand Data -------------
# all in ktons, by scenario

# Scenarios definition
# SPS: Stated Policies Scenario
# APS: Announced Pledges Scenario
# NZE: Net Zero Emissions by 2050 Scenario

# fmt: skip
names_header <- c("Sector","SPS_2024","a1","SPS_2030","SPS_2035","SPS_2040","SPS_2045","SPS_2050",
                  "a2","APS_2030","APS_2035","APS_2040","APS_2045","APS_2050",
                  "a3","NZE_2030","NZE_2035","NZE_2040","NZE_2045","NZE_2050")

# fmt: skip
cu <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A8:T17",col_names = F)
names(cu) <- names_header
cu$Mineral <- "Copper"

# fmt: skip
ni <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A42:T50",col_names = F)
names(ni) <- names_header
ni$Mineral <- "Nickel"

# fmt: skip
co <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A22:T28",col_names = F)
names(co) <- names_header
co$Mineral <- "Cobalt"

# fmt: skip
li <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A33:T37",col_names = F)
names(li) <- names_header
li$Mineral <- "Lithium"

df <- rbind(cu, ni, co, li)

# Reshape ------------------
df$a1 <- df$a2 <- df$a3 <- NULL

# long format
df <- df |>
  mutate(APS_2024 = SPS_2024, NZE_2024 = SPS_2024) |> # add 2024 for all scenarios
  pivot_longer(c(-Sector, -Mineral), names_to = 'key', values_to = 'ktons') |>
  tidyr::separate(key, into = c("Scenario", "Year"), sep = "_", remove = FALSE) |>
  mutate(Year = as.numeric(Year), key = NULL)


# Linear interpolation between years
library(zoo)
df <- df |>
  group_by(Sector, Mineral, Scenario) %>%
  complete(Year = 2024:2050) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(ktons = zoo::na.approx(ktons, x = Year, na.rm = FALSE)) %>%
  ungroup()

# Filter 2025 to 2050 and total demand
df <- df |> filter(Year >= 2025, Sector == "Total demand")

nrow(df) # 312 = 26 years * 4 minerals * 3 scenarios

# Save ----------------------

# Spread it to save
df_wide <- df |> mutate(Sector = NULL) |> pivot_wider(names_from = Mineral, values_from = ktons)
nrow(df_wide) # 78 = 26 years * 3 scenarios

write.csv(df_wide, "Parameters/IEA_Demand.csv", row.names = FALSE)

# EoF
