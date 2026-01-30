# Add water consumption per ton for deposits and combine it with AWARE characterization factors
# PBH Jan 2026

# LOAD---------

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# S&P Copper, Nickel, Cobalt Data - already filtered and pre-processed so it is based on ore processed
deposit <- read.csv("Parameters/Intermediate/CuNiCo_Deposit_SP.csv")
nrow(deposit) # 1630


# WATER CONSUMPTION ---------

# Water consumption allocation hierarchy

# 1. Based on Literature reported value
cu_water <- read_excel("Inputs/Cu_Water.xlsx", sheet = "Data")
cu_water <- cu_water %>% mutate(ore_grade = as.numeric(`Grade ore Cu%`)) %>% filter(!is.na(ore_grade))

# average for deposits with multiple data entries
cu_water <- cu_water |>
  group_by(Deposit) |>
  reframe(water_dep = mean(TotalWater_m3_tonCu, na.rm = T), grade = mean(ore_grade)) |>
  ungroup() |>
  mutate(ore_cons = water_dep * grade) |>
  rename(Name = Deposit) |>
  dplyr::select(Name, ore_cons)

# 2. Based on ore grade
# From collected data on copper deposits, the water consumption is 0.8156 m3 per ton of ore processed
ore_water <- 0.8156

deposit <- deposit %>%
  left_join(cu_water) |>
  # in m3 per ton ore
  mutate(
    water = if_else(!is.na(ore_cons), ore_cons, ore_water),
    water_fill = if_else(!is.na(ore_cons), "Literature", "Fitted Model")
  )
table(deposit$water_fill) # 46 literature
sum(is.na(deposit$water)) # no missing
range(deposit$water)

# AWARE ---------------

# add water risk baseline - AWARE factors
aware <- read.csv("Parameters/Intermediate/CuNiCo_Deposit_aware.csv")
# factor from 0.1 to 100
# annual demand and available in m3 per year
aware <- aware |> dplyr::select(Basin_ID, Name, ID, aware_cf, aware_demand, aware_available)


wb <- deposit %>% left_join(aware) |> mutate(water_footprint = water * aware_cf)
sum(is.na(wb$water_footprint)) # 0

# Save water data
names(wb)
wb$water_dep <- wb$coef <- wb$ore_cons <- NULL
write.csv(wb, "Parameters/CuNiCo_Deposit.csv", row.names = F)

# EoF
