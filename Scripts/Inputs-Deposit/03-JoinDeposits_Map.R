# Join deposits for the map purposes and for optimization code
# Source: S&P Data
# PBH Jan 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# LOAD AND MERGE -----------

df <- read.csv("Parameters/Intermediate/CuNiCo_Deposit_SP.csv")
li <- read.csv("Parameters/Intermediate/Li_Deposit_SP.csv")
names(df)
names(li)

# add mineral to join
df$reserves_Lithium <- df$grade_reserves_Lithium <- df$grade_resource_Lithium <- df$grade_head_Lithium <- df$resources_Lithium <- df$recovery_rate_Lithium <- 0
li$reserves_Copper <- li$grade_reserves_Copper <- li$grade_resource_Copper <- li$grade_head_Copper <- li$resources_Copper <- li$recovery_rate_Copper <- 0
li$reserves_Nickel <- li$grade_reserves_Nickel <- li$grade_resource_Nickel <- li$grade_head_Nickel <- li$resources_Nickel <- li$recovery_rate_Nickel <- 0
li$reserves_Cobalt <- li$grade_reserves_Cobalt <- li$grade_resource_Cobalt <- li$grade_head_Cobalt <- li$resources_Cobalt <- li$recovery_rate_Cobalt <- 0

df <- rbind(df, li)

# intermediate save
write.csv(df, "Parameters/Intermediate/All_Deposit_SP.csv", row.names = F)

# MAP --------
# Select
df <- df |>
  dplyr::select(
    ID,
    PRIMARY_COMMODITY,
    LATITUDE,
    LONGITUDE,
    resources_Copper,
    resources_Nickel,
    resources_Cobalt,
    resources_Lithium
  )


# Filter by status and primary commodity
df <- df |>
  mutate(
    PRIMARY_COMMODITY = if_else(
      PRIMARY_COMMODITY %in% c("Copper", "Nickel", "Cobalt", "Lithium"),
      PRIMARY_COMMODITY,
      "Other"
    )
  )

# gather to mineral (long format)
df <- df |>
  pivot_longer(
    c(resources_Copper, resources_Nickel, resources_Cobalt, resources_Lithium),
    names_to = 'Mineral',
    values_to = 'resources'
  ) |>
  mutate(Mineral = str_remove(Mineral, "resources_")) |>
  filter(resources > 0)

nrow(df) # 2204
length(unique(df$ID)) # 1781 unique
table(df$PRIMARY_COMMODITY, df$Mineral)
df |> filter(Mineral == "Lithium") |> group_by(PRIMARY_COMMODITY) |> summarise(n = n()) # 151 for lithium
df |> group_by(Mineral) |> summarise(total_resources = sum(resources) / 1e6) # in million tons

write.csv(df, "Parameters/Deposits_Map.csv", row.names = F)
