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
df$reserves_Lithium <- df$grade_reserves_Lithium <- df$grade_resource_Lithium <- df$grade_head_Lithium <- df$resources_Lithium <- df$recovery_rate_Lithium <- df$prod2025_Lithium <- 0
li$reserves_Copper <- li$grade_reserves_Copper <- li$grade_resource_Copper <- li$grade_head_Copper <- li$resources_Copper <- li$recovery_rate_Copper <- li$prod2025_Copper <- 0
li$reserves_Nickel <- li$grade_reserves_Nickel <- li$grade_resource_Nickel <- li$grade_head_Nickel <- li$resources_Nickel <- li$recovery_rate_Nickel <- li$prod2025_Nickel <- 0
li$reserves_Cobalt <- li$grade_reserves_Cobalt <- li$grade_resource_Cobalt <- li$grade_head_Cobalt <- li$resources_Cobalt <- li$recovery_rate_Cobalt <- li$prod2025_Cobalt <- 0

df <- rbind(df, li)

# Numerical stability issues - Force deposits with small resources to be zero
# Resourcesa are in tons
df |>
  pivot_longer(
    c(resources_ore, resources_Copper, resources_Nickel, reserves_Cobalt, reserves_Lithium),
    names_to = 'mineral',
    values_to = 'resources'
  ) |>
  filter(resources > 0) |>
  ggplot(aes(x = resources + 1e-6)) +
  # geom_histogram(bins = 100) +
  stat_ecdf() +
  facet_wrap(~mineral, scales = "free_x") +
  scale_x_log10()

# Spreads - limit below 1e6
cutoff <- c(ore = 0, Copper = 0, Nickel = 0, Cobalt = 0, Lithium = 0) # to inspect baseline scenario
cutoff <- c(ore = 1e6, Copper = 1e5, Nickel = 5e4, Cobalt = 1e4, Lithium = 1e4)

# spread above material-specific cutoff
df |>
  select(starts_with("resources_")) |>
  pivot_longer(everything(), names_to = "material", values_to = "value") |>
  mutate(material = sub("resources_", "", material), cutoff = cutoff[material]) |>
  filter(value > cutoff) |>
  reframe(
    max_val = max(value, na.rm = TRUE),
    min_val = min(value, na.rm = TRUE),
    spread = (max_val / min_val),
    .by = material
  ) |>
  mutate(
    max_val = formatC(max_val, format = "e", digits = 2),
    min_val = formatC(min_val, format = "e", digits = 2),
    spread = formatC(spread, format = "e", digits = 2)
  ) # in baseline problematic for everyone

# proportions below material-specific cutoff
df |>
  select(starts_with("resources_")) |>
  pivot_longer(everything(), names_to = "material", values_to = "value") |>
  mutate(material = sub("resources_", "", material), cutoff = cutoff[material]) |>
  summarise(
    n_total_pos = sum(value > 0, na.rm = TRUE),
    n_below_cutoff = sum(value > 0 & value < cutoff, na.rm = TRUE),
    prop_rows_below_cutoff = n_below_cutoff / n_total_pos,
    total_resource = sum(value[value > 0], na.rm = TRUE),
    total_below_cutoff = sum(value[value > 0 & value < cutoff], na.rm = TRUE),
    prop_resource_below_cutoff = total_below_cutoff / total_resource,
    .by = material
  )

# Change to zero and filter
df <- df |>
  mutate(
    resources_Copper = if_else(resources_Copper > cutoff["Copper"], resources_Copper, 0),
    resources_Nickel = if_else(resources_Nickel > cutoff["Nickel"], resources_Nickel, 0),
    resources_Cobalt = if_else(resources_Cobalt > cutoff["Cobalt"], resources_Cobalt, 0),
    resources_Lithium = if_else(resources_Lithium > cutoff["Lithium"], resources_Lithium, 0)
  ) |>
  filter(resources_Copper + resources_Nickel + resources_Cobalt + resources_Lithium > 0)
nrow(df) # 1108

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

nrow(df) # 1293
length(unique(df$ID)) # 1108 unique
table(df$PRIMARY_COMMODITY, df$Mineral)
df |> filter(Mineral == "Lithium") |> group_by(PRIMARY_COMMODITY) |> summarise(n = n()) # 139 for lithium
df |> group_by(Mineral) |> summarise(total_resources = sum(resources) / 1e6) # in million tons

write.csv(df, "Parameters/Deposits_Map.csv", row.names = F)
