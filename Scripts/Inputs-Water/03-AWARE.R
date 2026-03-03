# AWARE 2.0 LCA Water Scarcity Characterization Factors
# Source: https://onlinelibrary.wiley.com/doi/10.1111/jiec.70023
# Data: https://doi.org/10.5281/zenodo.15133241
# PBH Dec 2025

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

showMaps <- F
# showMaps <- T

# LOAD AWARE GEOSPATIAL DATA ---------

library(sf)
# unzip kmz to get kml
unzip("Inputs/AWARE/AWARE20_Native_CFs_geospatial.kmz", exdir = "kmz_unzip")
k <- st_read("kmz_unzip/doc.kml")
st_crs(k)
k_sf <- st_make_valid(k)
k_sf <- st_zm(k_sf) # drop Z/M if present
k_sf <- st_cast(k_sf, "MULTIPOLYGON") # cast to polygon type


names(k_sf)
k_sf$Name[1:2]
k_sf$Description[1:2]

# Load data from Excel
# m3 world equiv./m3	The characterization factors in watershed resolution, ranging rom 0.1 to 100.
cf <- read_excel("Inputs/AWARE/AWARE20_Native_CFs.xlsx", sheet = "native_CFs")
names(cf)

# m3/month and m3/year	water demand 2019, all sectors, used for annual averages of native CFs
demand <- read_excel("Inputs/AWARE/AWARE20_Native_CFs.xlsx", sheet = "2019_all_pHWC")
names(demand)
demand <- demand |> dplyr::select(Basin_ID, annual_sum)

# AMD: availability water minus demand
amd <- read_excel("Inputs/AWARE/AWARE20_Intermediate_Variables.xlsx", sheet = "AMD_final") # in m3/m2 month
area <- read_excel("Inputs/AWARE/AWARE20_Intermediate_Variables.xlsx", sheet = "basin_area") # in m2
amd <- amd |>
  left_join(area) |>
  mutate(available_m3 = (Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec) * area) |>
  dplyr::select(Basin_ID, available_m3)
sum(amd$available_m3) / 1e9 # 42769 km3 available water annually globally


cf_map <- k_sf
cf_map$Basin_ID <- as.numeric(str_remove(cf_map$Name, "CFs for Basin_ID "))
cf_map <- left_join(cf_map, cf, by = "Basin_ID") |> left_join(demand) |> left_join(amd)
head(cf_map)

cf_map |> filter(!is.na(annual_unspecified)) |> nrow() # 9406, same as excel

cf_map <- cf_map |> filter(!is.na(annual_unspecified))

cf_map <- cf_map |> rename(aware_cf = annual_unspecified, aware_demand = annual_sum, aware_available = available_m3)

# available after demand
sum(cf_map$aware_available) / 1e9 # 42769 km3 available water annually globally

cf_map <- cf_map |> mutate(gross_available = aware_available + aware_demand) # add demand back to availability to get gross available water
sum(cf_map$gross_available) / 1e9 #  44350 km3

## Numerical stability, convert really low numbers for availability to zero ----------
cf_map |>
  st_drop_geometry() |>
  ggplot(aes(x = gross_available + 1e-6)) +
  # geom_histogram(bins = 100) +
  stat_ecdf() +
  scale_x_log10()

cutoff <- 0 # to inspect baseline scenario
cutoff <- 1e6 # 1 million m3 per year per basin
cf_map |>
  st_drop_geometry() |>
  filter(gross_available > cutoff) |>
  reframe(
    max_val = max(gross_available, na.rm = TRUE),
    min_val = min(gross_available, na.rm = TRUE),
    spread = (max_val / min_val),
    n = n()
  ) |>
  mutate(
    max_val = formatC(max_val, format = "e", digits = 2),
    min_val = formatC(min_val, format = "e", digits = 2),
    spread = formatC(spread, format = "e", digits = 2) # e19 spread!!!!! to much
  )

# change to zero
cf_map <- cf_map |>
  mutate(gross_available = if_else(abs(gross_available) > cutoff, gross_available, 0)) |>
  mutate(aware_available = gross_available - aware_demand)

sum(cf_map$aware_available) / 1e9 # 42769 km3 available water annually globally

save_aux <- cf_map |>
  st_drop_geometry() |>
  dplyr::select(Basin_ID, aware_cf, aware_demand, aware_available, gross_available) |>
  mutate(stress = if_else(gross_available <= 0, 1, aware_demand / gross_available)) |>
  mutate(stress = if_else(aware_demand < 0, 0, stress))

# Save basin aware water stres
write.csv(save_aux, "Parameters/AWARE_Basin_Stress.csv", row.names = FALSE)


# colors for map, to replicate figure frm paper
cf_map <- cf_map |>
  mutate(
    col = case_when(
      aware_cf <= 0.5 ~ "#0000FF", # blue
      aware_cf < 1 ~ "#006400", # dark green
      aware_cf < 10 ~ "#90EE90", # light green
      aware_cf < 30 ~ "#FFFF00", # yellow
      aware_cf < 60 ~ "#FFA500", # orange
      aware_cf < 95 ~ "#FF7F7F", # light red
      TRUE ~ "#8B0000" # dark red
    )
  )


library(leaflet)

if (showMaps) {
  leaflet(cf_map) |>
    addTiles() |>
    addPolygons(
      fillColor = ~col,
      color = "#000000",
      weight = 0.3,
      fillOpacity = 0.8,
      popup = paste0("Basin_ID: ", cf_map$Basin_ID, "<br>", "CF_annual_unspecified: ", cf_map$aware_cf)
    )
}


# ADD DEPOSITS WATER CONSUMPTIONS --------------

# S&P Copper, Nickel, Cobalt, Lithium Data - already filtered and pre-processed so it is based on ore processed
deposit <- read.csv("Parameters/Intermediate/All_Deposit_SP.csv")
nrow(deposit) # 1108

## WATER CONSUMPTION ---------

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

li_water <- read_excel("Inputs/Li_Water.xlsx", sheet = "Database")
li_water <- li_water |>
  filter(Include == 1) %>%
  mutate(ore_grade = as.numeric(`Grade ore Li%`)) %>%
  group_by(ID) |>
  reframe(water_dep = mean(TotalWater_m3_tonLCE, na.rm = T) * 5.323, grade = mean(ore_grade)) |>
  ungroup() |>
  mutate(ore_cons_li = water_dep * grade) |>
  dplyr::select(ID, ore_cons_li) |>
  filter(!is.na(ore_cons_li))
# dup for salar de atacama
li_water <- rbind(li_water, tibble(ID = 52581, ore_cons_li = filter(li_water, ID == "37384")$ore_cons_li))


# 2. Based on ore grade
ore_water <- 0.8156 # From collected data on copper deposits, the water consumption is 0.8156 m3 per ton of ore processed
brine_water <- 0.604 + 1 # evaporation, m3 per m3 of brine, +1 is to include brine water as consumption
brineDLE_water <- 0.94 + 1 # DLE, m3 per m3 of brine, +1 is to include brine water as consumption
oreLi_water <- 1.99 # m3 per ton ore hard rock (for clay as well)

table(deposit$mine_type)
deposit <- deposit %>%
  left_join(cu_water) |>
  left_join(li_water) |>
  # in m3 per ton ore
  mutate(
    water = case_when(
      !is.na(ore_cons) ~ ore_cons,
      !is.na(ore_cons_li) ~ ore_cons_li,
      # by lithium type
      mine_type == "Brine" ~ brine_water,
      mine_type == "Brine DLE" ~ brineDLE_water,
      mine_type == "Hard Rock" ~ oreLi_water,
      TRUE ~ ore_water # rest for copper, nickel and cobalt
    ),
    water_fill = if_else(!is.na(ore_cons) | !is.na(ore_cons_li), "Literature", "Fitted Model")
  )
table(deposit$water_fill) # 77 literature
sum(is.na(deposit$water)) # no missing
range(deposit$water)

deposit$ore_cons <- deposit$ore_cons_li <- NULL

# SPATIAL JOIN DEPOSITS + AWARE ---------------

# convert to spatial points
deps <- deposit |> dplyr::select(ID, LONGITUDE, LATITUDE, Name)
pts_dep <- st_as_sf(deps, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


if (showMaps) {
  # Map with deposits
  leaflet(cf_map) |>
    addTiles() |>
    addPolygons(
      fillColor = ~col,
      color = "#000000",
      weight = 0.3,
      fillOpacity = 0.8,
      popup = paste0("Basin_ID: ", cf_map$Basin_ID, "<br>", "CF_annual_unspecified: ", cf_map$aware_cf)
    ) |>
    addCircleMarkers(
      data = pts_dep,
      radius = 4,
      color = "red",
      fillColor = "red",
      fillOpacity = 1,
      stroke = FALSE,
      popup = ~ paste0("Name: ", pts_dep$Name)
    )
}

# spatial join based on nearest feature
idx <- st_nearest_feature(pts_dep, cf_map)
pts_join <- cbind(pts_dep, st_drop_geometry(cf_map[idx, ]))
sum(is.na(pts_join$Basin_ID)) # 0 missing

# 4. minimal output
names(pts_join)
aware <- pts_join |> select(Basin_ID, Name, ID, aware_cf, aware_demand, aware_available)

# join to deposit database no geometry
aware <- st_drop_geometry(aware)
nrow(aware) # 1108
# add water risk baseline - AWARE factors
# factor from 0.1 to 100
# annual demand and available in m3 per year

wb <- deposit %>% left_join(aware) |> mutate(water_footprint = water * aware_cf)
sum(is.na(wb$water_footprint)) # 0


# SUBSTRACT CURRENT WATER CONSUMPTION AT BASIN LEVEL ---------------

# re-calculate available water for constraint at basin level
wb_basin <- wb |>
  mutate(water_cons_2025 = water * ore_processed) |>
  group_by(Basin_ID) |>
  reframe(
    aware_cf = mean(aware_cf, na.rm = T),
    aware_available = mean(aware_available, na.rm = T),
    aware_demand = mean(aware_demand, na.rm = T),
    water_cons_2025 = sum(water_cons_2025, na.rm = T)
  ) |>
  ungroup() |>
  mutate(water_cons_2025 = pmin(water_cons_2025, aware_demand)) |> # Limit water demand for mining as total demand listed in AWARE factors
  # mutate(demand_noMining = aware_demand - water_cons_2025) |>
  # arrange(demand_noMining)
  mutate(aware_available = aware_available + water_cons_2025) |> # add baseline water back to basin availability for mining purposes
  dplyr::select(Basin_ID, aware_available, water_cons_2025)

wb$aware_available <- NULL
wb <- wb %>% left_join(wb_basin, by = "Basin_ID")

# Add fish biodiversity index
fish <- read.csv("Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.csv")
wb <- wb |> left_join(fish, by = "Basin_ID")


# Save water data
names(wb)
write.csv(wb, "Parameters/Deposit.csv", row.names = F)

# Pre-compute water tree upstream table (acyclical) - a sub-basin only has one discharge basin
# Sub-basin ID Tree from AWARE 2.0
tree <- read_excel("Inputs/AWARE/AWARE20_Intermediate_Variables.xlsx", "additional_information")
upstream <- tree |>
  dplyr::select(Basin_ID, subbasin_discharges_into_Basin_ID) |>
  filter(!(subbasin_discharges_into_Basin_ID %in% c(-1, -11))) |>
  group_by(subbasin_discharges_into_Basin_ID) |>
  summarise(up = list(Basin_ID), .groups = "drop") |>
  deframe()

# recursive function: all upstream basins of i (including i)
f.get_upstream <- function(i) {
  u <- upstream[[as.character(i)]]
  if (is.null(u)) {
    return(i)
  }
  unique(c(i, unlist(map(u, f.get_upstream))))
}

basins <- unique(tree$Basin_ID)
# indicates basin to sub-basins (upstream) - including itself
upstream_df <- tibble(Basin_ID = basins, upstream_basins = map(basins, f.get_upstream)) |> unnest(upstream_basins)

# Create map - all deposits - including upstream - inside each basin
# Do to upstream, deposits may be repeated
basin_deposits <- upstream_df |>
  left_join(dplyr::select(wb, ID, Name, Basin_ID) |> rename(upstream_basins = Basin_ID), by = "upstream_basins") |>
  filter(!is.na(ID)) |>
  distinct(Basin_ID, ID, Name)

write.csv(basin_deposits, "Parameters/basin_to_deposits_upstream.csv", row.names = FALSE)

# EoF
