# AWARE 2.0 LCA Water Scarcity Characterization Factors
# Source: https://onlinelibrary.wiley.com/doi/10.1111/jiec.70023
# Data: https://doi.org/10.5281/zenodo.15133241
# PBH Dec 2025

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


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


sum(cf_map$aware_available) / 1e9 # 42769 km3 available water annually globally
save_aux <- cf_map |>
  st_drop_geometry() |>
  dplyr::select(Basin_ID, aware_cf, aware_demand, aware_available) |>
  mutate(full_available = aware_available + aware_demand) |> # add demand so it is full available water
  mutate(stress = if_else(full_available < 0, 1, aware_demand / full_available)) |>
  mutate(stress = if_else(aware_demand < 0, 0, stress))

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
leaflet(cf_map) |>
  addTiles() |>
  addPolygons(
    fillColor = ~col,
    color = "#000000",
    weight = 0.3,
    fillOpacity = 0.8,
    popup = paste0("Basin_ID: ", cf_map$Basin_ID, "<br>", "CF_annual_unspecified: ", cf_map$aware_cf)
  )

# LOAD DEPOSIT ID AND DO SPATIAL JOIN ---------

cu_dep <- read.csv("Parameters/Intermediate/Cu_Deposit_SP.csv")
names(cu_dep)
cu_dep <- cu_dep |> dplyr::select(Name, ID, LATITUDE, LONGITUDE)

# convert to spatial points
pts_cu <- st_as_sf(cu_dep, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

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
    data = pts_cu,
    radius = 4,
    color = "red",
    fillColor = "red",
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~ paste0("Name: ", pts_cu$Name)
  )

# spatial join based on nearest feature
idx <- st_nearest_feature(pts_cu, cf_map)
pts_join <- cbind(pts_cu, st_drop_geometry(cf_map[idx, ]))
sum(is.na(pts_join$Basin_ID)) # 0 missing

# 4. minimal output
names(pts_join)
out <- pts_join |> select(Basin_ID, Name, ID, aware_cf, aware_demand, aware_available)

# save
out <- st_drop_geometry(out)
head(out)
write.csv(out, "Parameters/Intermediate/Cu_Deposit_aware.csv", row.names = FALSE)

# EoF
