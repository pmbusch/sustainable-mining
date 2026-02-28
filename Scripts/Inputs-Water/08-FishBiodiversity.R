# Scripts/FW_FISH_BasinOverlap_Index.R
# Basin-level fish biodiversity overlap index:
#   Index_basin = sum_over_species( Area(basin ∩ species_range) / Area(species_range_total) )
# Based on method from https://assets-eu.researchsquare.com/files/rs-8662997/v1/aa005b25-d37f-4c23-81fb-fe66d35ee88f.pdf?c=1770217368
# PBH Feb 2026

source("Scripts/00-Libraries.R")

library(terra)
library(data.table)
library(stringr)

crs_eq <- "EPSG:6933"

temp_path <- "C:/terra_temp"
if (!dir.exists(temp_path)) {
  dir.create(temp_path, recursive = TRUE)
}

terraOptions(memfrac = 0.8, tempdir = temp_path, progress = 1)


# ----------------------------
# Load basins
# ----------------------------

unzip("Inputs/AWARE/AWARE20_Native_CFs_geospatial.kmz", exdir = "kmz_unzip")

basins_vect <- vect("kmz_unzip/doc.kml")
basins_vect <- project(basins_vect, crs_eq)
basins_vect$Basin_ID <- as.numeric(str_remove(basins_vect$Name, "CFs for Basin_ID "))

cat("Basins:", nrow(basins_vect), "\n") # 11661


# ----------------------------
# Raster template
# ----------------------------

r_template <- rast(ext(basins_vect), resolution = 10000, crs = crs_eq) # 10km resolution (very good for rasterization), 5km would be excellent
r_template <- rast(ext(basins_vect), resolution = 5000, crs = crs_eq)
basin_r <- rasterize(basins_vect, r_template, field = "Basin_ID", background = NA)

# Basin mean area 14,000 km2, so 10km resolution is 100km2 grids, good enough
# P5 of basin area is 78km2, too low

# ----------------------------
# Load ALL fish files together
# ----------------------------

fish_files <- c(
  "Inputs/FW_FISH/FW_FISH_PART1.shp",
  "Inputs/FW_FISH/FW_FISH_PART2.shp",
  "Inputs/FW_FISH/FW_FISH_PART3.shp"
)

fish_vect <- do.call(
  rbind,
  lapply(fish_files, function(f) {
    v <- vect(f)
    project(v, crs_eq)
  })
)

# species <- unique(fish_vect$sci_name)
species <- unique(fish_vect$id_no)
cat("Unique species:", length(species), "\n") # 14911


# ----------------------------
# Compute overlap
# ----------------------------

result_list <- vector("list", length(species))
names(result_list) <- species

# to speed up look up in the for loop
fish_split <- split(fish_vect, fish_vect$id_no)
species <- names(fish_split)

# Sum over each species
for (i in seq_along(species)) {
  sp <- species[i]

  if (i %% 10 == 0) {
    cat("Species", i, "of", length(species), "\n")
  }
  sp_vect <- fish_split[[sp]]
  sp_r <- rasterize(sp_vect, r_template, field = 1, background = 0)
  total_cells <- as.numeric(global(sp_r, "sum", na.rm = TRUE)[1, 1])

  if (total_cells == 0 || is.na(total_cells)) {
    next
  }

  overlap <- zonal(sp_r, basin_r, sum, na.rm = TRUE)
  overlap <- as.data.table(overlap)
  setnames(overlap, old = names(overlap), new = c("Basin_ID", "cells_overlap"))
  overlap[, frac := cells_overlap / total_cells] # area overlap
  result_list[[i]] <- overlap[, .(Basin_ID, frac)]
}

# ----------------------------
# Combine safely
# ----------------------------

result_all <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
result <- result_all[, .(fish_index = sum(frac, na.rm = TRUE)), by = Basin_ID]

# sanity checks
summary(result$fish_index) # 0, mean 1.23, max 452...
length(unique(fish_vect$id_no)) # 14911, same as species length
nrow(result) # 11624

# ----------------------------
# Normalize
# ----------------------------
quantile(result$fish_index, probs = c(0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99), na.rm = TRUE) # 20 is p99
# p98 is 8.5
result |> filter(fish_index < 10) |> ggplot(aes(fish_index)) + stat_ecdf() + theme_pb_wide()

# normalize from 0 to 100, cutoff at 10 (p98) to avoid outliers dominating the index
result <- result |>
  mutate(fish_index = if_else(is.na(fish_index), 0, fish_index)) |> # 0 if not present (NA)
  mutate(fish_index_raw = fish_index) |>
  mutate(fish_index = if_else(fish_index > 10, 100, fish_index / 10 * 100))
summary(result$fish_index) # mean 5.27


# ----------------------------
# Save
# ----------------------------

fwrite(result, "Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.csv")
saveRDS(result, "Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.rds")

unlink(temp_path, recursive = TRUE) # Clean temp file
cat("DONE\n")


# ----------------------------
# FIGURE DEBUG - Show in interactive map spatial extent
# Only run with zoom extent to avoid long processing time
# ----------------------------

debug <- F


if (debug) {
  library(leaflet)

  # Convert basins to sf and attach result
  basins_sf <- st_as_sf(basins_vect)
  basins_sf <- basins_sf |> left_join(result, by = "Basin_ID")

  # Convert fish to sf
  fish_sf <- st_as_sf(fish_vect)

  # Transform both to WGS84 (required for leaflet)
  basins_sf <- st_transform(basins_sf, 4326)
  fish_sf <- st_transform(fish_sf, 4326)

  # Color palette
  pal <- colorNumeric("viridis", domain = basins_sf$fish_index, na.color = "transparent")

  leaflet() |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      data = basins_sf,
      group = "Basins",
      fillColor = ~ pal(fish_index),
      fillOpacity = 0.7,
      color = "black",
      weight = 0.5,
      popup = ~ paste0("<b>Basin_ID:</b> ", Basin_ID, "<br><b>Fish index:</b> ", round(fish_index, 3))
    ) |>
    addPolygons(
      data = fish_sf,
      group = "Fish ranges",
      fillOpacity = 0.01,
      fillColor = "red",
      color = "red",
      weight = 1,
      popup = ~ paste0("<b>Species:</b> ", sci_name)
    ) |>
    addLegend(
      group = "Basins",
      pal = pal,
      values = basins_sf$fish_index,
      title = "Basin fish index",
      position = "bottomright"
    ) |>
    addLegend(colors = "red", labels = "Fish range", title = "Fish layer", position = "bottomleft") |>
    addLayersControl(overlayGroups = c("Basins", "Fish ranges"), options = layersControlOptions(collapsed = FALSE))
}
# ----------------------------
# END DEBUG FIGURE
# ----------------------------
