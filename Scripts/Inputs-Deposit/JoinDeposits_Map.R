# Join deposits for the map purposes
# Source: S&P Data
# PBH Jan 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# LOAD AND MERGE -----------

cu1 <- read_excel('Inputs/SP/SP_Copper_28Jan2026_USA_Europe.xls', skip = 3, col_types = "text") |> slice(-1, -2)
cu2 <- read_excel('Inputs/SP/SP_Copper_28Jan2026_ROW.xls', skip = 3, col_types = "text") |> slice(-1, -2)
ni <- read_excel('Inputs/SP/SP_Nickel_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)
co <- read_excel('Inputs/SP/SP_Cobalt_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)
li <- read_excel('Inputs/SP/SP_Lithium_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)
# fix Li names
Li_names <- read_excel('Inputs/SP/NamesLi_correction.xlsx')
names(li) <- Li_names$Correct

# Select and join
df <- rbind(
  mutate(
    dplyr::select(
      cu1,
      PROP_ID,
      PRIMARY_COMMODITY,
      LATITUDE,
      LONGITUDE,
      CONTAINED_R_AND_R_PCT_TONNE,
      GRD_R_AND_R_PCT_TONNE...30,
      ACTV_STATUS,
      MINE_TYPE1,
      MINE_TYPE2,
      MINE_TYPE3
    ),
    Mineral = "Copper"
  ),
  mutate(
    dplyr::select(
      cu2,
      PROP_ID,
      PRIMARY_COMMODITY,
      LATITUDE,
      LONGITUDE,
      CONTAINED_R_AND_R_PCT_TONNE,
      GRD_R_AND_R_PCT_TONNE...30,
      ACTV_STATUS,
      MINE_TYPE1,
      MINE_TYPE2,
      MINE_TYPE3
    ),
    Mineral = "Copper"
  ),
  mutate(
    dplyr::select(
      ni,
      PROP_ID,
      PRIMARY_COMMODITY,
      LATITUDE,
      LONGITUDE,
      CONTAINED_R_AND_R_PCT_TONNE,
      GRD_R_AND_R_PCT_TONNE...30,
      ACTV_STATUS,
      MINE_TYPE1,
      MINE_TYPE2,
      MINE_TYPE3
    ),
    Mineral = "Nickel"
  ),
  mutate(
    dplyr::select(
      co,
      PROP_ID,
      PRIMARY_COMMODITY,
      LATITUDE,
      LONGITUDE,
      CONTAINED_R_AND_R_PCT_TONNE,
      GRD_R_AND_R_PCT_TONNE...30,
      ACTV_STATUS,
      MINE_TYPE1,
      MINE_TYPE2,
      MINE_TYPE3
    ),
    Mineral = "Cobalt"
  )
)

# Lithium requires extra work for the filtering only
li <- li |>
  dplyr::select(
    PROP_ID,
    PRIMARY_COMMODITY,
    LATITUDE,
    LONGITUDE,
    CONTAINED_R_AND_R_PCT_TONNE,
    GRD_R_AND_R_PCT_TONNE,
    ACTV_STATUS,
    MINE_TYPE1,
    MINE_TYPE2,
    MINE_TYPE3,
    R_AND_R_ORE_VOLUME,
    CONCENTRATION_MG_LITER
  ) |>
  # only to avoid NA
  mutate(
    GRD_R_AND_R_PCT_TONNE...30 = if_else(
      !is.na(GRD_R_AND_R_PCT_TONNE) |
        !is.na(R_AND_R_ORE_VOLUME) |
        !is.na(CONCENTRATION_MG_LITER) |
        !is.na(CONTAINED_R_AND_R_PCT_TONNE),
      '1',
      GRD_R_AND_R_PCT_TONNE
    )
  ) |>
  mutate(Mineral = "Lithium") |>
  dplyr::select(-R_AND_R_ORE_VOLUME, -CONCENTRATION_MG_LITER, -GRD_R_AND_R_PCT_TONNE)

df <- rbind(df, li)

sum(!is.na(df$CONTAINED_R_AND_R_PCT_TONNE))


# Filter by status and primary commodity
df <- df |>
  rename(resources = CONTAINED_R_AND_R_PCT_TONNE) |> # Resources including reserves
  mutate(resources = as.numeric(resources)) |>
  filter(!is.na(resources)) |>
  rename(grade_resources = GRD_R_AND_R_PCT_TONNE...30) |>
  mutate(grade_resources = as.numeric(grade_resources)) |>
  filter(!is.na(grade_resources)) |>
  filter(
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  filter(PROP_ID != 88695) |> # spefici mine in the ocena
  # filter(PRIMARY_COMMODITY == Mineral) |> # Keep only one mine to show on the map
  mutate(aux = paste0(MINE_TYPE1, MINE_TYPE2, MINE_TYPE3)) |>
  filter(!str_detect(aux, "Ocean|Dredging")) |>
  mutate(aux = NULL) |>
  mutate(
    PRIMARY_COMMODITY = if_else(
      PRIMARY_COMMODITY %in% c("Copper", "Nickel", "Cobalt", "Lithium"),
      PRIMARY_COMMODITY,
      "Other"
    )
  )
# show multiple based on the contennt...
table(df$MINE_TYPE1)

# df <- df |> filter(MINE_TYPE1 %in% c("Brine", "Underground", "Open Pit", "Stock Pile", "Tailings", "Dump"))

nrow(df) # 986 or 1669
length(unique(df$PROP_ID)) # 1795 unique
table(df$PRIMARY_COMMODITY, df$Mineral)
df |> filter(Mineral == "Lithium") |> group_by(PRIMARY_COMMODITY) |> summarise(n = n()) # 165 for lithium
df |> group_by(Mineral) |> summarise(total_resources = sum(resources) / 1e6) # in million tons

write.csv(df, "Parameters/Deposits_Map.csv", row.names = F)
