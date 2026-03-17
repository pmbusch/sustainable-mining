# Lithium deposits data processing
# Source: S&P Data - Lithium as primary product only, assume no co-production
# PBH Jan 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# LOAD --------
# Load original data from S&P - splitted in 3 files due to size limits
df <- read_excel('Inputs/SP/SP_Lithium_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)
# fix Li names
Li_names <- read_excel('Inputs/SP/NamesLi_correction.xlsx')
(names(df) <- Li_names$Correct)

head(df)
nrow(df) # 715

# CONVERSION -----

# Different columns for different mine types (brine, hard rock, clay)
# ALL IS CONVERTED TO LITHIUM METAL, including grade (% Li, from 0 to 100)
df <- df |>
  rename(Name = PROP_NAME) |>
  rename(ID = PROP_ID) |>
  rename(country = COUNTRY_NAME) |>
  # Reserves and resources, include ore
  rename(reserves = CONTAINED_RESV_PCT_TONNE) |> # original: tons Li2O
  mutate(reserves = as.numeric(reserves) / 2.153) |>
  rename(resources = CONTAINED_R_AND_R_PCT_TONNE) |> # Resources including reserves, original is tons Li2O
  mutate(resources = as.numeric(resources) / 2.153) |>
  rename(resources_ore = R_AND_R_ORE_TONNAGE) |>
  mutate(resources_ore = as.numeric(resources_ore)) |> # tonnes ore
  rename(resources_ore_volume = R_AND_R_ORE_VOLUME) |>
  mutate(resources_ore_volume = as.numeric(resources_ore_volume)) |> # m3 ore
  # Grade
  rename(grade_reserves = GRD_RESV_PCT_TONNE) |> # original: %Li2O (lithium oxide), note Li conversion to Li2O is Li * 2.153
  mutate(grade_reserves = as.numeric(grade_reserves) / 2.153) |>
  rename(grade_resource = GRD_R_AND_R_PCT_TONNE) |> # original: %Li2O (lithium oxide)
  mutate(grade_resource = as.numeric(grade_resource) / 2.153) |>
  rename(grade_head = HEAD_GRADE) |>
  mutate(grade_head = as.numeric(grade_head) / 2.153) |> # original: % Li2O
  rename(grade_concentration = CONCENTRATION_MG_LITER) |>
  mutate(grade_concentration = as.numeric(grade_concentration) / 1e4) |> # original: mg/L Lithium; this is the grade for brine deposits
  # Costs - NOT CONVERTED HERE
  mutate(OPEX = as.numeric(TOTAL_CASH_COST)) |> # different units, brine is in USD/ton LCE and rest (hard rock) is in USD/ton ore processed
  mutate(OPEX_concentrate = as.numeric(TOTAL_CASH_COST_CONCENTRATE)) |> # USD per ton concentrate
  # Production or capacity
  rename(prodCap = PRODUCTION_CAPACITY_TONNE) |> # tons /year product my best guess is in LCE (original)
  mutate(prodCap = as.numeric(prodCap) / 5.323) |>
  rename(prod2025 = COMMODITY_PRODUCTION_TONNE_BY_PERIOD) |> # original: tons LCE/year
  mutate(prod2025 = as.numeric(prod2025) / 5.323) |>
  rename(ore_processed = ORE_PROCESSED_MASS_BY_PERIOD) |> # in 2025, for hard rock
  mutate(ore_processed = as.numeric(ore_processed)) |> # tonnes ore processed
  rename(brine_processed = PUMP_RATE_LITERS_MIN) |> # for brine deposits 2025
  mutate(brine_processed = as.numeric(brine_processed) / 1e3 * 60 * 24 * 365) |> # m3 per year
  rename(ore_cap = MILL_CAPACITY_TONNES_PER_YEAR) |>
  mutate(ore_cap = as.numeric(ore_cap)) |> # tonnes ore processed per year
  rename(brine_cap = MILL_CAPACITY_CUBIC_M_PER_YEAR) |>
  mutate(brine_cap = as.numeric(brine_cap)) |> # cubic m3 per year, but no data inside...
  # other
  mutate(RECOVERY_RATE = as.numeric(RECOVERY_RATE))

# Close to USGS numbers, convert from Li2O to Li or from LCE to Li
sum(df$resources, na.rm = T) / 1e6 # 168 million tons Li vs 115 million USGS
sum(df$reserves, na.rm = T) / 1e6 # 40 million tons Li vs 30 billion USGS
sum(df$prod2025, na.rm = T) / 1e3 # 276 ktons Li/year vs 240 ktons USGS
sum(df$prodCap, na.rm = T) / 1e3 # 663 ktons Li/year


# FILTER ---------------

# Filters, based on status, resource data availability, mine type
df_all <- df

# If no resources, no reserves
df |>
  mutate(resources_NA = is.na(resources), reserves_NA = is.na(reserves)) |>
  # mutate(resources_NA = is.na(grade_resource), reserves_NA = is.na(grade_reserves)) |>
  group_by(resources_NA, reserves_NA) |>
  tally() |>
  pivot_wider(names_from = reserves_NA, values_from = n)


df <- df |>
  filter(
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  filter(
    !is.na(resources) |
      !is.na(resources_ore) |
      !is.na(resources_ore_volume | !is.na(grade_resource) | !is.na(grade_concentration) | !is.na(grade_head))
  )

nrow(df) # 182 mines


# Table for data completeness
df |>
  reframe(
    n_opex = sum(!is.na(OPEX)),
    perc_opex = sum(!is.na(OPEX)) / n() * 100,
    n_reserves = sum(!is.na(reserves)),
    perc_reserves = sum(!is.na(reserves)) / n() * 100,
    n_resources = sum(!is.na(resources)),
    perc_resources = sum(!is.na(resources)) / n() * 100,
    n_prodcap = sum(!is.na(prodCap)),
    perc_prodcap = sum(!is.na(prodCap)) / n() * 100
  )


# CLASSIFICATION -----

## Mine Type classification --------

# Classify into brine, clay or hard rock

table(c(df$MINE_TYPE1, df$MINE_TYPE2, df$MINE_TYPE3)) |> sort(decreasing = T)
table(df$GEOLOGIC_ORE_BODY_TYPE) |> sort(decreasing = T) # brine, clay, hard rock ....
table(df$GEOLOGIC_ORE_BODY_TYPE, df$MINE_TYPE1)


df <- df |>
  mutate(
    mine_type = case_when(
      MINE_TYPE1 == "Brine" ~ "Brine",
      str_detect(Name, " Sal de") ~ "Brine",
      grade_concentration > 0 ~ "Brine",
      str_detect(GEOLOGIC_ORE_BODY_TYPE, "Clay|Evaporite") ~ "Clay",
      T ~ "Hard Rock"
    )
  )
table(df$mine_type) # majority is hard rock
df |> group_by(mine_type) |> reframe(res = sum(resources, na.rm = T) / 1e6) # a lot of resources in brines


table(df$PROCESSING_METHOD1) |> sort()
table(df$PROCESSING_METHOD2) |> sort()
table(df$PROCESSING_METHOD3) |> sort()
table(df$PROCESSING_METHOD1, df$mine_type)

# Classify the brine with DLE
df <- df |>
  mutate(
    mine_type = if_else(
      mine_type == "Brine" &
        PROCESSING_METHOD1 %in% c("Adsorption", "Ion Exchange", "Solvent Extraction", "Membrane Separation"),
      "Brine DLE",
      mine_type
    )
  )
table(df$mine_type)
df |> group_by(mine_type) |> reframe(res = sum(resources, na.rm = T) / 1e6) # a lot of resources in brines

# Status classification --------

table(df$DEV_STAGE) |> sort(decreasing = T)
# fmt: skip
df <- df %>%
  mutate(status = case_when(
      DEV_STAGE %in% c("Target Outline", "Grassroots", "Exploration", "Advanced Exploration") ~ "Exploration",
      DEV_STAGE %in% c("Prefeas/Scoping","Feasibility Started","Feasibility","Feasibility Complete","Reserves Development") ~ "Economic Studies",
      DEV_STAGE %in% c("Construction Planned", "Construction Started", "Commissioning", "Preproduction") ~ "Development",
      DEV_STAGE %in% c("Operating", "Expansion", "Satellite", "Limited Production", "Residual Production") ~ "Production",
      DEV_STAGE %in% c("Closed") ~ "Closed",
      TRUE ~ "Unclassified"))

table(df$status, df$DEV_STAGE)
table(df$status)

## Fix brine in different units ------
# simply assume brine density is 1 ton/m3 (water dens), actually does not mater as ton ore or m3 brine cancel outs in the opt model later
df <- df |>
  mutate(
    resources_ore = if_else(str_detect(mine_type, "Brine") & is.na(resources_ore), resources_ore_volume, resources_ore)
  ) |>
  mutate(ore_cap = if_else(str_detect(mine_type, "Brine") & is.na(ore_cap), brine_cap, ore_cap)) |>
  mutate(
    ore_processed = if_else(str_detect(mine_type, "Brine") & is.na(ore_processed), brine_processed, ore_processed)
  ) |>
  mutate(
    grade_resource = if_else(
      str_detect(mine_type, "Brine") & is.na(grade_resource),
      grade_concentration,
      grade_resource
    )
  )


df |>
  mutate(RECOV_RATE = as.numeric(RECOV_RATE), RECOV_RATE_BY_PERIOD = as.numeric(RECOV_RATE_BY_PERIOD)) |>
  group_by(mine_type) |>
  reframe(RECOV_RATE = mean(RECOV_RATE, na.rm = T), RECOV_RATE_BY_PERIOD = mean(RECOV_RATE_BY_PERIOD, na.rm = T))
# 70% to 80%

# OPEX model ---------
# Fill linear regression model to predict missing OPEX data

# No OPEX for clay
df |> group_by(mine_type) |> reframe(hasOPEX = sum(!is.na(OPEX)), n = n(), perc = hasOPEX / n * 100)


# Convert all to OPEX per ton ore (or cubic meter) processed
# Hard rock: OPEX in USD per ton ore processed, so we assume all is converted to

conv_cost_SC6_LCE <- 2500 # conversion cost assumed from SC6 (spodueme concentrate at 6% Li2O) to LCE 99% (battery grade)
# Note that model by ore assures that all costs are comparable in terms of mass per lithium, the conversion is used to account for differnt products
# Assume clay follows hard rock route

df <- df |>
  mutate(
    OPEX_ore = if_else(
      # note that we convert from /LCE to /Li, then uses the recovery rate and concentration to reverse-engineer the cost per raw ore (cubic meter of brine)
      str_detect(mine_type, "Brine"),
      OPEX * 5.323 * RECOVERY_RATE / 100 * grade_concentration / 100,
      # To fit the model we downscale the conversion cost to per ton ore processed
      OPEX + conv_cost_SC6_LCE * 5.323 * RECOVERY_RATE / 100 * grade_head / 100,
    )
  )


df |>
  filter(!is.na(OPEX_ore)) |>
  group_by(mine_type, country) |>
  tally() |>
  pivot_wider(names_from = mine_type, values_from = n)

# Classify based on limited data
df <- df |>
  mutate(
    model_class = case_when(
      mine_type == "Hard Rock" &
        country %in%
          c(
            "Australia",
            "Germany",
            "United Kingdom",
            "Spain",
            "France",
            "Finland",
            "Canada",
            "Czechia",
            "Austria",
            "Portugal",
            "Italy",
            "USA"
          ) ~ "Hard Rock Developed Nations",
      mine_type == "Hard Rock" ~ "Hard Rock RoW",
      T ~ "Brine"
    )
  )


# Predictive model
mod <- lm(OPEX_ore ~ model_class, data = df)
nobs(mod) # 36
summary(mod) # R2=0.83


# Fit model to rest of deposits
df <- df |>
  mutate(OPEX_orig = OPEX_ore) |>
  mutate(OPEX_source = case_when(!is.na(OPEX_ore) ~ "S&P", T ~ "Fitted Model")) |>
  mutate(
    OPEX_ore = if_else(
      is.na(OPEX_ore),
      # use 95% upper bound to be conservative of not opened mines yet
      predict(mod, newdata = pick(model_class), interval = "confidence", level = 0.95)[, "upr"],
      OPEX_ore
    )
  )

data_fig <- df |> group_by(model_class) |> mutate(OPEX_avg = mean(OPEX_ore)) |> ungroup()

range(df$OPEX_ore)
ggplot(data_fig, aes(x = reorder(model_class, OPEX_avg), y = OPEX_ore)) +
  geom_boxplot(aes(fill = OPEX_source), alpha = 0.8, outlier.shape = NA) +
  geom_point( data = filter(data_fig,OPEX_source == "Fitted Model"),
  position = position_nudge(x = -0.2),aes(col=mine_type), alpha=0.6) +
  geom_point( data = filter(data_fig,OPEX_source != "Fitted Model"),
  position = position_nudge(x = 0.2),aes(col=mine_type), alpha=0.6) +
  coord_flip(expand = F) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, 225)) +
  scale_fill_manual(values = c("S&P" = "#ccebc5", "Fitted Model" = "#fbb4ae")) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(y = "OPEX\n(USD per ton ore processed or m3 brine processed)", x = "", fill = "Data Source", col = "Mine type") +
  theme_pb_wide() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.8, 0.2), axis.text.x = element_text(hjust = 1))


# fmt: skip
ggsave("Figures/Deposit/Lithium/Li_Opex.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

# CAPEX model ------

# Based on historical capital costs
# Database comes with 3 columns of capital costs
table(df$CAPITAL_COST_TYPE_1)
capex <- df |>
  # only consider first investment
  filter(CAPITAL_COST_TYPE_1 == "Initial Capital Cost") |>
  # consider further expansions towards existing capacity
  filter(CAPITAL_COST_TYPE_2 %in% c("Initial Capital Cost", "Expansion") | is.na(CAPITAL_COST_TYPE_2)) |>
  filter(CAPITAL_COST_TYPE_3 %in% c("Initial Capital Cost", "Expansion") | is.na(CAPITAL_COST_TYPE_3)) |>
  mutate(mine_type = if_else(str_detect(mine_type, "Brine"), "Brine", mine_type)) |>
  mutate(ore_cap = if_else(mine_type == "Brine", brine_processed, ore_cap)) |>
  mutate(
    cost1 = as.numeric(AMT_CAPITAL_INVESTED_1),
    cost1 = if_else(is.na(cost1), 0, cost1),
    cost2 = as.numeric(AMT_CAPITAL_INVESTED_2),
    cost2 = if_else(is.na(cost2), 0, cost2),
    cost3 = as.numeric(AMT_CAPITAL_INVESTED_3),
    cost3 = if_else(is.na(cost3), 0, cost3),
    capCost = cost1 + cost2 + cost3,
    capM = capCost / 1e3, # for figure # capCost is in thousand USD)
    prodK = ore_cap / 1e3 # prodCap is in tonnes/year
  ) |>
  filter(mine_type != "Clay")

p <- ggplot(capex, aes(prodK, capM, col = mine_type)) +
  geom_point(alpha=0.7) +
  geom_smooth(method="lm",se=F,formula="y~x") +
  coord_cartesian(expand = F, xlim = c(0, max(capex$prodK, na.rm = T) * 1.05), ylim = c(0, max(capex$capM) * 1.05)) +
  labs(
    x = "Production Capacity (thousand tonnes Li/year)",
    y = "",
    title = "Capital Cost (million USD)",
    col = "Resource Type"
  ) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  # geom_segment(x = 0, xend = 50, y = 500, yend = 500, col = "black", linetype = "dashed", linewidth = 0.5) +
  # geom_segment(x = 50, xend = 50, y = 0, yend = 500, col = "black", linetype = "dashed", linewidth = 0.5) +
  theme_pb_wide() +
  theme(legend.position = c(0.1, 0.8), legend.box.background = element_rect(colour = "black"))
p

# fmt: skip
ggsave("Figures/Deposit/Lithium/Li_CAPEX.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)


# Linear model: directly interpretable
# Separate model per mine type
mod_capex_hr <- lm(capCost ~ ore_cap, data = filter(capex, mine_type == "Hard Rock"))
nobs(mod_capex_hr) # 35
summary(mod_capex_hr) # R2=0.04 # POOR FIT...
# plot(mod_capex)

mod_capex_br <- lm(capCost ~ brine_processed, data = filter(capex, mine_type == "Brine"))
nobs(mod_capex_br) # 8
summary(mod_capex_br) # R2=0.07 # POOR FIT...


# Fill capex for all projects
table(df$status)
df <- df |>
  mutate(
    # sunk cost considered
    CAPEX_opening = case_when(
      status %in% c("Production", "Development") ~ 0,
      str_detect(mine_type, "Brine") ~ unname(coef(mod_capex_br)[1]) / 1e3,
      T ~ unname(coef(mod_capex_hr)[1]) / 1e3
    ), # opening cost, in million USD
    CAPEX_exp = if_else(str_detect(mine_type, "Brine"), unname(coef(mod_capex_br)[2]), unname(coef(mod_capex_hr)[2])) *
      1e3, # expansion cost, in USD per tpa (ton per year)
  )


# Baseline capacity production -------

df <- df |>
  mutate(
    cap2025 = case_when(
      is.na(ore_cap) & is.na(ore_processed) & is.na(brine_processed) ~ 0, # no data either on capacity or reported production, so zero
      is.na(ore_cap) & !is.na(ore_processed) ~ ore_processed, # only production data available
      is.na(ore_cap) & !is.na(brine_processed) ~ brine_processed, # only production data available
      is.na(ore_processed) & !is.na(ore_cap) ~ ore_cap, # only capacity data available
      ore_cap > ore_processed ~ ore_cap, # capacity higher than production, use capacity
      T ~ ore_processed
    )
  )


# Delays in openings based on status ------
table(df$status)
df <- df |>
  mutate(
    delay_years = case_when(
      status == "Exploration" ~ 8,
      status == "Economic Studies" ~ 5,
      status == "Development" ~ 2,
      status == "Production" ~ 0,
      T ~ 7
    )
  )

#

# Filter and save ---------
names(df)
df_save <- df |>
  dplyr::select(
    Name,
    ID,
    country,
    LATITUDE,
    LONGITUDE,
    ACTV_STATUS,
    status,
    DEV_STAGE,
    mine_type,
    reserves, # tons
    grade_reserves, # in % 0 to 100
    resources, # tons
    grade_resource, # in % 0 to 100
    resources_ore, # tons ore
    ore_processed, # tons ore processed in 2025
    prod2025, # tons Li 2025
    cap2025, # tons ore per year
    grade_head,
    OPEX_ore, # USD per ton ore processed
    OPEX_source,
    CAPEX_opening, # million USD
    CAPEX_exp, # USD per tpa ore added
    delay_years,
    PRIMARY_COMMODITY
  ) |>
  mutate(share_NiCoCu = 1, Mineral = "Lithium") # all lithium only
df_save$cap2026 <- df_save$cap2027 <- df_save$cap2028 <- df_save$cap2029 <- df_save$cap2030 <- df_save$cap2031 <- df_save$cap2032 <- df_save$cap2025

# fill NA
names(df_save)
df_save <- df_save |>
  mutate(across(all_of(c("grade_reserves", "grade_head", "ore_processed", "prod2025")), ~ replace_na(.x, 0))) |>
  pivot_wider(
    names_from = Mineral,
    values_from = c(reserves, grade_reserves, resources, grade_resource, grade_head, prod2025),
    names_sep = "_",
    values_fill = 0
  )

nrow(df_save) # 182

# Do resources, ore and grades make sense? NOT ALWAYS
df_save |>
  mutate(est = resources_ore * grade_resource_Lithium / 100) |>
  mutate(abs_diff = abs(est - resources_Lithium) / 1e6) |>
  dplyr::select(Name, ID, est, resources_Lithium, abs_diff) |>
  arrange(desc(abs_diff))
# Not really, approach, fix grade based on ore and resources
# These approach assumes resources are correct

# First lets check completeness
df_save |>
  mutate(
    has_resources = !is.na(resources_Lithium),
    has_grade = !is.na(grade_resource_Lithium),
    has_ore = !is.na(resources_ore)
  ) |>
  group_by(has_resources, has_grade, has_ore) |>
  tally()

# Do it by case
# 1. No resources, No Ore and No Grade -> filter out
# 2. Only has ore but has head grade -> use head grade as grade resource and to estimate resources
# 3. Only has grade -> filter out
# 4. Only has resources -> filter out
# 5. ore and resources -> calculate grade
# 6. resources and grade -> calculate ore
# 7. has all...
df_save <- df_save |>
  filter(!is.na(resources_Lithium) | !is.na(resources_ore) | !is.na(grade_resource_Lithium)) |> # case 1
  filter(!is.na(resources_Lithium) | !is.na(resources_ore)) |> # case 3
  filter(!is.na(grade_resource_Lithium) | !is.na(resources_ore)) |> # case 4
  mutate(
    resources_ore = case_when(
      !is.na(resources_ore) ~ resources_ore,
      is.na(resources_ore) & !is.na(resources_Lithium) & !is.na(grade_resource_Lithium) ~ resources_Lithium /
        (grade_resource_Lithium / 100), # case 6
      T ~ NA_real_
    ),
    grade_resource_Lithium = case_when(
      !is.na(grade_resource_Lithium) ~ grade_resource_Lithium,
      is.na(grade_resource_Lithium) & !is.na(resources_Lithium) & !is.na(resources_ore) ~ resources_Lithium /
        resources_ore *
        100, # case 5
      is.na(grade_resource_Lithium) & !is.na(grade_head_Lithium) ~ grade_head_Lithium, # case 2
      T ~ NA_real_
    ),
    resources_Lithium = case_when(
      !is.na(resources_Lithium) ~ resources_Lithium,
      is.na(resources_Lithium) & !is.na(resources_ore) & !is.na(grade_resource_Lithium) ~ resources_ore *
        grade_resource_Lithium /
        100,
      T ~ NA_real_
    )
  )

#  fix grade based on ore and resources
df_save <- df_save |>
  mutate(grade_resource_Lithium = if_else(resources_Lithium > 0, (resources_Lithium / resources_ore) * 100, 0))

# last filter
df_save <- df_save |> filter(resources_Lithium > 0)

# Add assumptions on recovery rate and max depletion rate
df_save <- df_save |>
  mutate(recovery_rate_Lithium = if_else(str_detect(mine_type, "Brine"), 0.8, 0.7)) |>
  mutate(max_depletion_rate = if_else(mine_type == "Brine", 0.02, 0.04))

# Save
df_save <- df_save |> arrange(ID)
nrow(df_save) # 151
write.csv(df_save, "Parameters/Intermediate/Li_Deposit_SP.csv", row.names = F)

# EoF
