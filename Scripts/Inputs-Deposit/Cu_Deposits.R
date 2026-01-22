# Copper deposits data processing
# Source: S&P Data - Copper as primary product
# PBH Dec 2025

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# LOAD --------
# Load original data from S&P
df <- read_excel('Inputs/SP/SP_Copper_10Dec2025.xls', skip = 3)
df <- df |> slice(-1, -2)
head(df)
nrow(df) # 4999
names(df)

# Active vs inactive
table(df$ACTV_STATUS) # 1971 active


# CONVERSION -----

df <- df |>
  rename(Name = PROP_NAME) |>
  rename(ID = PROP_ID) |>
  rename(country = COUNTRY_NAME) |>
  mutate(OPEX = as.numeric(TOTAL_CASH_COST_LB) * 2204.62 / 100) |> # from cents/lb to usd/ton (metric)
  rename(reserves = CONTAINED_RESV_PCT_TONNE) |> # tons Cu
  mutate(reserves = as.numeric(reserves)) |>
  rename(grade_reserves = GRD_RESV_PCT_TONNE) |> # %Cu
  mutate(grade_reserves = as.numeric(grade_reserves)) |>
  rename(resources = CONTAINED_R_AND_R_PCT_TONNE) |> # Resources including reserves
  mutate(resources = as.numeric(resources)) |>
  rename(grade_resource = GRD_R_AND_R_PCT_TONNE) |> # Resources including reserves
  mutate(grade_resource = as.numeric(grade_resource)) |>
  rename(prod2025 = COMMODITY_PRODUCTION_TONNE_BY_PERIOD...19) |> # tons/year
  mutate(prod2025 = as.numeric(prod2025)) |>
  rename(prodCap = PRODUCTION_CAPACITY_TONNE) |> # tons /year
  mutate(prodCap = as.numeric(prodCap)) |>
  rename(grade_head = HEAD_GRD_PCT) |>
  mutate(grade_head = as.numeric(grade_head))


# Data check -------

# 880 deposits with resource data and in active status
df |> filter(!is.na(resources), ACTV_STATUS == "Active") |> nrow()
df |>
  filter(
    !is.na(resources),
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  nrow()


# Close to USGS numbers
sum(df$resources, na.rm = T) / 1e6 # 2.8 billion tons Cu vs 3.5 billion USGS
sum(df$reserves, na.rm = T) / 1e6 # 741 million tons Cu vs 1.0 billion USGS
sum(df$prod2025, na.rm = T) / 1e6 # 20.5 million tons Cu/year vs 22 million USGS
sum(df$prodCap, na.rm = T) / 1e6 # 32.5 million tons Cu/year


# Mine Type classification --------

# majority is open pit and underground
table(c(df$MINE_TYPE1, df$MINE_TYPE2, df$MINE_TYPE3)) |> sort(decreasing = T)
# Lots of NA
table(paste(df$MINE_TYPE1, df$MINE_TYPE2, df$MINE_TYPE3, sep = "-")) |> sort()
table(df$MINE_TYPE1)

# Either combined, open pit, underground, or Other (tailings, ...)
df <- df |>
  mutate(aux = paste0(MINE_TYPE1, "-", MINE_TYPE2, "-", MINE_TYPE3)) |>
  mutate(
    mine_type = case_when(
      str_detect(aux, "Open Pit") & str_detect(aux, "Underground") ~ "Combined",
      MINE_TYPE1 %in% c("Open Pit", "Underground") ~ MINE_TYPE1,
      MINE_TYPE2 %in% c("Open Pit", "Underground") ~ MINE_TYPE2,
      MINE_TYPE3 %in% c("Open Pit", "Underground") ~ MINE_TYPE3,
      T ~ "Other"
    )
  )
table(df$mine_type) # a lot of other
df |> group_by(mine_type) |> reframe(res = sum(resources, na.rm = T) / 1e6) # less others in active


table(df$PROCESSING_METHOD1) |> sort()


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
table(df$status) # most on exploration
df |> filter(ACTV_STATUS == "Active") |> group_by(status) |> tally()


# OPEX model ---------
# Fill linear regression model to predict missing OPEX data

sum(!is.na(df$OPEX)) # 286 with OPEX data


# Which variable could be a good predictor for OPEX
# 1177 have grade resource and no OPEX
df |>
  # filter(ACTV_STATUS=="Active") |>
  rename(pred = grade_resource) |>
  mutate(OPEX = is.na(OPEX), pred = is.na(pred)) |>
  group_by(OPEX, pred) |>
  tally() |>
  pivot_wider(names_from = OPEX, values_from = n)

# Classify countries for model
df |> filter(!is.na(OPEX)) |> group_by(country) |> tally() |> arrange(desc(n)) |> print(n = 100)


# fmt: skip
df <- df %>% mutate(
  country_agg = case_when(
    country %in% c("Brazil","Ecuador","Argentina","Colombia","Bolivia",
                   "Panama","Guatemala","Haiti","Nicaragua") ~ "South America",
    country %in% c("Canada","Dominican Republic","Greenland","Jamaica",
                   "Cuba","Honduras") ~ "North America",
    country %in% c("Bulgaria","Portugal","Serbia","Spain","Poland","Romania",
                   "Sweden","Finland","Albania","Cyprus","Ireland","Slovakia",
                   "North Macedonia","Norway","Bosnia & Herzegovina","Italy",
                   "Greece","Germany","Austria","United Kingdom","Ukraine",
                   "Hungary") ~ "Europe",
    country %in% c("Indonesia","Mongolia","Philippines","Kazakhstan","Russia",
                   "Kyrgyzstan","Laos","Papua New Guinea","India","Oman","Yemen",
                   "Türkiye","Uzbekistan","Afghanistan","Pakistan","Japan",
                   "Myanmar","New Zealand","North Korea","South Korea",
                   "Solomon Islands","Taiwan","Turkmenistan","Vietnam","Thailand",
                   "Malaysia","Cambodia","Azerbaijan","Georgia","Fiji","Vanuatu") ~ "Asia",
    country %in% c("Saudi Arabia","Iran","Armenia","Jordan","Israel") ~ "Middle East",
    country %in% c("Botswana","Mauritania","South Africa","Eritrea","Morocco",
                   "Algeria","Rep. Of the Congo","Angola","Uganda","Namibia",
                   "Egypt","Guinea","Zimbabwe","Sudan","Mozambique","Gabon",
                   "Tanzania","Madagascar","Kenya","Ethiopia","Nigeria") ~ "Africa",
    TRUE ~ country
  )
)
# Countries with more than 10 deposits are their own category
table(df$country_agg)

# Plot by cost type

data_opex <- df
names(data_opex) <- names(data_opex) |> str_remove_all("_CENTS/LB|_LB")
data_opex <- data_opex |>
  filter(!is.na(OPEX)) |>
  filter(OPEX > 0) |>
  dplyr::select(
    Name,
    ACTV_STATUS,
    country,
    mine_type,
    grade_resource,
    OPEX,
    MINESITE_LABOR_COST,
    MINESITE_FUEL_COST,
    MINESITE_REAGENTS_COST,
    MINESITE_ELEC_COST,
    MINESITE_OTHER_COST,
    TRANSPORT_AND_OFFSITE_COST,
    SMELTING_AND_REFINING_COST,
    ROYALTY_COST
  ) |>
  pivot_longer(
    c(
      MINESITE_LABOR_COST,
      MINESITE_FUEL_COST,
      MINESITE_REAGENTS_COST,
      MINESITE_ELEC_COST,
      MINESITE_OTHER_COST,
      TRANSPORT_AND_OFFSITE_COST,
      SMELTING_AND_REFINING_COST,
      ROYALTY_COST
    ),
    names_to = 'key',
    values_to = 'cost'
  ) |>
  mutate(cost = as.numeric(cost) * 2204.62 / 100) |> # to USD/ton
  mutate(key = key |> str_remove_all("_COST|MINESITE_"))

# electricity, labor and other dominate costs
ggplot(data_opex, aes(reorder(Name, OPEX), cost, fill = key)) +
  geom_col(col="black",linewidth=0.01) +
  # ggh4x::facet_grid2(country ~ ., scales = "free_y", axes = "all", space = "free_y", switch = "x") +
  ggforce::facet_col(facets = vars(country), scales = "free_y", space = "free") +
  coord_flip(expand = F) +
  labs(x = "", y = "OPEX (USD/tonne Cu)", fill = "") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 5),
    axis.text.y = element_text(size = 3),
    legend.text = element_text(size = 5)
  )

# fmt: skip
ggsave("Figures/Deposit/Copper_Cost_detail.png", ggplot2::last_plot(),
       units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*5)


# Predictive model
df <- df |> mutate(OPEX = if_else(OPEX > 0, OPEX, NA))
mod <- lm(OPEX ~ grade_resource + mine_type + country_agg, data = df)
nobs(mod) # 235
summary(mod) # R2=0.2

coefs <- broom::tidy(mod) |>
  dplyr::select(term, estimate) |>
  mutate(country_agg = str_remove(term, "country_agg")) |>
  mutate(mine_type = str_remove(term, "mine_type"))
head(coefs)
base_coef <- coefs[1, 2]$estimate
grade_coef <- coefs[2, 2]$estimate
# to add to model later
coef_country <- coefs |>
  filter(str_detect(term, "country")) |>
  rbind(tibble(term = "x", estimate = 0, country_agg = "Africa", mine_type = "x")) |>
  rename(addCountry = estimate) |>
  mutate(term = NULL, mine_type = NULL)
coef_mine <- coefs |>
  filter(str_detect(term, "mine")) |>
  rbind(tibble(term = "x", estimate = 0, mine_type = "Combined", country_agg = "x")) |>
  rename(addMine = estimate) |>
  mutate(term = NULL, country_agg = NULL)


ggplot(df, aes(grade_resource, OPEX)) +
  geom_point(aes(col=country_agg)) +
  # geom_smooth(aes(col=mine_type))+
  labs(x = "Resource Grade (% Cu)", y = "OPEX\n(USD/tonne Cu)", col = "")

# Data filling strategy,
df <- df |>
  mutate(OPEX_orig = OPEX) |>
  mutate(
    OPEX_source = case_when(
      !is.na(OPEX) ~ "S&P",
      !is.na(grade_resource) ~ "Based on grade, country, mine type",
      T ~ "Based on country, mine type"
    )
  ) |>
  left_join(coef_country) |>
  left_join(coef_mine) |>
  mutate(
    OPEX = case_when(
      !is.na(OPEX) ~ OPEX,
      !is.na(grade_resource) ~ base_coef + grade_coef * grade_resource + addCountry + addMine,
      T ~ base_coef + addCountry + addMine # assumes 0% grade, to penalize not having a grade estimate
    )
  )

sum(is.na(df$OPEX))
table(df$OPEX_source)

df |>
  group_by(country_agg) |>
  mutate(OPEX_avg = mean(OPEX)) |>
  ungroup() |>
  filter(
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  filter(MINE_TYPE1 != "Ocean" | is.na(MINE_TYPE1)) |>
  ggplot(aes(x = reorder(country_agg, OPEX_avg), y = OPEX)) +
  geom_boxplot(aes(fill = country_agg), alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(col=country_agg),position = position_jitter(width = 0.2), alpha=0.3) +
  coord_flip(expand = F) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, 16) * 1e3) +
  labs(y = "OPEX\n(USD/tonne Cu)", x = "") +
  theme(legend.position = "none", axis.text.x = element_text(hjust = 1))

# fmt: skip
ggsave("Figures/Deposit/Cu_Opex.png", ggplot2::last_plot(),
       units = 'cm', dpi = 600, width = 8.7*2.5, height = 8.7*2.5)

# CAPEX model ------

# Based on historical capital costs
# Database comes with 3 columns of capital costs
capex <- df |>
  # only consider first investment
  filter(CAPITAL_COST_TYPE...44 == "Initial Capital Cost") |>
  # consider further expansions towards existing capacity
  filter(CAPITAL_COST_TYPE...45 %in% c("Initial Capital Cost", "Expansion") | is.na(CAPITAL_COST_TYPE...45)) |>
  filter(CAPITAL_COST_TYPE...46 %in% c("Initial Capital Cost", "Expansion") | is.na(CAPITAL_COST_TYPE...46)) |>
  mutate(
    cost1 = as.numeric(AMT_CAPITAL_INVESTED...41),
    cost1 = if_else(is.na(cost1), 0, cost1),
    cost2 = as.numeric(AMT_CAPITAL_INVESTED...42),
    cost2 = if_else(is.na(cost2), 0, cost2),
    cost3 = as.numeric(AMT_CAPITAL_INVESTED...43),
    cost3 = if_else(is.na(cost3), 0, cost3),
    capCost = cost1 + cost2 + cost3,
    capM = capCost / 1e3, # for figure # capCost is in thousand USD)
    prodK = prodCap / 1e3 # prodCap is in tonnes/year
  )

p <- ggplot(capex, aes(prodK, capM, col = mine_type)) +
  geom_point(alpha=0.7) +
  # geom_smooth(method="lm",se=F,formula="y~x") +
  coord_cartesian(expand = F, xlim = c(0, max(capex$prodK, na.rm = T) * 1.05), ylim = c(0, max(capex$capM) * 1.05)) +
  labs(
    x = "Production Capacity (thousand tonnes Cu/year)",
    y = "",
    title = "Capital Cost (million USD)",
    col = "Mine Type"
  ) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  geom_segment(x = 0, xend = 50, y = 500, yend = 500, col = "black", linetype = "dashed", linewidth = 0.5) +
  geom_segment(x = 50, xend = 50, y = 0, yend = 500, col = "black", linetype = "dashed", linewidth = 0.5) +
  theme(legend.position = c(0.1, 0.8), legend.box.background = element_rect(colour = "black"))
p

# zoom version
p_zoom <- ggplot(capex, aes(prodK, capM, col = mine_type)) +
  geom_point(alpha=0.7) +
  coord_cartesian(expand = F, xlim = c(0, 50), ylim = c(0, 500)) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  theme(legend.position = "none") +
  labs(x = "", y = "")

library(cowplot)
ggdraw() + draw_plot(p) + draw_plot(p_zoom, x = 0.55, y = 0.55, width = 0.38, height = 0.38)

# fmt: skip
ggsave("Figures/Deposit/Cu_CAPEX.png", ggplot2::last_plot(),
       units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

mod_capex <- lm(capCost ~ prodCap:mine_type, data = capex)
nobs(mod_capex) # 324
summary(mod_capex) # R2=0.29
# plot(mod_capex)

# Fill capex for all projects
coefs_CAPEX <- broom::tidy(mod_capex) |>
  dplyr::select(term, estimate) |>
  mutate(mine_type = str_remove(term, "prodCap:mine_type")) |>
  rename(capex_est = estimate)
head(coefs_CAPEX)
base_capex <- coefs_CAPEX[1, 2]$capex_est / 1e3 # million USD

table(df$status)
df <- df |>
  left_join(coefs_CAPEX) |>
  mutate(
    # sunk cost considered
    CAPEX_opening = if_else(status %in% c("Production", "Development"), 0, base_capex), # opening cost, in million USD
    CAPEX_exp = capex_est * 1e3, # expansion cost, in USD per tpa (ton per year)
  )

# Baseline capacity production -------

df <- df |>
  mutate(
    cap2025 = case_when(
      is.na(prodCap) & is.na(prod2025) ~ 0,
      is.na(prodCap) & !is.na(prod2025) ~ prod2025,
      is.na(prod2025) & !is.na(prodCap) ~ prodCap,
      prodCap > prod2025 ~ prodCap,
      T ~ prod2025
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

# Filter and save ---------
names(df)
df_save <- df |>
  filter(
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  filter(!is.na(resources)) |>
  filter(MINE_TYPE1 != "Ocean" | is.na(MINE_TYPE1)) |>
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
    reserves,
    grade_reserves,
    resources,
    grade_resource,
    cap2025,
    grade_head,
    OPEX,
    OPEX_source,
    CAPEX_opening,
    CAPEX_exp,
    delay_years
  ) |>
  mutate(reserves = if_else(is.na(reserves), 0, reserves))


nrow(df_save) # 901
write.csv(df_save, "Parameters/Intermediate/Cu_Deposit_SP.csv", row.names = F)

# EoF
