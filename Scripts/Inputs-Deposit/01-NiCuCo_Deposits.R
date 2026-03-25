# Copper,Nickel and Cobalt deposits data processing
# Source: S&P Data
# PBH Dec 2025

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# LOAD --------
# Load original data from S&P - Cu splitted in 2 files due to size limits
cu1 <- read_excel('Inputs/SP/SP_Copper_28Jan2026_USA_Europe.xls', skip = 3, col_types = "text") |> slice(-1, -2)
cu2 <- read_excel('Inputs/SP/SP_Copper_28Jan2026_ROW.xls', skip = 3, col_types = "text") |> slice(-1, -2)
ni <- read_excel('Inputs/SP/SP_Nickel_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)
co <- read_excel('Inputs/SP/SP_Cobalt_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)

df <- rbind(
  mutate(cu1, Mineral = "Copper"),
  mutate(cu2, Mineral = "Copper"),
  mutate(ni, Mineral = "Nickel"),
  mutate(co, Mineral = "Cobalt")
)
rm(cu1, cu2, ni, co)

table(df$PRIMARY_COMMODITY) |> sort(decreasing = T)

# CONVERSION -----
df <- df |>
  rename(Name = PROP_NAME) |>
  rename(ID = PROP_ID) |>
  rename(country = COUNTRY_NAME) |>
  mutate(OPEX_metal = as.numeric(TOTAL_CASH_COST_LB) * 2204.62 / 100) |> # from cents/lb to usd/ton (metric)
  mutate(OPEX_ore = as.numeric(TOTAL_CASH_COST_TONNE)) |> # usd per ton ore processed
  rename(reserves = CONTAINED_RESV_PCT_TONNE) |> # tons Cu
  mutate(reserves = as.numeric(reserves)) |>
  rename(grade_reserves = GRD_RESV_PCT_TONNE) |> # %Cu
  mutate(grade_reserves = as.numeric(grade_reserves)) |>
  rename(resources = CONTAINED_R_AND_R_PCT_TONNE) |> # Resources including reserves
  mutate(resources = as.numeric(resources)) |>
  rename(grade_resource = GRD_R_AND_R_PCT_TONNE...30) |> # Resources including reserves
  mutate(grade_resource = as.numeric(grade_resource)) |>
  rename(resources_ore = R_AND_R_ORE_TONNAGE) |>
  mutate(resources_ore = as.numeric(resources_ore)) |> # tonnes ore
  rename(prod2025 = COMMODITY_PRODUCTION_TONNE_BY_PERIOD) |> # tons/year
  mutate(prod2025 = as.numeric(prod2025)) |>
  rename(prodCap = PRODUCTION_CAPACITY_TONNE) |> # tons /year
  mutate(prodCap = as.numeric(prodCap)) |>
  rename(grade_head = HEAD_GRD_PCT) |>
  mutate(grade_head = as.numeric(grade_head)) |>
  rename(prodMetal = PAID_METAL_PRODUCED_KILOTONNES) |>
  mutate(prodMetal = as.numeric(prodMetal) * 1e3) |> # to tonnes
  rename(price_metal = COMMODITY_PRICE_LB) |>
  mutate(price_metal = as.numeric(price_metal) * 2204.62 / 100) |> # to USD/ton
  rename(ore_processed = ORE_PROCESSED_MASS_BY_PERIOD...24) |> # in 2025
  mutate(ore_processed = as.numeric(ore_processed)) |> # tonnes ore processed
  rename(ore_cap = MILL_CAPACITY_TONNES_PER_YEAR) |>
  mutate(ore_cap = as.numeric(ore_cap)) |> # tonnes ore processed per year
  rename(grossRevenue = NON_FERROUS_GROSS_REV) |>
  mutate(grossRevenue = as.numeric(grossRevenue) * 1e6) |> # USD
  # grade for other things
  rename(grResource_Zinc = GRD_R_AND_R_PCT_TONNE...75) |>
  mutate(grResource_Zinc = as.numeric(grResource_Zinc)) |>
  # from grade in grams per tonne to grade in %
  rename(grResource_Gold = GRD_R_AND_R_G_PER_TONNE...76) |>
  mutate(grResource_Gold = as.numeric(grResource_Gold) / 1e6 * 100) |>
  rename(grResource_Palladium = GRD_R_AND_R_G_PER_TONNE...77) |>
  mutate(grResource_Palladium = as.numeric(grResource_Palladium) / 1e6 * 100) |>
  rename(grResource_Platinum = GRD_R_AND_R_G_PER_TONNE...78) |>
  mutate(grResource_Platinum = as.numeric(grResource_Platinum) / 1e6 * 100) |>
  rename(grResource_Silver = GRD_R_AND_R_G_PER_TONNE...79) |>
  mutate(grResource_Silver = as.numeric(grResource_Silver) / 1e6 * 100)


# Other main primary minerals
df <- df |>
  mutate(primary_mineral = if_else(PRIMARY_COMMODITY %in% c("Copper", "Nickel", "Cobalt"), PRIMARY_COMMODITY, "Other"))
table(df$primary_mineral)


## Data check -------

df |> filter(!is.na(resources), ACTV_STATUS == "Active") |> nrow()
df |>
  filter(
    !is.na(resources),
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  nrow()


df |>
  group_by(Mineral) |>
  reframe(
    resources = sum(resources, na.rm = T) / 1e6,
    reserves = sum(reserves, na.rm = T) / 1e6,
    prod2025 = sum(prod2025, na.rm = T) / 1e6,
    prodCap = sum(prodCap, na.rm = T) / 1e6
  )
# Close to USGS numbers

# Copper
# 3 billion tons Cu vs 3.5 billion USGS
# 800 million tons Cu vs 1.0 billion USGS
# 21.4 million tons Cu/year vs 22 million USGS
# 35.6 million tons Cu/year

# mean recovery grade
df |>
  mutate(RECOV_RATE = as.numeric(RECOV_RATE), RECOV_RATE_BY_PERIOD = as.numeric(RECOV_RATE_BY_PERIOD)) |>
  group_by(Mineral) |>
  reframe(RECOV_RATE = mean(RECOV_RATE, na.rm = T), RECOV_RATE_BY_PERIOD = mean(RECOV_RATE_BY_PERIOD, na.rm = T))
# 70% to 80%

# weighted by production 2025
df |>
  mutate(RECOV_RATE = as.numeric(RECOV_RATE), RECOV_RATE_BY_PERIOD = as.numeric(RECOV_RATE_BY_PERIOD)) |>
  # mutate(prod2025 = reserves) |>
  filter(!is.na(prod2025)) |>
  group_by(Mineral) |>
  reframe(
    w.RECOV_RATE = weighted.mean(RECOV_RATE, w = prod2025, na.rm = T),
    w.RECOV_RATE_BY_PERIOD = weighted.mean(RECOV_RATE_BY_PERIOD, w = prod2025, na.rm = T)
  )
# Cu 81-88%, Ni 74-80%, Co 69-71%

# Quantiles of recovery grade (weigthed)
library(Hmisc)
df |>
  mutate(RECOV_RATE = as.numeric(RECOV_RATE), RECOV_RATE_BY_PERIOD = as.numeric(RECOV_RATE_BY_PERIOD)) |>
  group_by(Mineral) |>
  reframe(
    q5_RECOV_RATE = wtd.quantile(RECOV_RATE, prod2025, 0.05, na.rm = T),
    q10_RECOV_RATE = wtd.quantile(RECOV_RATE, prod2025, 0.10, na.rm = T),
    q90_RECOV_RATE = wtd.quantile(RECOV_RATE, prod2025, 0.90, na.rm = T),
    q95_RECOV_RATE = wtd.quantile(RECOV_RATE, prod2025, 0.95, na.rm = T),
    q5_RECOV_RATE_BY_PERIOD = wtd.quantile(RECOV_RATE_BY_PERIOD, prod2025, 0.05, na.rm = T),
    q10_RECOV_RATE_BY_PERIOD = wtd.quantile(RECOV_RATE_BY_PERIOD, prod2025, 0.10, na.rm = T),
    q90_RECOV_RATE_BY_PERIOD = wtd.quantile(RECOV_RATE_BY_PERIOD, prod2025, 0.90, na.rm = T),
    q95_RECOV_RATE_BY_PERIOD = wtd.quantile(RECOV_RATE_BY_PERIOD, prod2025, 0.95, na.rm = T)
  )
# Using recov by period p5-95
# Cu 55-95%, Ni 26-90%, Co 45-85%
# Using recov
# Cu 80-95%, Ni 65-95%, Co 65-85%

## Mine Type classification --------

# majority is open pit and underground
table(c(df$MINE_TYPE1, df$MINE_TYPE2, df$MINE_TYPE3)) |> sort(decreasing = T)
# Lots of NA
table(paste(df$MINE_TYPE1, df$MINE_TYPE2, df$MINE_TYPE3, sep = "-")) |> sort()
table(df$MINE_TYPE1)
df |> group_by(Mineral, MINE_TYPE1) |> tally() |> pivot_wider(names_from = Mineral, values_from = n)


# Either combined, open pit, underground, or Other (tailings, ...)
df <- df |>
  # remove ocean and dredging
  mutate(aux = paste0(MINE_TYPE1, "-", MINE_TYPE2, "-", MINE_TYPE3)) |>
  filter(!str_detect(aux, "Ocean|Dredging")) |> # remove ocean
  mutate(
    mine_type = case_when(
      str_detect(aux, "Open Pit") & str_detect(aux, "Underground") ~ "Combined",
      MINE_TYPE1 %in% c("Open Pit", "Tailings", "Stock Pile") ~ "Open Pit",
      MINE_TYPE1 %in% c("Underground") ~ "Underground",
      MINE_TYPE2 %in% c("Open Pit", "Tailings", "Stock Pile") ~ "Open Pit",
      MINE_TYPE2 %in% c("Underground") ~ "Underground",
      MINE_TYPE3 %in% c("Open Pit", "Tailings", "Stock Pile") ~ "Open Pit",
      MINE_TYPE3 %in% c("Underground") ~ "Underground",
      T ~ "Other"
    )
  )
table(df$mine_type) # a lot of other
df |>
  group_by(Mineral, mine_type) |>
  reframe(res = sum(resources, na.rm = T) / 1e6) |>
  pivot_wider(names_from = Mineral, values_from = res) # less others in active


table(df$PROCESSING_METHOD1) |> sort()


## Status classification --------

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

df |>
  filter(ACTV_STATUS == "Active") |>
  group_by(Mineral, status) |>
  tally() |>
  pivot_wider(names_from = Mineral, values_from = n)

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
  filter(!is.na(resources)) |>
  filter(!is.na(grade_resource)) |>
  filter(!is.na(resources_ore)) |>
  filter(ID != 88695) # specific mine in the ocean and not showing on mine type

nrow(df) # 2053, but duplicated entries due to multiple minerals

# Table for data completeness
df |>
  group_by(Mineral) |>
  reframe(
    n_opex = sum(!is.na(OPEX_ore)),
    perc_opex = sum(!is.na(OPEX_ore)) / n() * 100,
    n_reserves = sum(!is.na(reserves)),
    perc_reserves = sum(!is.na(reserves)) / n() * 100,
    n_resources = sum(!is.na(resources)),
    perc_resources = sum(!is.na(resources)) / n() * 100,
    n_prodcap = sum(!is.na(prodCap)),
    perc_prodcap = sum(!is.na(prodCap)) / n() * 100
  )
.Last.value[, -1] |> colSums()

# OPEX model ---------

# Based on ORE processed OPEX
sum(!is.na(df$OPEX_ore)) # 394 with OPEX data

df |> filter(!is.na(OPEX_ore)) |> select(OPEX_ore) |> distinct() |> nrow() # 341 unique OPEX values

## Variable classification -------------

### Countries -------------------

# Classify countries for model
df |> filter(!is.na(OPEX_ore)) |> group_by(country) |> tally() |> arrange(desc(n)) |> print(n = 100)

# get list of regions
dict <- read_excel('Inputs/Dictionaries/Dict_Countries_SP.xlsx', sheet = "Dict")
df <- df %>%
  left_join(dict) |>
  mutate(
    country_agg = case_when(
      # unique countries relevant
      country %in%
        c(
          "China",
          "Australia",
          "South Africa",
          "Dem. Rep. Congo",
          "Chile",
          "Canada",
          "USA",
          "Peru",
          "Mexico",
          "Zambia",
          "Brazil",
          "Russia",
          "Indonesia",
          "Kazakhstan",
          "Philippines"
        ) ~ country,
      TRUE ~ Continent
    )
  )
# Countries with more than 10 deposits are their own category
table(df$country_agg)

### Main primary minerals -------------
table(df$PRIMARY_COMMODITY) |> sort(decreasing = T)
df <- df |>
  mutate(
    primary_min_agg = case_when(
      PRIMARY_COMMODITY %in%
        c("Copper", "Nickel", "Cobalt", "Gold", "Zinc", "Silver", "Platinum", "Palladium") ~ PRIMARY_COMMODITY,
      TRUE ~ "Other"
    )
  )

# exploratory plot
ggplot(df, aes(primary_min_agg, OPEX_ore)) +
  # geom_point(aes(col=country_agg)) +
  geom_boxplot(aes(fill = country_agg), alpha = 0.3, outlier.shape = NA) +
  # geom_smooth(aes(col=mine_type))+
  coord_flip() +
  labs(x = "Primary Mineral", y = "OPEX\n(USD per ton ore processed)", col = "") +
  theme_pb_wide()


## Pick predictive Model --------------

library(rpart)
set.seed(28012026)
df2 <- df |> select(OPEX_ore, country_agg, primary_min_agg) |> drop_na() |> distinct()


# 80% for train, 20% for test
idx <- sample.int(nrow(df2), size = floor(0.8 * nrow(df2)))
train <- df2[idx, ]
test <- df2[-idx, ]

# Linear Regression
lm_fit <- lm(OPEX_ore ~ country_agg + primary_min_agg, data = train)

# Regression tree
tree_fit <- rpart(
  OPEX_ore ~ country_agg + primary_min_agg,
  data = train,
  method = "anova",
  control = rpart.control(xval = 10)
)
cp_best <- tree_fit$cptable[which.min(tree_fit$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_fit, cp = cp_best)

# Random Forest
library(ranger)
rf <- ranger(
  OPEX_ore ~ country_agg + primary_min_agg,
  data = train,
  num.trees = 300,
  min.node.size = 10,
  respect.unordered.factors = "order",
  seed = 28012026
)


# Model predictions on test data
pred_lm <- predict(lm_fit, test)
pred_tree <- predict(tree_pruned, test)
pred_rf <- predict(rf, test)$predictions

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
rsq <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

# Comparison
tibble(
  model = c("lm", "tree", "random forest"),
  rmse = c(rmse(test$OPEX_ore, pred_lm), rmse(test$OPEX_ore, pred_tree), rmse(test$OPEX_ore, pred_rf)), # root mean squared error
  rsq = c(rsq(test$OPEX_ore, pred_lm), rsq(test$OPEX_ore, pred_tree), rsq(test$OPEX_ore, pred_rf)), # coeficient of determination, or R2
)
# Randomf orest performs best: lower error, higher R2 and more stability for data filling
# in this case we do not need coefficient interpretability

rm(df2, pred_rf, pred_lm, pred_tree, rf, cp_best, tree_pruned, idx, train, test, tree_fit, lm_fit)

## Data filling based on selected RF model ------

# fit full model
rf <- ranger(
  OPEX_ore ~ country_agg + primary_min_agg,
  data = df |> drop_na(OPEX_ore) |> dplyr::select(OPEX_ore, country_agg, primary_min_agg) |> distinct(),
  num.trees = 300,
  min.node.size = 10,
  respect.unordered.factors = "order",
  quantreg = T,
  seed = 28012026
)

pred_q <- predict(
  rf,
  data = df |> dplyr::select(country_agg, primary_min_agg),
  type = "quantiles",
  quantiles = c(0.1, 0.5, 0.9)
)$predictions

# Data filling strategy,
df <- df |>
  mutate(OPEX_orig = OPEX_ore) |>
  mutate(OPEX_source = case_when(!is.na(OPEX_ore) ~ "S&P", T ~ "Fitted Model")) |>
  mutate(
    OPEX_ore = if_else(is.na(OPEX_ore), predict(rf, data = pick(country_agg, primary_min_agg))$predictions, OPEX_ore),
    OPEX_ore_median = if_else(OPEX_source == "Fitted Model", pred_q[, 2], OPEX_ore),
    OPEX_ore_low = if_else(OPEX_source == "Fitted Model", pred_q[, 1], OPEX_ore * 0.9),
    OPEX_ore_high = if_else(OPEX_source == "Fitted Model", pred_q[, 3], OPEX_ore * 1.1)
  )

sum(is.na(df$OPEX_ore))
table(df$OPEX_source)

range(df$OPEX_ore) # no negatives
ggplot(df, aes(OPEX_ore)) +
  geom_histogram() +
  facet_wrap(~primary_min_agg) +
  theme_pb_wide() +
  geom_vline(xintercept = 0, col = "red")

# just to sort plot
data_fig <- df |> group_by(country_agg) |> mutate(OPEX_avg = mean(OPEX_ore)) |> ungroup()

ggplot(data_fig, aes(x = reorder(country_agg, OPEX_avg), y = OPEX_ore)) +
  geom_boxplot(aes(fill = OPEX_source), alpha = 0.8, outlier.shape = NA) +
  geom_point( data = filter(data_fig,OPEX_source == "Fitted Model"),
  position = position_nudge(x = -0.2),aes(col=primary_min_agg), alpha=0.6) +
  geom_point( data = filter(data_fig,OPEX_source != "Fitted Model"),
  position = position_nudge(x = 0.2),aes(col=primary_min_agg), alpha=0.6) +
  coord_flip(expand = F) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, 503)) +
  scale_fill_manual(values = c("S&P" = "#ccebc5", "Fitted Model" = "#fbb4ae")) +
  # scico::scale_colour_scico_d(palette = "batlow") +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(y = "OPEX\n(USD per ton ore processed)", x = "", fill = "Data Source", col = "Primary Mineral of Deposit") +
  theme_pb_wide() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.8, 0.2), axis.text.x = element_text(hjust = 1))

# fmt: skip
ggsave("Figures/Deposit/Ore_Opex.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

# CI Figure
data_fig <- df |> group_by(country_agg) |> mutate(OPEX_avg = mean(OPEX_ore)) |> ungroup()

data_long <- df |>
  mutate(id = row_number()) |>
  select(id, country_agg, OPEX_ore, OPEX_ore_low, OPEX_ore_high) |>
  pivot_longer(cols = c(OPEX_ore_low, OPEX_ore, OPEX_ore_high), names_to = "scenario", values_to = "OPEX_value") |>
  mutate(scenario = recode(scenario, "OPEX_ore_low" = "Low (P5)", "OPEX_ore" = "Mean", "OPEX_ore_high" = "High (P95)"))

ggplot(data_long, aes(x = reorder(country_agg, OPEX_value), y = OPEX_value)) +
  geom_boxplot(aes(fill = scenario), alpha = 0.8, outlier.shape = NA) +
  geom_point(data=filter(data_long,scenario=="Mean"),position = position_nudge(x = 0.2),aes(col=scenario), alpha=0.6) +
  geom_point(data=filter(data_long,scenario=="Low (P5)"),position = position_nudge(x = 0),aes(col=scenario), alpha=0.6) +
  geom_point(data=filter(data_long,scenario=="High (P95)"),position = position_nudge(x = -0.2),aes(col=scenario), alpha=0.6) +
  coord_flip(expand = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = " ", prefix = "$"), limits = c(0, 503)) +
  scale_fill_manual(values = c("Low (P5)" = "#33a02c", "Mean" = "#1f78b4", "High (P95)" = "#e31a1c")) +
  scale_color_manual(values = c("Low (P5)" = "#33a02c", "Mean" = "#1f78b4", "High (P95)" = "#e31a1c"), guide = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = "OPEX\n(USD per ton ore processed)", x = "", fill = "Scenario") +
  theme_pb_wide() +
  theme(legend.position = c(0.8, 0.2), axis.text.x = element_text(hjust = 1))

# fmt: skip
ggsave("Figures/Deposit/Ore_Opex_CI.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)


# CAPEX model ------

# Based on historical capital costs
# Database comes with 3 columns of capital costs
capex <- df |>
  # only consider first investment
  filter(CAPITAL_COST_TYPE...48 == "Initial Capital Cost") |>
  # consider further expansions towards existing capacity
  filter(CAPITAL_COST_TYPE...49 %in% c("Initial Capital Cost", "Expansion") | is.na(CAPITAL_COST_TYPE...49)) |>
  filter(CAPITAL_COST_TYPE...50 %in% c("Initial Capital Cost", "Expansion") | is.na(CAPITAL_COST_TYPE...50)) |>
  mutate(
    cost1 = as.numeric(AMT_CAPITAL_INVESTED...45),
    cost1 = if_else(is.na(cost1), 0, cost1),
    cost2 = as.numeric(AMT_CAPITAL_INVESTED...46),
    cost2 = if_else(is.na(cost2), 0, cost2),
    cost3 = as.numeric(AMT_CAPITAL_INVESTED...47),
    cost3 = if_else(is.na(cost3), 0, cost3),
    capCost = cost1 + cost2 + cost3,
    capM = capCost / 1e3, # for figure # capCost is in thousand USD)
    ore_processed_K = ore_processed / 1e3 #
  ) |>
  distinct(Name, .keep_all = T)

p <- ggplot(capex, aes(ore_processed_K, capM, col = Continent)) +
  geom_point(alpha=0.7) +
  # geom_smooth(method="lm",se=F,formula="y~x") +
  # facet_wrap(~mine_type) +
  coord_cartesian(
    expand = F,
    xlim = c(0, max(capex$ore_processed_K, na.rm = T) * 1.05),
    ylim = c(0, max(capex$capM) * 1.05)
  ) +
  labs(
    x = "Production Capacity (thousand tonnes ore processed per year)",
    y = "",
    title = "Capital Cost (million USD)",
    col = "Region"
  ) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::comma) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  geom_segment(x = 0, xend = 15000, y = 1000, yend = 1000, col = "black", linetype = "dashed", linewidth = 0.5) +
  geom_segment(x = 15000, xend = 15000, y = 0, yend = 1000, col = "black", linetype = "dashed", linewidth = 0.5) +
  theme_pb_wide() +
  theme(legend.position = c(0.1, 0.8), legend.box.background = element_rect(colour = "black"))
p

# zoom version
p_zoom <- ggplot(capex, aes(ore_processed_K, capM, col = primary_min_agg)) +
  geom_point(alpha=0.7) +
  coord_cartesian(expand = F, xlim = c(0, 15000), ylim = c(0, 1000)) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::comma) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  theme_pb_wide() +
  theme(legend.position = "none") +
  labs(x = "", y = "")

library(cowplot)
ggdraw() + draw_plot(p) + draw_plot(p_zoom, x = 0.55, y = 0.55, width = 0.38, height = 0.38)

# fmt: skip
ggsave("Figures/Deposit/Ore_CAPEX.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

# group continents, based on exploratory plot with geom_smooth they have different trends
capex <- capex |>
  mutate(
    continent_groups = if_else(
      Continent %in% c("South America", "Asia", "Europe", "North America"),
      "Group 1",
      "Group 2"
    )
  )

# Linear model: directly interpretable
mod_capex <- lm(capCost ~ ore_processed:continent_groups, data = capex)
nobs(mod_capex) # 100
summary(mod_capex) # R2 =0.27
# plot(mod_capex)

# Fill capex for all projects
# get coefficients + intervals
ci <- confint(mod_capex, level = 0.80) # ~P10–P90
coefs_CAPEX <- broom::tidy(mod_capex) |>
  mutate(
    conf_low = ci[, 1],
    conf_high = ci[, 2],
    continent_groups = str_remove(term, "ore_processed:continent_groups"),
    capex_est = estimate,
    capex_p10 = conf_low,
    capex_p90 = conf_high
  ) |>
  dplyr::select(continent_groups, capex_est, capex_p10, capex_p90)
head(coefs_CAPEX)
(base_capex_est <- coefs_CAPEX$capex_est[1] / 1e3) # million USD, intial investment
base_capex_p10 <- coefs_CAPEX$capex_p10[1] / 1e3
base_capex_p90 <- coefs_CAPEX$capex_p90[1] / 1e3

coefs_CAPEX[2, 2] * 1e3 # USD per tpa ore added, according to industry a reasonable range is $30-$70 per tpa
coefs_CAPEX[3, 2] * 1e3


table(df$status)
df <- df |>
  mutate(
    continent_groups = if_else(
      Continent %in% c("South America", "Asia", "Europe", "North America"),
      "Group 1",
      "Group 2"
    )
  ) |>
  left_join(coefs_CAPEX) |>
  mutate(
    # sunk cost considered
    CAPEX_opening = if_else(status %in% c("Production", "Development"), 0, base_capex_est), # opening cost, in million USD
    CAPEX_opening_low = if_else(status %in% c("Production", "Development"), 0, base_capex_p10),
    CAPEX_opening_high = if_else(status %in% c("Production", "Development"), 0, base_capex_p90),
    CAPEX_exp = capex_est * 1e3, # expansion cost, in USD per tpa (ton per year)
    CAPEX_exp_low = capex_p10 * 1e3,
    CAPEX_exp_high = capex_p90 * 1e3
  )

sum(is.na(df$CAPEX_opening)) # 0
sum(is.na(df$CAPEX_exp)) # 0
sum(is.na(df$CAPEX_opening_low)) # 0


# Cost Allocation Coproduction ------------------

# Based on shared revenue

sum(!is.na(df$grossRevenue)) # 461

share_rev <- df |>
  dplyr::select(Name, ID, grossRevenue, prodMetal, price_metal) |>
  drop_na() |>
  mutate(revenue = prodMetal * price_metal) |>
  group_by(ID, Name) |>
  reframe(revenue = sum(revenue), grossRevenue = mean(grossRevenue)) |>
  ungroup() |>
  mutate(share_NiCoCu = revenue / grossRevenue) |>
  arrange(desc(share_NiCoCu)) # max is 100%

ggplot(share_rev, aes(share_NiCoCu)) + geom_histogram(col = "black", fill = "grey") + theme_pb_small()
ggplot(share_rev, aes(share_NiCoCu)) + stat_ecdf() + theme_pb_small()


# Allocation for deposits with no data on gross revenues, based on Grade x Price (proxy for revenue given same ore)
# Price: 2021-2025 avg, daily price data from S&P
# fmt: skip
p_cu <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Copper(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B1313") |> mutate(Mineral="Copper")
names(p_cu)[1:2] <- c("Date", "Price")
# fmt: skip
p_ni <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Nickel(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B1313") |> mutate(Mineral="Nickel")
names(p_ni)[1:2] <- c("Date", "Price")
# fmt: skip
p_co <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Cobalt(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B1312") |> mutate(Mineral="Cobalt")
names(p_co)[1:2] <- c("Date", "Price")
# fmt: skip
p_zinc <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Zinc(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B1313") |> mutate(Mineral="Zinc")
names(p_zinc)[1:2] <- c("Date", "Price")
# fmt: skip
p_gold <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Gold(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A8:B1312") |> mutate(Mineral="Gold")
names(p_gold)[1:2] <- c("Date", "Price")
# fmt: skip
p_palladium <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Palladium(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B1312") |> mutate(Mineral="Palladium")
names(p_palladium)[1:2] <- c("Date", "Price")
# fmt: skip
p_platinum <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Platinum(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B1313") |> mutate(Mineral="Platinum")
names(p_platinum)[1:2] <- c("Date", "Price")
# fmt: skip
p_silver <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Silver(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A8:B1312") |> mutate(Mineral="Silver")
names(p_silver)[1:2] <- c("Date", "Price")
# fmt: skip
p_lithium <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Lithium(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B370") |> mutate(Mineral="Lithium")
names(p_lithium)[1:2] <- c("Date", "Price")
p_lithium$Price <- as.numeric(p_lithium$Price) * 5.323 # from USD/LCE to USD/Li

# Average of whole period (2021-2025)
(prices <- rbind(p_cu, p_ni, p_co, p_zinc, p_gold, p_palladium, p_platinum, p_silver, p_lithium) |>
  mutate(Price = as.numeric(Price)) |>
  mutate(Price = if_else(Mineral %in% c("Gold", "Palladium", "Platinum", "Silver"), Price * 35274, Price)) |> # from $/oz to $/ton
  group_by(Mineral) |>
  reframe(price_avg = mean(Price, na.rm = T)))
rm(p_cu, p_ni, p_co, p_zinc, p_gold, p_palladium, p_platinum, p_silver, p_lithium)

write.csv(prices, "Parameters/MineralPrices.csv", row.names = F)

# Play with the shapes long/wide format cleverly
share_rev2 <- df |>
  dplyr::select(
    Name,
    ID,
    grResource_Gold,
    grResource_Palladium,
    grResource_Platinum,
    grResource_Silver,
    grResource_Zinc,
    grade_resource,
    Mineral
  ) |>
  pivot_wider(names_from = Mineral, values_from = grade_resource) |>
  rename(
    Gold = grResource_Gold,
    Palladium = grResource_Palladium,
    Platinum = grResource_Platinum,
    Silver = grResource_Silver,
    Zinc = grResource_Zinc
  ) |>
  pivot_longer(c(-Name, -ID), names_to = 'Mineral', values_to = 'grade') |>
  filter(!is.na(grade))

# Add price and get total revenue, then share allocated to Ni, Cu, Co
share_rev2 <- share_rev2 |>
  left_join(prices, by = "Mineral") |>
  mutate(revenue = grade * price_avg) |>
  mutate(revenue_CuNiCo = grade * if_else(Mineral %in% c("Copper", "Nickel", "Cobalt"), price_avg, 0)) |>
  group_by(ID, Name) |>
  reframe(revenue = sum(revenue), revenue_CuNiCo = sum(revenue_CuNiCo)) |>
  ungroup() |>
  mutate(share_NiCoCu2 = revenue_CuNiCo / revenue)
range(share_rev2$share_NiCoCu2, na.rm = T) # 0-100%

ggplot(share_rev2, aes(share_NiCoCu2)) + geom_histogram(col = "black", fill = "grey") + theme_pb_small()
ggplot(share_rev2, aes(share_NiCoCu2)) + stat_ecdf() + theme_pb_small()

# merge
share_rev <- share_rev2 |>
  left_join(share_rev) |>
  mutate(share_NiCoCu = if_else(!is.na(share_NiCoCu), share_NiCoCu, share_NiCoCu2)) |>
  dplyr::select(Name, ID, share_NiCoCu)

range(share_rev$share_NiCoCu)
ggplot(share_rev, aes(share_NiCoCu)) + stat_ecdf() + theme_pb_small()

# Some zeros, limit the minimum allocation to 50%, which 25% of data is below
share_rev <- share_rev |>
  mutate(share_NiCoCu = case_when(share_NiCoCu < 0.5 ~ 0.5, share_NiCoCu > 1.0 ~ 1.0, T ~ share_NiCoCu))

# add to main database

df <- df |> left_join(share_rev, by = c("Name", "ID"))

# Baseline capacity production -------

df <- df |>
  mutate(
    cap2025 = case_when(
      is.na(ore_cap) & is.na(ore_processed) ~ 0, # no data either on capacity or reported production, so zero
      is.na(ore_cap) & !is.na(ore_processed) ~ ore_processed, # only production data available
      is.na(ore_processed) & !is.na(ore_cap) ~ ore_cap, # only capacity data available
      ore_cap > ore_processed ~ ore_cap, # capacity higher than production, use capacity
      T ~ ore_processed
    )
  )

# expected production towards 2032
df <- df |>
  # in tones ore per year
  rename(cap2032 = ORE_PROCESSED_MASS_BY_PERIOD...68) |>
  mutate(cap2032 = as.numeric(cap2032)) |>
  rename(cap2031 = ORE_PROCESSED_MASS_BY_PERIOD...69) |>
  mutate(cap2031 = as.numeric(cap2031)) |>
  rename(cap2030 = ORE_PROCESSED_MASS_BY_PERIOD...70) |>
  mutate(cap2030 = as.numeric(cap2030)) |>
  rename(cap2029 = ORE_PROCESSED_MASS_BY_PERIOD...71) |>
  mutate(cap2029 = as.numeric(cap2029)) |>
  rename(cap2028 = ORE_PROCESSED_MASS_BY_PERIOD...72) |>
  mutate(cap2028 = as.numeric(cap2028)) |>
  rename(cap2027 = ORE_PROCESSED_MASS_BY_PERIOD...73) |>
  mutate(cap2027 = as.numeric(cap2027)) |>
  rename(cap2026 = ORE_PROCESSED_MASS_BY_PERIOD...74) |>
  mutate(cap2026 = as.numeric(cap2026)) |>
  mutate(across(c(cap2026, cap2027, cap2028, cap2029, cap2030, cap2031, cap2032), ~ replace_na(.x, 0))) |>
  mutate(cap2026 = if_else(cap2026 < cap2025, cap2025, cap2026)) |>
  mutate(cap2027 = if_else(cap2027 < cap2026, cap2026, cap2027)) |>
  mutate(cap2028 = if_else(cap2028 < cap2027, cap2027, cap2028)) |>
  mutate(cap2029 = if_else(cap2029 < cap2028, cap2028, cap2029)) |>
  mutate(cap2030 = if_else(cap2030 < cap2029, cap2029, cap2030)) |>
  mutate(cap2031 = if_else(cap2031 < cap2030, cap2030, cap2031)) |>
  mutate(cap2032 = if_else(cap2032 < cap2031, cap2031, cap2032))


# Delays in openings based on status ------
table(df$status)
df <- df |>
  mutate(
    delay_years = case_when(
      status == "Exploration" ~ 8,
      status == "Economic Studies" ~ 5,
      status == "Development" ~ 2,
      status == "Production" ~ 0,
      T ~ 8
    )
  )


# AGGREGATE: 1 ROW = 1 DEPOSIT ---------------
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
    prod2025, # tons metal produced in 2025
    cap2025, # tons ore per year
    cap2026,
    cap2027,
    cap2028,
    cap2029,
    cap2030,
    cap2031,
    cap2032,
    grade_head,
    OPEX_source,
    OPEX_ore, # USD per ton ore processed
    OPEX_ore_low,
    OPEX_ore_high,
    OPEX_ore_median,
    CAPEX_opening, # million USD
    CAPEX_opening_low,
    CAPEX_opening_high,
    CAPEX_exp, # USD per tpa ore added
    CAPEX_exp_low,
    CAPEX_exp_high,
    share_NiCoCu, # 0 to 1, cost allocation based on revenues of coproducts
    delay_years,
    Mineral,
    PRIMARY_COMMODITY
  ) |>
  mutate(reserves = if_else(is.na(reserves), 0, reserves))

# spread  and fill na
df_save <- df_save |>
  mutate(across(all_of(c("grade_reserves", "grade_head", "ore_processed", "prod2025")), ~ replace_na(.x, 0))) |>
  pivot_wider(
    names_from = Mineral,
    values_from = c(reserves, grade_reserves, resources, grade_resource, grade_head, prod2025),
    names_sep = "_",
    values_fill = 0
  )


nrow(df_save) # 1630 deposits
table(df_save$PRIMARY_COMMODITY) |> sort(decreasing = T)

# Do resources, ore and grades make sense? NOT ALWAYS
df_save |>
  mutate(est_cu = resources_ore * grade_resource_Copper / 100, est_ni = resources_ore * grade_resource_Nickel / 100) |>
  mutate(abs_diff_cu = abs(est_cu - resources_Copper), abs_diff_ni = abs(est_ni - resources_Nickel)) |>
  dplyr::select(Name, ID, est_cu, resources_Copper, abs_diff_cu, est_ni, resources_Nickel, abs_diff_ni) |>
  arrange(desc(abs_diff_ni))

# Not really, approach: fix grade based on ore and resources
# These approach assumes resources are correct
df_save <- df_save |>
  mutate(
    grade_resource_Copper = if_else(resources_Copper > 0, (resources_Copper / resources_ore) * 100, 0),
    grade_resource_Nickel = if_else(resources_Nickel > 0, (resources_Nickel / resources_ore) * 100, 0),
    grade_resource_Cobalt = if_else(resources_Cobalt > 0, (resources_Cobalt / resources_ore) * 100, 0)
  )

# Minor corrections
# df_save |> dplyr::select(grade_resource_Copper,grade_resource_Nickel,grade_resource_Cobalt) |> skimr::skim()

# Add assumptions on recovery rate and max depletion rate
df_save <- df_save |>
  mutate(recovery_rate_Copper = 0.8, recovery_rate_Nickel = 0.7, recovery_rate_Cobalt = 0.7) |>
  mutate(max_depletion_rate = 0.04) |>
  # based on observed spread of currently producing deposits
  mutate(recovery_rate_Copper_low = 0.6, recovery_rate_Copper_high = 0.95) |>
  mutate(recovery_rate_Nickel_low = 0.5, recovery_rate_Nickel_high = 0.9) |>
  mutate(recovery_rate_Cobalt_low = 0.55, recovery_rate_Cobalt_high = 0.85) |>
  # scenarios
  mutate(max_depletion_rate_low = 0.02, max_depletion_rate_high = 0.05)


# SAVE ---------
df_save <- df_save |> arrange(ID)
write.csv(df_save, "Parameters/Intermediate/CuNiCo_Deposit_SP.csv", row.names = F)

# EoF
