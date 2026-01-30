# Equation for water consumption impact per ton

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Lithium ------

df <- read_excel("Inputs/Water_Data.xlsx", sheet = "Database")
names(df)

df <- df %>%
  filter(Include == 1) %>%
  mutate(ore_grade = as.numeric(`Grade ore Li%`)) %>%
  mutate(
    key = case_when(
      str_detect(Type, "Brine") & DLE == "Yes" ~ "Brine - DLE",
      str_detect(Type, "Brine") ~ "Brine",
      T ~ Type
    )
  )

table(df$Type)
table(df$key)

ggplot(df, aes(FreshWater_m3_tonLCE, fill = key)) +
  geom_histogram() +
  # geom_density()+
  facet_wrap(~key, scales = "free")


# according to ecoivent
# brine 7.8 m3 per kg LCE
# spod: 42.7 m3 per kg LCE

ggplot(df, aes(ore_grade, FreshWater_m3_tonLCE, col = key)) +
  geom_point(size=3) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ I(1 / x) - 1, alpha = 0.7, linewidth = 1) +
  # geom_line(
  #   stat = "smooth",
  #   se = F,
  #   method = "lm",
  #   formula = y ~ I(1 / x) - 1,
  #   col = "black",
  #   alpha = 0.6,
  #   linewidth = 1
  # ) +
  scale_x_continuous(labels = scales::percent) +
  ylim(0, 800) +
  coord_cartesian(expand = F) +
  labs(
    x = "Grade Ore (% Li)",
    y = "",
    title = expression("Freshwater consumption [" ~ m^3 * ~" per ton of LCE]"),
    col = "Resource type"
  ) +
  theme(
    legend.position = c(0.7, 0.7),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

ggsave(
  "Figures/water-use-intensity.png",
  ggplot2::last_plot(),
  units = 'cm',
  dpi = 600,
  width = 8.7 * 1.5,
  height = 8.7 * 1.5
)

ggplot(df, aes(key, FreshWater_m3_tonLCE)) + geom_boxplot()

mod <- lm(data = df, FreshWater_m3_tonLCE ~ I(1 / ore_grade):key - 1)
summary(mod)
coefficients(mod)

summary(lm(data = df, FreshWater_m3_tonLCE ~ I(1 / ore_grade) - 1))

summary(lm(data = filter(df, key == "Brine"), FreshWater_m3_tonLCE ~ I(1 / ore_grade) - 1))
summary(lm(data = filter(df, key == "Brine - DLE"), FreshWater_m3_tonLCE ~ I(1 / ore_grade) - 1))
summary(lm(data = filter(df, key == "Hard Rock"), FreshWater_m3_tonLCE ~ I(1 / ore_grade) - 1))
summary(lm(data = filter(df, key == "Volcano-Sedimentary"), FreshWater_m3_tonLCE ~ I(1 / ore_grade) - 1))


lm_summary_corrected <- function(data) {
  model <- lm(FreshWater_m3_tonLCE ~ I(1 / ore_grade) - 1, data = data)
  y <- data$FreshWater_m3_tonLCE
  y_pred <- fitted(model)
  R2_corrected <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)

  cat("Standard lm() summary:\n")
  print(summary(model))

  cat(sprintf("R²_corrected = %.4f\n", R2_corrected))

  invisible(list(model = model, R2_corrected = R2_corrected))
}

df2 <- df |> filter(!is.na(FreshWater_m3_tonLCE))

lm_summary_corrected(df2)
lm_summary_corrected(filter(df2, key == "Brine"))
lm_summary_corrected(filter(df2, key == "Brine - DLE"))
lm_summary_corrected(filter(df2, key == "Hard Rock"))
lm_summary_corrected(filter(df2, key == "Volcano-Sedimentary"))


# use full one to estimate based on grade ore
deposit <- read.csv("Inputs/Deposit.csv")

nrow(deposit)
deposit %>% filter(is.na(Grade_percLi_Reserve), is.na(grade_resource_inferred), is.na(grade_resource)) %>% nrow() # 52

# add S&P data on ore grade for missing
join <- read_excel("Inputs/Dict_LiDeposit_SP.xlsx") %>% mutate(PROP_ID = as.numeric(PROP_ID)) %>% filter(PROP_ID > 0)
sp <- read_excel("Inputs/SP_Lithium_22Oct2025.xls", skip = 4) %>% slice(-1, -2)

ore <- deposit %>%
  filter(is.na(Grade_percLi_Reserve), is.na(grade_resource_inferred), is.na(grade_resource)) %>%
  left_join(join, by = c("Deposit_Name" = "Li_Busch2025")) %>%
  left_join(dplyr::select(
    sp,
    PROP_ID,
    GRD_RESV_PCT_TONNE,
    GRD_MEAS_IND_PCT_TONNE,
    GRD_INF_PCT_TONNE,
    GRD_R_AND_R_PCT_TONNE
  )) %>%
  mutate(
    Grade_percLi_Reserve = as.numeric(GRD_RESV_PCT_TONNE),
    grade_resource = as.numeric(GRD_MEAS_IND_PCT_TONNE),
    grade_resource_inferred = as.numeric(GRD_INF_PCT_TONNE)
  ) %>%
  dplyr::select(-c(PROP_ID, GRD_RESV_PCT_TONNE, GRD_MEAS_IND_PCT_TONNE, GRD_R_AND_R_PCT_TONNE, GRD_INF_PCT_TONNE))

# add back to deposit
deposit <- deposit %>%
  filter(!is.na(Grade_percLi_Reserve) | !is.na(grade_resource_inferred) | !is.na(grade_resource)) %>%
  rbind(ore)

deposit %>% filter(is.na(Grade_percLi_Reserve), is.na(grade_resource_inferred), is.na(grade_resource)) %>% nrow() # 37 now

# DLE
# fmt: skip
dle_dep <- c("Salar de Atacama (SQM)","Salton Sea (All)","Upper Rhine Valley",
    "Salar de Olaroz (Allkem)","Salar del Hombre Muerto (Livent)",
    "Qaidam Basin (All Projects)","Kachi","Centenario Ratones","Salar del Rincon (Rio Tinto)",
    "Salar de Atacama (Albemarle)","Boardwalk","Diablillios/Sal de Los Angeles","Smackover LANXESS",
    "Great Salt Lake","Clearwater","Laguna Verde","Paradox","Francisco Basin")
deposit <- deposit |> mutate(Resource_Type = if_else(Deposit_Name %in% dle_dep, "Brine-DLE", Resource_Type))
table(deposit$Resource_Type)

# worst case by type (or 90th percentile)
table(deposit$Resource_Type)
df2 %>% group_by(key) %>% reframe(m3_tonLCE = max(FreshWater_m3_tonLCE), p90 = quantile(FreshWater_m3_tonLCE, 0.9))
# fill for NA in ore grade
fill_na <- tibble(
  Resource_Type = c("Brine", "Brine-DLE", "Hard Rock", "Volcano-Sedimentary"),
  water_fill = c(366, 357, 161, 913)
)


# Water consumption allocation hierarchy
# 1. Based on deposit name
dep_water <- df |>
  filter(Include == 1) |>
  group_by(Name, key) |>
  reframe(water_dep = mean(FreshWater_m3_tonLCE, na.rm = T)) |>
  rename(Deposit_Name = Name)
# 2. Based on ore grade
mod <- lm(data = df, FreshWater_m3_tonLCE ~ I(1 / ore_grade):key - 1)
fill_coef <- tibble(
  Resource_Type = c("Brine", "Brine-DLE", "Hard Rock", "Volcano-Sedimentary"),
  coef = coefficients(mod)
)

# 3. Based on resource type worst case
fill_na

names(deposit)
# 5.323 from LCE to Li
deposit <- deposit %>%
  left_join(dep_water) |>
  left_join(fill_coef) |>
  left_join(fill_na) |>
  # in m3 per kton Li
  mutate(
    water1 = case_when(
      !is.na(water_dep) ~ water_dep,
      is.na(Grade_percLi_Reserve) | near(Grade_percLi_Reserve, 0) ~ water_fill,
      T ~ coef / (Grade_percLi_Reserve / 100)
    ) *
      5.323 *
      1e3,
    water2 = case_when(
      !is.na(water_dep) ~ water_dep,
      is.na(grade_resource) | near(grade_resource, 0) ~ water_fill,
      T ~ coef / (grade_resource / 100)
    ) *
      5.323 *
      1e3,
    water3 = case_when(
      !is.na(water_dep) ~ water_dep,
      is.na(grade_resource_inferred) | near(grade_resource_inferred, 0) ~ water_fill,
      T ~ coef / (grade_resource_inferred / 100)
    ) *
      5.323 *
      1e3
  )


# add water risk baseline
water_base <- read.csv("Parameters/Water_Baseline.csv")
water_base$d <- NULL

wb <- deposit %>% left_join(water_base)

# scatter plot
data_fig <- wb %>%
  filter(ratio_demand_avai < 10) %>%
  # avg cost and water by stage size
  mutate(
    cost = (cost1 * reserve + cost2 * resource_demostrated + cost3 * resource_inferred) /
      (reserve + resource_demostrated + resource_inferred),
    water = (water1 * reserve + water2 * resource_demostrated + water3 * resource_inferred) /
      (reserve + resource_demostrated + resource_inferred),
    res = (reserve + resource_demostrated + resource_inferred) / 1e3
  ) %>%
  mutate(cost = cost / 5.323) %>% # $ per ton LCE
  mutate(water = water / 1e3 / 5.323) %>% # m3 per ton LCE
  mutate(surplus = surplus / 1e6)

# range(data_fig$surplus)

range(data_fig$ratio_demand_avai)
legend_breaks <- c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10)
leg_log <- log2(legend_breaks)

ggplot(data_fig, aes(ratio_demand_avai)) + geom_histogram()

ggplot(data_fig, aes(water)) + geom_histogram()


ggplot(data_fig, aes(water, cost, col = Resource_Type)) +
  geom_point(data=filter(data_fig,count_water==1),aes(size=res),alpha=1,col="black",stroke=1) +
  geom_point(aes(size=res),alpha=0.7) +
  # scale_color_gradient2(low = "darkblue",midpoint=0.1,mid="grey", high = "darkred",
  #                       trans="log10",breaks=legend_breaks,labels=legend_breaks) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(7, 14.5) * 1e3) +
  annotate(
    geom = "text",
    x = 770,
    y = 7500,
    label = "Located in a basin with current\nwater demand > available water"
  ) +
  xlim(0, 2200) +
  # scale_x_log10()+
  coord_cartesian(expand = F) +
  scale_color_manual(
    values = c(
      "Brine" = "#0000FF33",
      "Brine-DLE" = "#9B870D",
      "Hard Rock" = "#80008080",
      "Volcano-Sedimentary" = "#FF000080"
    )
  ) +
  labs(
    x = expression("Unitary Water Consumption [" ~ m^3 * ~"per ton LCE]"),
    y = "Extraction Costs [$USD per ton LCE]",
    fill = "",
    col = "Resource Type",
    size = "Total Resource [M tons Li]",
    title = "160 Lithium deposits"
  ) +
  theme_bw(8) +
  theme(panel.grid = element_blank())

ggsave("Figures/water_impact.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# determine water usage based on stress
wb <- wb |>
  mutate(
    ratio_interval = case_when(
      ratio_demand_avai < 0.1 ~ "<0.1",
      ratio_demand_avai < 0.5 ~ "0.1-0.5",
      ratio_demand_avai < 1 ~ "0.5-1",
      ratio_demand_avai < 2 ~ "1-2",
      ratio_demand_avai < 5 ~ "2-5",
      T ~ ">5"
    )
  )
table(wb$ratio_interval)

wb <- wb |>
  mutate(
    waterRisk_coef = case_when(
      ratio_demand_avai < 0.1 ~ 0,
      ratio_demand_avai < 0.5 ~ 0.5,
      ratio_demand_avai < 1 ~ 0.75,
      ratio_demand_avai < 2 ~ 1,
      ratio_demand_avai < 5 ~ 1,
      T ~ 1
    )
  )
table(wb$waterRisk_coef)

# Save water data
names(wb)
write.csv(wb, "Parameters/Deposit_water.csv", row.names = F)

# EoF
