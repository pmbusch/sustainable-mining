# Equation for water consumption impact per ton
# Based on collected data from various academic sources

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Lithium ------

df <- read_excel("Inputs/Li_Water.xlsx", sheet = "Database")
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

df <- df |>
  mutate(mine_type = if_else(str_detect(key, "Volcano"), "Clay", key)) |>
  mutate(FreshWater_m3_tonLi = FreshWater_m3_tonLCE * 5.323)

ggplot(df, aes(FreshWater_m3_tonLi, fill = mine_type)) +
  geom_histogram() +
  # geom_density()+
  facet_wrap(~key, scales = "free") +
  theme_pb_wide()


# according to ecoivent
# brine 7.8 m3 per kg LCE
# spod: 42.7 m3 per kg LCE

ggplot(df, aes(ore_grade, FreshWater_m3_tonLi, col = mine_type)) +
  geom_point(size=3) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ I(1 / x) - 1, alpha = 0.7, linewidth = 1) +
  scale_x_continuous(labels = scales::percent) +
  ylim(0, 4500) +
  coord_cartesian(expand = F) +
  labs(
    x = "Grade Ore (% Li)",
    y = "",
    title = expression("Freshwater consumption [" ~ m^3 * ~" per ton of Lithium]"),
    col = "Resource type"
  ) +
  theme_pb_wide() +
  theme(
    legend.position = c(0.7, 0.7),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

# fmt: skip
ggsave("Figures/Deposit/Lithium/li-water-use-intensity.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 1.5,height = 8.7 * 1.5)

ggplot(df, aes(mine_type, FreshWater_m3_tonLi)) + geom_boxplot() + theme_pb_wide()

mod <- lm(data = df, FreshWater_m3_tonLi ~ I(1 / ore_grade):mine_type - 1)
summary(mod) # R2 0.78
coefficients(mod)

summary(lm(data = df, FreshWater_m3_tonLi ~ I(1 / ore_grade) - 1))

table(df$mine_type)
summary(lm(data = filter(df, mine_type == "Brine"), FreshWater_m3_tonLi ~ I(1 / ore_grade) - 1))
summary(lm(data = filter(df, mine_type == "Brine - DLE"), FreshWater_m3_tonLi ~ I(1 / ore_grade) - 1))
summary(lm(data = filter(df, mine_type == "Hard Rock"), FreshWater_m3_tonLi ~ I(1 / ore_grade) - 1))
summary(lm(data = filter(df, mine_type == "Clay"), FreshWater_m3_tonLi ~ I(1 / ore_grade) - 1))

# Correct R2 for models with no intercept
lm_summary_corrected <- function(data) {
  model <- lm(FreshWater_m3_tonLi ~ I(1 / ore_grade) - 1, data = data)
  y <- data$FreshWater_m3_tonLi
  y_pred <- fitted(model)
  R2_corrected <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)

  cat("Standard lm() summary:\n")
  print(summary(model))

  cat(sprintf("R²_corrected = %.4f\n", R2_corrected))

  invisible(list(model = model, R2_corrected = R2_corrected))
}

df2 <- df |> filter(!is.na(FreshWater_m3_tonLi))

lm_summary_corrected(df2) # R2 0.61
lm_summary_corrected(filter(df2, mine_type == "Brine")) # R2 0.89
lm_summary_corrected(filter(df2, mine_type == "Brine - DLE")) # R2 -0.41
lm_summary_corrected(filter(df2, mine_type == "Hard Rock")) # R2 -0.06
lm_summary_corrected(filter(df2, mine_type == "Clay")) # R2 -0.55
# negative means worse than just mean

# Decision: use full model but combine clay and hard rock
df <- df |> mutate(model_class = if_else(mine_type == "Clay", "Hard Rock", mine_type))

mod <- lm(data = df, FreshWater_m3_tonLi ~ I(1 / ore_grade):model_class - 1)
summary(mod) # R2 0.78
fill_coef <- tibble(mine_type = c("Brine", "Brine-DLE", "Hard Rock"), coef = coefficients(mod))
fill_coef
# the fitted coefficients for (1/grade Li) actually represents water consumption per ton of ore processed
# 0.604 m3 water per m3 of brine processed using evaporation method
# 0.294 m3 water per m3 of brine processed using DLE method
# 1.99 m3 water per ton of hard rock ore processed
# NOTE: If we consider brine as water depletion then add 1m3 per m3 of brine processed

# 95% CI
confint(mod, level = 0.95)
# Brine 0.52-0.69 m3 per m3 of brine processed using evaporation method
# Brine-DLE 0.18-0.40 m3 per m3 of brine processed using DLE method
# Hard Rock 0.66-3.3 m3 per ton of hard rock ore processed

# Save results
df <- tibble(
  mine_type = c("Brine", "Brine-DLE", "Hard Rock"),
  water_consumption_m3_per_ton_ore = coefficients(mod),
  ci_lower_m3_per_ton_ore = confint(mod, level = 0.95)[, 1],
  ci_upper_m3_per_ton_ore = confint(mod, level = 0.95)[, 2]
)
write.csv(df, "Parameters/Li_water_consumption_intensity.csv", row.names = F)

# EoF
