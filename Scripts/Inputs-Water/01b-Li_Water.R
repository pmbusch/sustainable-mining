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

# Decision: use full model but combine clay and hard rock
df <- df |> mutate(model_class = if_else(mine_type %in% c("Clay", "Hard Rock"), "Hard Rock & Clay", mine_type))

mod <- lm(FreshWater_m3_tonLi ~ I(1 / ore_grade):model_class - 1, data = df)

water_use_intensity_fits <- tibble(term = names(coef(mod)), coef = unname(coef(mod))) |>
  mutate(model_class = sub("^.*model_class", "", term)) |>
  left_join(count(model.frame(mod), model_class), by = "model_class")

eq_li_water <- paste0(
  paste(
    sprintf("%s: y = %.1f/x", water_use_intensity_fits$model_class, water_use_intensity_fits$coef),
    collapse = "\n"
  ),
  "\nn = ",
  paste(water_use_intensity_fits$n, collapse = ", "),
  "\nR² = ",
  sprintf("%.2f", summary(mod)$r.squared)
)

li_mine_type_colors <- c(
  "Brine" = "#2C7FB8", # water / brine
  "Brine - DLE" = "#41AB9C", # tech-driven brine extraction
  "Hard Rock & Clay" = "#7F7F7F" # rock
)

ggplot(df, aes(ore_grade, FreshWater_m3_tonLi, col = model_class)) +
  geom_point(size=3) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ I(1 / x) - 1, alpha = 0.7, linewidth = 1) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = eq_li_water,
    hjust = 1.02,
    vjust = 1.1,
    size = 10 * 5 / 14 * 0.8,
    lineheight = 0.9,
    colour = "black"
  ) +
  scale_x_continuous(labels = scales::percent) +
  ylim(0, 4500) +
  scale_color_manual(values = li_mine_type_colors) +
  coord_cartesian(expand = F, clip = "off") +
  labs(
    x = "Grade Ore (% Li)",
    y = expression("Freshwater consumption [" ~ m^3 * ~" per ton of Lithium]"),
    title = NULL,
    col = "Resource type"
  ) +
  theme_pb_large() +
  theme(
    legend.position = c(0.7, 0.7),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

# fmt: skip
ggsave("Figures/Deposit/Lithium/li-water-use-intensity.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 1.5,height = 8.7 * 1.5)


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
write.csv(df, "Parameters/Li_Water_consumption_intensity.csv", row.names = F)

# EoF
