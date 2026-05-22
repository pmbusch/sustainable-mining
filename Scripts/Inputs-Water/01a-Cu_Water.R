# Copper Water Consumption
# Equation for water consumption impact per ton Cu
# Based on collected data from various academic sources

source("Scripts/00-Libraries.R", encoding = "UTF-8")

df <- read_excel("Inputs/Cu_Water.xlsx", sheet = "Data")
names(df)

df <- df %>% mutate(ore_grade = as.numeric(`Grade ore Cu%`)) %>% filter(!is.na(ore_grade))

table(df$Mine_type)
table(df$Resource_type)

# Merge underground with combined, as they look similar in the plot
df <- df |> mutate(Mine_type2 = ifelse(Mine_type == "Open Pit", Mine_type, "Underground"))
table(df$Mineral)
df <- df |> filter(Mineral != "Cobalt") # only one obs

# Scatter
ggplot(df, aes(ore_grade, TotalWater_m3_tonCu)) +
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
    x = "Grade Ore (%)",
    y = "",
    title = expression("Freshwater consumption [" ~ m^3 * ~" per ton of metal (Cu, Ni or Co)]"),
    col = "Resource type"
  ) +
  theme_pb_wide() +
  theme(
    legend.position = c(0.7, 0.7),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

# fmt: skip
ggsave("Figures/Deposit/CuNiCo-water-use-intensity.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 1.5,height = 8.7 * 1.5)

ggplot(df, aes(Mine_type, TotalWater_m3_tonCu)) + geom_boxplot()

mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade) - 1)
# mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade):Mine_type2 - 1) # same R2

nobs(mod) # 98
summary(mod) # R2 = 0.3749
coefficients(mod)
confint(mod, level = 0.95)
quantile(df$m3_perTonOre, probs = c(0.0025, 0.5, 0.975)) # from data sampling, much higher

# the fitted coefficient for (1/grade Cu) actually represents water consumption per ton of ore processed
# Model contains for Nickel and Copper
# 0.8074 m3 per ton of ore processed
# 95% CI: 0.59 - 1.01 m3 per ton of ore processed

# save results
tibble(
  m3_perTonOre = coefficients(mod)[1],
  m3_perTonOre_low = confint(mod, level = 0.95)[1, 1],
  m3_perTonOre_high = confint(mod, level = 0.95)[1, 2]
) |>
  write.csv("Parameters/Cu_Water_consumption_intensity.csv", row.names = F)

# EoF
