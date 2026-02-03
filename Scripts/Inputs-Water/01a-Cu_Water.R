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

# Scatter
ggplot(df, aes(ore_grade, TotalWater_m3_tonCu, col = Mine_type2)) +
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
    x = "Grade Ore (% Cu)",
    y = "",
    title = expression("Freshwater consumption [" ~ m^3 * ~" per ton of Cu]"),
    col = "Resource type"
  ) +
  theme_pb_wide() +
  theme(
    legend.position = c(0.7, 0.7),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

# fmt: skip
ggsave("Figures/Deposit/Copper/cu-water-use-intensity.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 1.5,height = 8.7 * 1.5)

ggplot(df, aes(Mine_type, TotalWater_m3_tonCu)) + geom_boxplot()

mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade) - 1)
# mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade):Mine_type2 - 1) # same R@

nobs(mod) # 64
summary(mod) #
coefficients(mod) # R2 = 0.41

# the fitted coefficient for (1/grade Cu) actually represents water consumption per ton of ore processed
# 0.8155 m3 per ton of ore processed

# EoF
