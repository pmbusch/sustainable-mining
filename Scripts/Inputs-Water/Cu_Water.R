# Copper Water Consumption

# Equation for water consumption impact per ton

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
  theme(
    legend.position = c(0.7, 0.7),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

# fmt: skip
ggsave("Figures/Deposit/cu-water-use-intensity.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 1.5,height = 8.7 * 1.5)

ggplot(df, aes(Mine_type, TotalWater_m3_tonCu)) + geom_boxplot()

mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade) - 1)
mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade):Mine_type2 - 1)

nobs(mod)
summary(mod)
coefficients(mod)


# S&P Copper Data - already filtered for active mines
deposit <- read.csv("Parameters/Intermediate/Cu_Deposit_SP.csv")
nrow(deposit)

# Water consumption allocation hierarchy
# 1. Based on deposit name
dep_water <- df |>
  group_by(Deposit) |>
  reframe(water_dep = mean(TotalWater_m3_tonCu, na.rm = T)) |>
  rename(Name = Deposit)

# 2. Based on ore grade
mod <- lm(data = df, TotalWater_m3_tonCu ~ I(1 / ore_grade):Mine_type2 - 1)
# assumed combined is underground and otehr open pit
table(deposit$mine_type)
fill_coef <- tibble(mine_type = c("Open Pit", "Underground", "Other", "Combined"), coef = rep(coefficients(mod), 2))


names(deposit)
deposit |> filter(!is.na(grade_resource)) |> nrow() # 885, almost all
range(deposit$grade_resource, na.rm = T)
deposit <- deposit %>%
  left_join(dep_water) |>
  left_join(fill_coef) |>
  # in m3 per ton Cu
  mutate(grade_resource = if_else(is.na(grade_resource), grade_head, grade_resource)) |>
  mutate(
    water = if_else(!is.na(water_dep), water_dep, coef / (grade_resource / 100)),
    water_fill = if_else(!is.na(water_dep), "Deposit", "Ore Grade Model")
  )
table(deposit$water_fill) # 43 by name
sum(is.na(deposit$water)) # 8 missing
deposit <- deposit |> filter(!is.na(water))

# add water risk baseline - AWARE factors
aware <- read.csv("Parameters/Intermediate/Cu_Deposit_aware.csv")
# factor from 0.1 to 100
# annual demand and available in m3 per year
aware <- aware |> dplyr::select(Basin_ID, Name, ID, aware_cf, aware_demand, aware_available)


wb <- deposit %>% left_join(aware) |> mutate(water_footprint = water * aware_cf)
sum(is.na(wb$water_footprint)) # 0

# Save water data
names(wb)
wb$water_dep <- wb$coef <- NULL
write.csv(wb, "Parameters/Cu_Deposit.csv", row.names = F)


sum(wb$resources) / 1e6
sum(wb$reserves) / 1e6
sum(wb$cap2025) / 1e6

# EoF
