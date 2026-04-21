# Estimate share of battery minerals as fraction of total GDP
# Based on 2025 prod data

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


## Mineral prices (for revenue) ------

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
# fmt: skip
p_lithium <- read_excel("Inputs/SP/Prices/SPGlobal_PriceChart-Lithium(Chart)_28-Jan-2026.xlsx", sheet = "Data", range = "A9:B370") |> mutate(Mineral="Lithium")
names(p_lithium)[1:2] <- c("Date", "Price")
p_lithium$Price <- as.numeric(p_lithium$Price) * 5.323 # from USD/LCE to USD/Li

# Average of whole period (2025)
(prices <- rbind(p_cu, p_ni, p_co, p_lithium) |>
  filter(Date >= as.Date("2025-01-01") & Date <= as.Date("2025-12-31")) |> # 2025 only
  mutate(Price = as.numeric(Price)) |>
  group_by(Mineral) |>
  reframe(price_avg = mean(Price, na.rm = T)))

write.csv(prices, "Parameters/MineralPrices2025.csv", row.names = F)


prices <- prices |> pivot_wider(names_from = Mineral, values_from = price_avg)

## GDP Share of revenue by country -------------

depositAll <- read.csv("Parameters/Deposit.csv")
names(depositAll)
# ore_processed = tons ore processed in 2025
prod2025 <- depositAll |>
  mutate(
    revenue = 0 +
      prod2025_Copper * prices$Copper +
      prod2025_Nickel * prices$Nickel +
      prod2025_Cobalt * prices$Cobalt +
      prod2025_Lithium * prices$Lithium
  ) |>
  group_by(country) |>
  reframe(
    revenue_2025 = sum(revenue),
    prod2025_Copper = sum(prod2025_Copper),
    prod2025_Nickel = sum(prod2025_Nickel),
    prod2025_Cobalt = sum(prod2025_Cobalt),
    prod2025_Lithium = sum(prod2025_Lithium)
  ) |>
  ungroup()
head(prod2025)

# Check to USGS numbers
sum(prod2025$prod2025_Copper) / 1e6 # 20.17 Mt, USGS 23 Mt
sum(prod2025$prod2025_Nickel) / 1e6 # 1.1 Mt, USGS 3.9 Mt (difference as only considering class 1 nickel)
sum(prod2025$prod2025_Cobalt) / 1e6 # 0.22 Mt, USGS 0.31 Mt
sum(prod2025$prod2025_Lithium) / 1e6 # 0.24 Mt, USGS 0.29 Mt

## GDP data by country
gdp_raw <- readxl::read_xls("Inputs/Worldbank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_3.xls", sheet = "Data", skip = 3)

# in USD
gdp <- gdp_raw |>
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "GDP") |>
  mutate(Year = as.numeric(Year)) |>
  group_by(`Country Name`) |>
  filter(!is.na(GDP)) |>
  filter(Year == max(Year[Year <= 2025], na.rm = TRUE)) |>
  ungroup() |>
  select(country = `Country Name`, GDP)

setdiff(prod2025$country, gdp$country) # check if all countries in prod2025 are in gdp
# fix
gdp <- gdp %>%
  mutate(
    country = recode(
      country,
      "Bosnia and Herzegovina" = "Bosnia & Herzegovina",
      "Cote d'Ivoire" = "Côte d'Ivoire",
      "Congo, Dem. Rep." = "Dem. Rep. Congo",
      "Iran, Islamic Rep." = "Iran",
      "Kyrgyz Republic" = "Kyrgyzstan",
      "Lao PDR" = "Laos",
      "Russian Federation" = "Russia",
      "Turkiye" = "Türkiye",
      "United States" = "USA",
      "Venezuela, RB" = "Venezuela",
      "Viet Nam" = "Vietnam"
    )
  )
setdiff(prod2025$country, gdp$country)

# Share
share_gdp <- prod2025 |> left_join(gdp) |> mutate(gdp_share = revenue_2025 / GDP)
share_gdp <- share_gdp |> dplyr::select(country, gdp_share, GDP)

share_gdp |> arrange(desc(gdp_share)) |> head(10)

write.csv(share_gdp, "Parameters/GDP_Share_BatteryMinerals.csv", row.names = FALSE)
