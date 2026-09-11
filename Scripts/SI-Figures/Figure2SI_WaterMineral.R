# ============================================================
# Stacked Area Graph - Water Consumption by Mineral
# Shows percentage of total water consumption by mineral
# across water impact levels
# ============================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# ============================================================
# 00. COMMON INPUTS ------------------------------------------
# ============================================================

label_text <- 7

# Load preprocessed data
# Compiled in "Scripts/Figure X - Precompute.R"
water_consumption_data <- read.csv("Results/Processed/mineral_decomp_demand.csv")

# ============================================================
# 01. DATA PREPARATION ---------------------------------------
# ============================================================

# Filter for NZE scenario and reshape water consumption to long format
water_long <- water_consumption_data %>%
  filter(Scenario == "NZE") %>%
  filter(!str_detect(metric, "water")) %>%
  select(water, copper_water, nickel_water, cobalt_water, lithium_water) %>%
  rename(Copper = copper_water, Nickel = nickel_water, Cobalt = cobalt_water, Lithium = lithium_water) %>%
  pivot_longer(cols = c(Copper, Nickel, Cobalt, Lithium), names_to = "Mineral", values_to = "Water_Value") %>%
  mutate(Pct = (Water_Value / water) * 100) %>%
  ungroup()


# ============================================================
# 02. CREATE STACKED AREA PLOT --------------------------------
# ============================================================

ggplot(water_long, aes(x = water, y = Pct, fill = Mineral, group = Mineral)) +
  geom_area(position = "stack",col="black",linewidth=0.2) +
  scale_fill_manual(values = minerals_colors) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x = "Total Water Consumption",
    y = "Percentage of Total Water Consumption",
    fill = "Mineral",
    title = "Water Consumption by Mineral (NZE Scenario)"
  ) +
  theme_pb_large() +
  theme(axis.text = element_text(size = label_text), axis.title = element_text(size = label_text + 1))

# ============================================================
# 03. EXPORT PLOT --------------------------------------------
# ============================================================

ggsave("Figures/SI/Figure2_waterMineral.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
