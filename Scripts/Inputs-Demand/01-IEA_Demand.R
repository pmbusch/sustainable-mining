# Battery minerals demand from IEA 2025 Critical Minerals Explorer
# https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset
# Data is for 2024 to 2050 in 5-year intervals - interpolation is needed

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# ---------------------
# LOAD IEA Demand Data -------------
# all in ktons, by scenario
# ---------------------

# Scenarios definition
# SPS: Stated Policies Scenario
# APS: Announced Pledges Scenario
# NZE: Net Zero Emissions by 2050 Scenario

# fmt: skip
names_header <- c("Sector","SPS_2024","a1","SPS_2030","SPS_2035","SPS_2040","SPS_2045","SPS_2050",
                  "a2","APS_2030","APS_2035","APS_2040","APS_2045","APS_2050",
                  "a3","NZE_2030","NZE_2035","NZE_2040","NZE_2045","NZE_2050")

# fmt: skip
cu <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A8:T17",col_names = F)
names(cu) <- names_header
cu$Mineral <- "Copper"

# fmt: skip
ni <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A42:T50",col_names = F)
names(ni) <- names_header
ni$Mineral <- "Nickel"

# Assume 30% of other uses demand is class 1 nickel (match S&P supply data)
# https://www.systemiq.earth/wp-content/uploads/2024/12/2024-12-10-EU-CRM-Innovation-Roadmap-vFinal-1.0.pdf
ni[8, 1]
ni[8, 2:20] <- ni[8, 2:20] * 0.3

# fmt: skip
co <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A22:T28",col_names = F)
names(co) <- names_header
co$Mineral <- "Cobalt"

# fmt: skip
li <- read_excel("Inputs/IEA/CM_Data_Explorer.xlsx", sheet = "1 Total demand for key minerals", range = "A33:T37",col_names = F)
names(li) <- names_header
li$Mineral <- "Lithium"

df <- rbind(cu, ni, co, li)

# Reshape ------------------
df$a1 <- df$a2 <- df$a3 <- NULL

# long format
df <- df |>
  mutate(APS_2024 = SPS_2024, NZE_2024 = SPS_2024) |> # add 2024 for all scenarios
  pivot_longer(c(-Sector, -Mineral), names_to = 'key', values_to = 'ktons') |>
  tidyr::separate(key, into = c("Scenario", "Year"), sep = "_", remove = FALSE) |>
  mutate(Year = as.numeric(Year), key = NULL)

df_orig <- df


# ---------------------
# INTERPOLATION (LINEAR) between years -------------
# ---------------------

library(zoo)
df <- df |>
  group_by(Sector, Mineral, Scenario) %>%
  complete(Year = 2024:2050) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(ktons = zoo::na.approx(ktons, x = Year, na.rm = FALSE)) %>%
  ungroup()

# Filter 2025 to 2050 and total demand
df_all <- df |> filter(Year >= 2025)
unique(df$Sector)
df_ev <- df |>
  filter(Sector == "Electric vehicles" & Year >= 2025) |>
  rename(ktons_ev = ktons) |>
  dplyr::select(-Sector)
df <- df |>
  filter(Year >= 2025, !(Sector %in% c("Total demand", "Total clean technologies"))) |> # remove totals
  group_by(Scenario, Mineral, Year) |>
  summarise(ktons = sum(ktons), .groups = "drop") |>
  left_join(df_ev) # add EV demand as separate column for later use in chemistry share estimation


nrow(df) # 312 = 26 years * 4 minerals * 3 scenarios

# Additional scenarios for contour plot ------------------

# levels
df |> group_by(Scenario) |> summarise(ktons = sum(ktons) / 1e3, .groups = "drop") |> arrange(ktons)
# Pick even interpolation lenghts for total mineral - 25 mtons per step

targets_sps_aps <- c(975, 1000, 1025)
targets_aps_nze <- c(1050, 1075, 1100, 1125)
# weights for interpolation between SPS and APS, and between APS and NZE
w_sps_aps <- (targets_sps_aps - 971) / (1040 - 971)
w_aps_nze <- (targets_aps_nze - 1040) / (1127 - 1040)

interp_pair <- function(df, w, s_low, s_high, prefix) {
  low <- df |> filter(Scenario == s_low)
  high <- df |> filter(Scenario == s_high)

  bind_rows(lapply(seq_along(w), function(i) {
    low |>
      left_join(high, by = c("Mineral", "Year"), suffix = c("_low", "_high")) |>
      mutate(
        Scenario = paste0(prefix, "_", i),
        ktons = (1 - w[i]) * ktons_low + w[i] * ktons_high,
        ktons_ev = (1 - w[i]) * ktons_ev_low + w[i] * ktons_ev_high,
      ) |>
      select(Mineral, Scenario, Year, ktons, ktons_ev)
  }))
}

interp_sps_aps <- interp_pair(df, w_sps_aps, "SPS", "APS", "SPS_APS")
interp_aps_nze <- interp_pair(df, w_aps_nze, "APS", "NZE", "APS_NZE")

df_interp <- bind_rows(df, interp_sps_aps, interp_aps_nze)
# levels
# df_interp |> group_by(Scenario) |> summarise(ktons = sum(ktons) / 1e3, .groups = "drop") |> arrange(ktons)

# Extrapolation - For contour plots - Even step lenght of 25mtons
targets_ext <- seq(725, 1400, 25)
m_sps <- targets_ext[targets_ext < 971] / 971
m_nze <- targets_ext[targets_ext > 1127] / 1127

extra_sps <- bind_rows(lapply(m_sps, function(m) {
  df |>
    filter(Scenario == "SPS") |>
    mutate(Scenario = paste0("SPS_x", round(m, 2)), ktons = ktons * m, ktons_ev = ktons_ev * m)
}))

extra_nze <- bind_rows(lapply(m_nze, function(m) {
  df |>
    filter(Scenario == "NZE") |>
    mutate(Scenario = paste0("NZE_x", round(m, 2)), ktons = ktons * m, ktons_ev = ktons_ev * m)
}))

df_interp <- bind_rows(df_interp, extra_sps, extra_nze)
df_interp |> group_by(Scenario) |> summarise(ktons = sum(ktons) / 1e3, .groups = "drop") |> arrange(ktons)

# Visual check
pdat <- df_interp |> group_by(Mineral, Scenario, Year) |> summarise(ktons = sum(ktons), .groups = "drop")

labs_df <- pdat |> filter(Year == max(Year))

ggplot(pdat, aes(Year, ktons, col = Scenario)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Mineral, scales = "free_y") +
  geom_text_repel(
    data = labs_df,
    aes(label = Scenario),
    direction = "y",
    hjust = 0,
    nudge_x = 0.5,
    segment.color = NA,
    size = 3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "Demand (ktons)", col = "") +
  theme_minimal()


table(df_interp$Scenario)


# RATIO SHARES guess based on ratios of Ni-Co, Ni-Li and Li-Co -----------

## ---- 1. intensity matrix A (rows = minerals, cols = chemistries) ----
# From BatPac5.2 (https://pubs.acs.org/doi/abs/10.1021/acs.est.5c12420)
# in kg per kWh
# fmt: skip
A <- matrix(c(
  0,           0.317596994,0.436148562403854, 0.607822242, 0.655632282,  # Nickel
  0,           0.318787104,0.175113165, 0.076262487, 0.123391704,  # Cobalt
  0.088958365, 0.115469158,0.105714031, 0.092243627, 0.099365656   # Lithium
), nrow = 3, byrow = TRUE)

rownames(A) <- c("Nickel", "Cobalt", "Lithium")
colnames(A) <- c("LFP", "NMC111", "NMC532", "NMC811", "NCA")

# Only EV mineral demand
df_ev <- df_interp %>%
  filter(Mineral %in% c("Nickel", "Cobalt", "Lithium")) %>%
  dplyr::select(-ktons) |>
  pivot_wider(names_from = Mineral, values_from = ktons_ev)

# Ni-Co ratio for EV demand
# Chemistry Ni/Co ratios
# NMC111 ~ 1
A[1, 4] / A[2, 4] # NMC811 ~ 8
A[1, 5] / A[2, 5] # NCA ~ 5.3
df_ev %>%
  filter(Scenario %in% c("SPS", "APS", "NZE")) %>%
  filter(Year %in% seq(2025, 2050, 5)) %>%
  mutate(ratio = Nickel / Cobalt) %>%
  ggplot(aes(x = factor(Year), y = ratio)) +
  geom_col(fill = "#378ADD") +
  facet_wrap(~Scenario) +
  labs(x = NULL, y = "Ni / Co ratio") +
  theme_pb_large()
# Ni-Co ratio tends to go towards 10

# Ni-Li ratio for EV Demand
A[1, 4] / A[3, 4] # NMC811 ~ 6.6
A[1, 5] / A[3, 5] # NCA ~ 6.6
# LFP zero Nickel, so no ratio
df_ev %>%
  filter(Scenario %in% c("SPS", "APS", "NZE")) %>%
  filter(Year %in% seq(2025, 2050, 5)) %>%
  mutate(ratio = Nickel / Lithium) %>%
  ggplot(aes(x = factor(Year), y = ratio)) +
  geom_col(fill = "#378ADD") +
  facet_wrap(~Scenario) +
  labs(x = NULL, y = "Ni / Li ratio") +
  theme_pb_large()

## Step one: Calculate LFP share based on Ni-Li ratio ------
NMC_Ni_Li <- 6.6

(lfp_share <- df_ev |>
  mutate(Ni_Li_ratio = Nickel / Lithium, Ni_Co_ratio = Nickel / Cobalt) |>
  mutate(share_LFP = 1 - (Nickel / Lithium) / NMC_Ni_Li) |>
  mutate(share_NMC811 = 1 - share_LFP))

# Figure
df_ev |>
  filter(Scenario %in% c("SPS", "APS", "NZE")) %>%
  filter(Year %in% seq(2025, 2050, 5)) %>%
  mutate(Ni_Li_ratio = Nickel / Lithium, Ni_Co_ratio = Nickel / Cobalt) |>
  mutate(share_LFP = 1 - (Nickel / Lithium) / NMC_Ni_Li) |>
  mutate(share_NMC811 = 1 - share_LFP) |>
  dplyr::select(Year, Scenario, share_LFP, share_NMC811) |>
  gather("Chemistry", "Share", -Year, -Scenario) |>
  mutate(Scenario = factor(Scenario, levels = c("SPS", "APS", "NZE"))) |>
  mutate(Chemistry = Chemistry |> str_remove("share_")) |>
  ggplot(aes(Year, Share, fill = Chemistry)) +
  geom_col(position = "stack",col = "black",linewidth=0.2) +
  facet_wrap(~Scenario) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(2025, 2050, 5)) +
  coord_cartesian(expand = F) +
  theme_pb_large()

# fmt: skip
ggsave("Figures/Demand/BatShare.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

# Save LFP share in main csv
df_save <- df_interp |> left_join(dplyr::select(lfp_share, Scenario, Year, share_LFP, Ni_Co_ratio))

# Scenarios for figure
# Scenarios for LFP share
alpha <- seq(0.3, 0.9, by = 0.1)
# Ni-Co ratio for NMC share (complement to LFP share)
ni_co_ratio <- seq(6, 12, 1)

ni_co_ratios <- df_ev |>
  filter(Scenario %in% c("SPS", "APS", "NZE")) %>%
  filter(Year %in% seq(2025, 2050, 5)) %>%
  mutate(Ni_Li_ratio = Nickel / Lithium, Ni_Co_ratio = Nickel / Cobalt) |>
  mutate(share_LFP = 1 - (Nickel / Lithium) / NMC_Ni_Li) |>
  mutate(share_NMC811 = 1 - share_LFP) |>
  mutate(B = Nickel + Cobalt) |> # Redistribute for NMC share
  cross_join(expand.grid(alpha_new = alpha, r_new = ni_co_ratio)) |>
  mutate(
    # r/(1+r) and 1/(1+r) split budget B into Ni and Co shares implied by the proposed ratio
    Ni_new = B * (1 - alpha_new) / share_LFP * (r_new / (1 + r_new)),
    Co_new = B * (1 - alpha_new) / share_LFP * (1 / (1 + r_new)),
    Li_new = Lithium
  ) |>
  mutate(test_ratio = Ni_new / Co_new, test_total = Ni_new + Co_new) # Check that the new ratios and totals are correct

ni_co_ratios |>
  mutate(Nickel = Ni_new, Cobalt = Co_new) |>
  filter(Scenario == "NZE") |>
  filter(Year == 2050) |>
  dplyr::select(Nickel, Cobalt, alpha_new, r_new) |>
  pivot_longer(c(Nickel, Cobalt), names_to = "mineral", values_to = "demand") |>
  mutate(aux = paste0(mineral, r_new)) |>
  mutate(alpha_new = paste0("LFP share =", round(alpha_new, 1) * 100, "%"), ) |>
  ggplot(aes(x = factor(r_new), y = demand, fill = mineral, group = factor(aux))) +
  geom_col(aes(alpha = factor(r_new)),col="black",linewidth=0.2) +
  facet_grid(mineral ~ alpha_new, scales = "free_y") +
  labs(y = "Battery Demand (ktons)", x = "Ni-Co Ratio assumption") +
  theme_pb_large() +
  theme(legend.position = "none")

# ratio maters for Cobalt
# fmt: skip
ggsave("Figures/Demand/Scenarios_NiCo.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*2)


# Save ----------------------

# Spread it to save
df_wide <- df_save |> mutate(Sector = NULL) |> pivot_wider(names_from = Mineral, values_from = c(ktons, ktons_ev))
nrow(df_wide) # 78 = 26 years * 3 scenarios
names(df_wide) <- names(df_wide) |> str_remove("ktons_")
write.csv(df_wide, "Parameters/IEA_Demand.csv", row.names = FALSE)


# Main Figure ----------------------

df <- df |> mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

data_fig <- df_all |>
  filter(!(Sector %in% c("Total demand", "Total clean technologies"))) |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium"))) |>
  filter(Scenario == "SPS") |>
  mutate(Sector = str_replace(Sector, "Low", "Other low") |> str_replace("emissions power", "emissions\npower")) |>
  mutate(
    Sector = factor(
      Sector,
      levels = c(
        "Solar PV",
        "Wind",
        "Other low emissions\npower generation",
        "Electric vehicles",
        "Grid battery storage",
        "Electricity networks",
        "Hydrogen technologies",
        "Other uses"
      )
    )
  ) |>
  group_by(Mineral, Year) %>%
  mutate(share = ktons / sum(ktons)) %>%
  ungroup() |>
  mutate(label_end = if_else(share > 0.3, Sector, ""))

# Direct labels (NZE/APS/SPS) shown only on the Lithium panel, bold + descriptive names
scen_label_lookup <- c(NZE = "Net-zero", APS = "Pledges", SPS = "Current policies")
df_scen_label <- df |> filter(Year == 2043, Mineral == "Lithium") |> mutate(scen_label = scen_label_lookup[Scenario])

ggplot(data_fig, aes(Year, ktons / 1e3)) +
  geom_area(aes(fill=Sector),col="darkgrey",linewidth=.1) +
  geom_line(data = df, aes(col = Scenario), linewidth = .7) +
  geom_text(
    data = df_scen_label,
    aes(label = scen_label, col = Scenario),
    hjust = 0.5,
    fontface = "bold",
    angle=15,
    nudge_y=0.1,
    size = 7 * 5 / 14 * 0.8
  ) +
  facet_wrap(~Mineral, scales = "free") +
  labs(y = "Demand (million metal metric tonnes)", title = NULL, x = "", col = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_color_manual(guide = "none", values = c("NZE" = "#C44E00", "APS" = "#5E8A00", "SPS" = "#1A6FA4")) +
  scale_fill_paletteer_d("MoMAColors::Klein", name = "Sector") +
  coord_cartesian(xlim = c(2025, 2050), ylim = c(0, NA)) +
  theme_pb_wide() +
  theme(legend.position = "right")


# fmt: skip
ggsave("Figures/Demand/MineralDemand.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

# EoF
