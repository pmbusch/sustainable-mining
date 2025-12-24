# Analysis Results of Optimization
# Common script to load optimization results from Julia in a fast way, and process the results accordingly
# Need to specify the runs variable beforehand
# PBH March 2024

# Input Parameters
demand <- read.csv("Parameters/Cu_Demand.csv")
deposit <- read.csv("Parameters/Cu_Deposit.csv")


(d_size <- nrow(deposit))
(t_size <- nrow(filter(demand, str_detect(Scenario, "Enhanced rec"))))

# prod_rate <- expand.grid(Deposit_Name = unique(deposit$Deposit_Name), t = unique(demand$t)) %>%
#   left_join(dplyr::select(deposit, Deposit_Name, prod_rate2022, prod_rate2023, prod_rate2025, prod_rate2030)) %>%
#   mutate(
#     prod_rate = case_when(
#       t == 2022 ~ prod_rate2022,
#       t == 2023 ~ prod_rate2023,
#       t == 2024 ~ (prod_rate2023 + prod_rate2025) / 2, # interpolation
#       t == 2025 ~ prod_rate2025,
#       t >= 2026 & t <= 2029 ~ (1 - (t - 2025) / 5) * prod_rate2025 + ((t - 2025) / 5) * prod_rate2030,
#       T ~ prod_rate2030
#     )
#   ) %>%
#   dplyr::select(Deposit_Name, t, prod_rate)

# opt_param <- read.csv(file.path(runs[1], "OptimizationInputs.csv"))
# (bigM_cost <- opt_param[2, 2])
# (discount_rate <- opt_param[1, 2])

# Load Results --------

# Read all results and put them in the same dataframe!
df_results <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(read.csv(folder_path), run = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
) %>%
  rename(Name = d)

df_results$Scenario %>% unique()

# unique(df_results$run)
df_results <- df_results |>
  mutate(cost_deg = run |> str_remove_all("Water|\\.csv") |> as.numeric()) |>
  mutate(cost_deg = if_else(is.na(cost_deg), 0, cost_deg)) # full cost
table(df_results$cost_deg)

# dict of scenarios - specified beforehand
df_results <- df_results %>% left_join(dict_scen) %>% mutate(name = factor(name, levels = name_abbr))
demand <- demand %>% left_join(dict_scen) %>% mutate(name = factor(name, levels = name_abbr))
table(df_results$name)

# EoF
