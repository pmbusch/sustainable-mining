# Venn diagram showing coproduction occurance between active deposits
# Source: S&P Data
# PBH Jan 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# LOAD AND MERGE -----------

cu1 <- read_excel('Inputs/SP/SP_Copper_28Jan2026_USA_Europe.xls', skip = 3, col_types = "text") |> slice(-1, -2)
cu2 <- read_excel('Inputs/SP/SP_Copper_28Jan2026_ROW.xls', skip = 3, col_types = "text") |> slice(-1, -2)
ni <- read_excel('Inputs/SP/SP_Nickel_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)
co <- read_excel('Inputs/SP/SP_Cobalt_28Jan2026.xls', skip = 3, col_types = "text") |> slice(-1, -2)

# Merge all and delete duplicas by deposit ID
df <- rbind(
  mutate(cu1, Database = "Copper"),
  mutate(cu2, Database = "Copper"),
  mutate(ni, Database = "Nickel"),
  mutate(co, Database = "Cobalt")
) |>
  dplyr::select(PROP_NAME, PROP_ID, PRIMARY_COMMODITY, ACTV_STATUS, CONTAINED_R_AND_R_PCT_TONNE, Database)

# Filter by status
df <- df |>
  filter(
    ACTV_STATUS %in% c("Active", "On Hold Awaiting Financing", "On Hold Awaiting Higher Prices", "Temporarily On Hold")
  ) |>
  mutate(
    PRIMARY_COMMODITY = if_else(PRIMARY_COMMODITY %in% c("Copper", "Nickel", "Cobalt"), PRIMARY_COMMODITY, "Other")
  ) |>
  rename(resources = CONTAINED_R_AND_R_PCT_TONNE) |> # Resources including reserves
  mutate(resources = as.numeric(resources)) |>
  filter(!is.na(resources))
table(df$PRIMARY_COMMODITY)

# Figure - Venn Diagram -----------

library(ggVennDiagram)

# Get a datafame by PROP_ID indicating if it exists in any database with other coproduct...
prop_db <- df %>% distinct(PROP_ID, Database, PRIMARY_COMMODITY)
prop_ids <- df %>% distinct(PROP_ID)
prop_presence <- prop_ids %>%
  left_join(
    prop_db %>% filter(Database == "Copper") %>% distinct(PROP_ID) %>% mutate(in_copper_db = TRUE),
    by = "PROP_ID"
  ) %>%
  left_join(
    prop_db %>% filter(Database == "Nickel") %>% distinct(PROP_ID) %>% mutate(in_nickel_db = TRUE),
    by = "PROP_ID"
  ) %>%
  left_join(
    prop_db %>% filter(Database == "Cobalt") %>% distinct(PROP_ID) %>% mutate(in_cobalt_db = TRUE),
    by = "PROP_ID"
  ) %>%
  left_join(
    prop_db %>% filter(PRIMARY_COMMODITY == "Other") %>% distinct(PROP_ID) %>% mutate(in_other_db = TRUE),
    by = "PROP_ID"
  ) %>%
  mutate(across(starts_with("in_"), ~ replace_na(.x, FALSE)))

sets <- list(
  Copper = prop_presence %>% filter(in_copper_db) %>% pull(PROP_ID),
  Nickel = prop_presence %>% filter(in_nickel_db) %>% pull(PROP_ID),
  Cobalt = prop_presence %>% filter(in_cobalt_db) %>% pull(PROP_ID),
  Other = prop_presence %>% filter(in_other_db) %>% pull(PROP_ID)
)

ggVennDiagram(set_names(sets, rep("", length(sets))), label_alpha = 0, label = "count", set_size = 3.5) +
  annotate(
    "text",
    x = c(0.8, 2, 4, 5) * 0.18,
    y = c(2.7, 2.95, 2.95, 2.7) * 0.3,
    label = paste0(c("Copper", "Nickel", "Cobalt", "Other"), "\n(n=", lengths(sets), ")"),
    size = 4,
    lineheight = 0.8
  ) +
  annotate("text", x = 0.41, y = 0.62, label = "Cu-Ni-Co", size = 3) +
  scale_fill_gradient(low = "grey90", high = "red") +
  theme(legend.position = "none", text = element_text(hjust = 1))

# fmt: skip
ggsave("Figures/Deposit/Coproducts.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*1.5, height = 8.7,bg="white")
