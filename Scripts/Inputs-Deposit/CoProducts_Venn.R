# Venn diagram showing coproduction occurance between active deposits
# Source: S&P Data
# Use pre-processed deposit dataset
# PBH Jan 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# LOAD AND MERGE -----------

df <- read.csv("Parameters/CuNiCo_Deposit.csv")

names(df)

df <- df |>
  mutate(
    PRIMARY_COMMODITY = if_else(PRIMARY_COMMODITY %in% c("Copper", "Nickel", "Cobalt"), PRIMARY_COMMODITY, "Other")
  )


# Figure - Venn Diagram -----------

library(ggVennDiagram)

# Database is already on format one deposit per row, with columns indicated presence of minerals
prop_db <- df %>% distinct(ID, , PRIMARY_COMMODITY)
prop_ids <- df %>% distinct(ID)
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

prop_presence <- df |>
  mutate(
    in_copper_db = grade_resource_Copper > 0,
    in_nickel_db = grade_resource_Nickel > 0,
    in_cobalt_db = grade_resource_Cobalt > 0,
    in_other_db = PRIMARY_COMMODITY == "Other"
  )

sets <- list(
  Copper = prop_presence %>% filter(in_copper_db) %>% pull(ID),
  Nickel = prop_presence %>% filter(in_nickel_db) %>% pull(ID),
  Cobalt = prop_presence %>% filter(in_cobalt_db) %>% pull(ID),
  Other = prop_presence %>% filter(in_other_db) %>% pull(ID)
)

ggVennDiagram(set_names(sets, rep("", length(sets))), label_alpha = 0, label = "count", set_size = 3.5) +
  annotate(
    "text",
    x = c(0.144, 0.36, 0.72, 0.9),
    y = c(.81, .885, .885, .81),
    label = paste0(c("Copper", "Nickel", "Cobalt", "Other"), "\n(n=", lengths(sets), ")"),
    size = 4,
    lineheight = 0.8
  ) +
  annotate("text", x = 0.41, y = 0.62, label = "Cu-Ni-Co", size = 3) +
  annotate("text", x = 0.2, y = 0.56, label = "Cu", size = 3) +
  annotate("text", x = 0.32, y = 0.46, label = "Cu-Co", size = 3) +
  annotate("text", x = 0.31, y = 0.7, label = "Cu-Ni", size = 3) +
  annotate("text", x = 0.36, y = 0.8, label = "Ni", size = 3) +
  annotate("text", x = 0.5, y = 0.72, label = "Ni-Co", size = 3) +
  annotate("text", x = 0.63, y = 0.8, label = "Co", size = 3) +
  scale_fill_gradient(low = "grey90", high = "red") +
  theme(legend.position = "none", text = element_text(hjust = 1))

# fmt: skip
ggsave("Figures/Deposit/Coproducts.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*1.5, height = 8.7,bg="white")
