# Common Variables

scens_selected <- read_excel("Inputs/Selected_Scenarios.xlsx")
scens_selected <- scens_selected$scen_all

# Create a named vector of colors for each region
region_colors <- c(
  "United States" = "#c4dfbe",
  "Mexico" = "#33a02c",
  "Canada" = "#ff7f00",
  "Brazil" = "#e31a1c",
  "Other Latin America and Caribbean" = "#6a3d9a",
  "European Union" = "#fbe484",
  "EFTA" = "#b2df8a",
  "United Kingdom" = "#fb9a99",
  "Other Europe" = "#fdbf6f",
  "China" = "#d74c5a",
  "Japan" = "#0d0d0fff",
  "South Korea" = "#fdb462",
  "ASEAN" = "#66c2a5",
  "India" = "#ff7f50",
  "Australia/NZ" = "#cab2d6",
  "Other Asia Pacific" = "#8b008b",
  "Middle East" = "#8b4513",
  "Africa" = "#4682b4",
  "Rest of the World" = "#808080"
)


# Names of scenarios
scens_names <- c(
  "(1) Reference",
  "(2) Large Capacity LIB",
  "(3) Small Capacity LIB",
  "(4) NMC811 Dominant Chemistry",
  "(5) LFP Dominant Chemistry",
  "(6) Solid State Adoption",
  "(7) SIB Adoption",
  "(8) Enhanced Repurposing",
  "(9) Enhanced Recycling",
  "(10) US Recycling",
  "(11) Medium Recycling + \nSmall Capacity LIB"
)

# Abbreviation
name_abbr <- c(
  "Ref.",
  "Large LIB",
  "Small LIB",
  "NMC811",
  "LFP",
  "Solid State",
  "SIB",
  "Repurp.",
  "Recyc.",
  "US Recyc.",
  "Med. Rec.+ Small LIB"
)
