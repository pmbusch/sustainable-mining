# Common Variables

scens_selected <- read_excel("Inputs/Selected_Scenarios.xlsx")
scens_selected <- scens_selected$scen_all


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
