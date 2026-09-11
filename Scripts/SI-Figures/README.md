# Supplementary Information Figures

Figures for the paper's Supplementary Information (SI), not the main body. Each script is self-contained and reads from `Parameters/` or `Results/Processed/` (produced by `Scripts/Figure2_PrepareData.R` and the main input-processing pipeline).

- **`Figure2SI_Biodiversity.R`** - Cost vs. water-stress trade-off under fish-biodiversity-weighted scenarios (companion to `Figure2_ParetoCurves.R`)
- **`Figure2SI_Production.R`** - Production decomposition by country
- **`Figure2SI_WaterMineral.R`** - Water impact decomposition by mineral
- **`Figure_ResourcesByWaterStress.R`** - Cumulative mineral resources by basin water-stress level
- **`Figure_MineralCostCurve.R`** - Mineral cost curves by deposit
- **`Figure_AWARE_Fish.R`** - Water stress vs. fish biodiversity index by deposit (moved here from `Inputs-Water/`, since it's a figure built from the final `Deposit.csv`, not a raw-input step)
