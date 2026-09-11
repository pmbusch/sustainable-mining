# Inputs

Raw external data feeding the pipeline (see `Scripts/Inputs-Demand/`, `Inputs-Deposit/`, `Inputs-Water/`). Several subfolders are excluded from GitHub via `.gitignore` because they are proprietary, very large, or unused - see the root `README.md` "Data Availability" section for full download/access instructions.

| Folder | Contents | On GitHub? |
|---|---|---|
| `IEA/` | IEA Critical Minerals Data Explorer (demand scenarios) | Yes |
| `SP/` | S&P Global (Capital IQ Pro) deposit exports - **proprietary, licensed** | No - see root README |
| `AWARE/` (root files) | AWARE 2.0 baseline characterization factors | Yes |
| `AWARE/Stochastic/` | AWARE 2.0 Monte Carlo CF ensemble (Zenodo, ~6 GB) | No - download link in root README |
| `FW_FISH.zip`, `FW_FISH/` | IUCN Red List freshwater fish range shapefiles (~2.5 GB zipped) | No - download link in root README |

