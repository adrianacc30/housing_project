# Running the selected R scripts

Use R 4.1 or later and open the repository root as your working directory. All paths are relative to that folder.

## Setup and plots

Install dependencies once:

```r
source("install_packages.R")
```

Generate the four figures:

```r
source("run_all.R")
```

From a terminal, the equivalents are `Rscript install_packages.R` and `Rscript run_all.R`.

The plotting workflow reads the aggregate CSVs and municipal boundary snapshot already supplied in `data/processed/`. It writes PNGs to `figures/`, replacing files with the same names. It does not download source PDFs or install packages automatically.

## Script guide

| File | Purpose |
| --- | --- |
| `scripts/00_helpers.R` | Relative paths, required packages, input schemas, and neighbourhood-join checks. |
| `scripts/01_extract_data.R` | Optional PDF downloads and extraction of population, household, dwelling-stock, and completion tables. |
| `scripts/02_bivariate_maps.R` | Both maps with a three-by-three quantile legend. |
| `scripts/03_household_dwelling_growth.R` | Household and dwelling-stock growth between year-end 2024 and 2025. |
| `scripts/04_stock_vs_completions.R` | Room-size shares in existing stock and completed dwellings. |

To rebuild the CSVs, run `source("scripts/01_extract_data.R")`. This downloads missing official reports into `data/raw/`, validates row counts and totals, and writes the aggregate CSVs. PDF parsing is specific to the cited report editions and pages. The boundary snapshot stays fixed to the bundled version.

## Preparation and validation

The figures were freshly exported from the official tables for this public copy, with clear headings and an explicit age definition. The original indicator formulas were retained. Neighbourhood-level rows are used throughout; district totals are excluded to avoid counting the same population or stock twice.

The published city totals, row sums, 38 neighbourhood codes, joins, and completion total were checked. The R files passed syntax validation. A native R runtime was unavailable during preparation: source-table calculations and the bundled publication figures were independently regenerated in Python. The R scripts implement the same indicators, but execution in R and exact rendering have not been verified; plotting styles may differ.

The national rent/entrant matching and DSW student-housing experiments are outside this focused neighbourhood package.
