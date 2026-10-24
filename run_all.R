# Run from the repository root after installing dependencies.
# Bundled aggregate CSVs and boundary geometry are sufficient for these plots.
source("scripts/02_bivariate_maps.R")
source("scripts/03_household_dwelling_growth.R")
source("scripts/04_stock_vs_completions.R")
message("Figures saved to figures/.")
