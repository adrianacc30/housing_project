# Housing and Demography in Mannheim

How does Mannheim's housing stock compare with its population and household structure?

This R project combines municipal population, household, and housing statistics for **38 neighbourhoods**, using bivariate maps and comparisons of **2024–2025** changes and new construction.

## Highlights

- Young adults and smaller dwellings are concentrated in overlapping neighbourhoods.
- Household and dwelling growth vary considerably across the city.
- **One- and two-room dwellings account for 53.7% of 2025 completions, compared with 22.2% of the end-2024 stock.**

## Figures

**Households with children and larger dwellings**

![Families and larger dwellings](figures/01_families_large_dwellings.png)

**Young adults and smaller dwellings**

![Young adults and smaller dwellings](figures/02_young_adults_small_dwellings.png)

<details>
<summary>Growth and construction mix</summary>

![Household and dwelling growth](figures/03_household_dwelling_growth.png)

![Existing stock versus completions](figures/04_stock_vs_completions.png)

</details>

## Code and data

Selected R scripts cover PDF-table extraction, spatial joins, mapping, and plotting. Aggregate inputs are included.

**Run:** `Rscript run_all.R`. See [setup instructions](RUNNING.md) and [data definitions and sources](DATA.md).

*Young adults are aged 18 to under 30. Room counts include kitchens. Neighbourhood associations do not reveal which households occupy which dwellings; completions are not net stock growth.*

**Source:** [City of Mannheim, municipal statistics](https://www.mannheim.de/de/stadt-gestalten/daten-und-fakten). Calculations based on published aggregate tables.
