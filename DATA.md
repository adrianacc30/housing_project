# Data definitions and sources

## Scope

The bundled inputs contain published, aggregated statistics for Mannheim's 38 Stadtteile. Each neighbourhood code is a three-character string, such as `011`; preserve its leading zero. The original reports also list 17 Stadtbezirke, which are excluded from the CSVs to prevent double counting.

Population, household, and stock measures refer to 31 December of the stated year. The population measure is *Wohnberechtigte Bevölkerung*, including residents with a main or secondary residence, rather than main-residence population alone.

## Original reports

| Dataset | Official report | Page/table used |
| --- | --- | --- |
| Population by age, 2025 | [Bevölkerungsbestand 2025, No. 1/2026](https://www.mannheim.de/sites/default/files/2026-03/d202601_einwohnerbestand_2025.pdf) | Page 8, Table 3. |
| Household types, 2024 | [Privathaushalte 2024, No. 2/2025](https://www.mannheim.de/sites/default/files/2025-03/d202502_privathaushalte_2024.pdf) | Page 9, Table 3. |
| Household types, 2025 | [Privathaushalte 2025, No. 2/2026](https://www.mannheim.de/sites/default/files/2026-04/d202602_privathaushalte_2025.pdf) | Page 9, Table 3. |
| Dwelling stock, 2024 | [Wohnungsbau 2024, No. 9/2025](https://www.mannheim.de/sites/default/files/2025-09/d202509_wohnungsbau_2024.pdf) | Page 5, Table 1. |
| Dwelling stock and completions, 2025 | [Wohnungsbau 2025, No. 9/2026](https://www.mannheim.de/sites/default/files/2026-08/d202609_wohnungsbau_2025.pdf) | Page 5, Table 1; page 15, Table 8. |

Source: Stadt Mannheim, Kommunale Statistikstelle. Neighbourhood names are standardised to the municipal boundary layer's names. Reports were retrieved on 24 October 2026; the full PDFs are not bundled.

## Files

- `age_2025.csv`: `code`, `name`, total population, and age groups `u18`, `a18_25`, `a25_30`, `a30_65`, `a65_80`, `o80`. Upper age limits are exclusive except the open-ended 80+ category.
- `households_2024.csv` and `households_2025.csv`: `total_hh`, `single_hh`, `multi_no_kids`, and `multi_with_kids`, using the report's household-type categories. Children are under 18.
- `dwellings_2024.csv` and `dwellings_2025.csv`: `total`, `w1` through `w6`, `w7plus`, and `total_rooms`.
- `completions_2025.csv`: seven room-size categories and `completions_count`; counts sum to 691.
- `neighbourhoods.geojson`: a municipal boundary snapshot, with only neighbourhood code and name as attributes. Coordinates use ETRS89 / UTM zone 32N, EPSG:25832, in metres. Source: [City of Mannheim WFS, Stadtteile](https://www.gis-mannheim.de/mannheim/mod_ogc/wfs_getmap.php?mapfile=stadtteil_grenze&service=WFS&Request=GetCapabilities), retrieved 24 October 2026.

## Indicators

| Indicator | Calculation |
| --- | --- |
| Households with children (%) | `100 * multi_with_kids / total_hh`. |
| Young adults (%) | `100 * (a18_25 + a25_30) / total population`. This is ages 18–29. |
| Smaller dwellings (%) | `100 * (w1 + w2) / total dwellings`. |
| Larger dwellings (%) | `100 * (w4 + w5 + w6 + w7plus) / total dwellings`. |
| Growth (%) | `100 * (2025 / 2024 - 1)` for each count; undefined when the 2024 count is zero. |
| Stock/completion shares | Each room-size count divided by the relevant citywide total. |

Each map variable is classified independently into thirds using sample quantiles; the two classes form nine colours. Numeric cut points differ between the two maps. The maps show neighbourhood composition, rather than directly matching individual households to homes.

The housing reports define rooms as rooms of at least 6 m² plus all kitchens. These room counts therefore differ from property listings that exclude kitchens. Completed dwellings include building work on existing buildings and exclude demolitions; their mix is not a measure of net additions by size or of unmet housing demand.

## Checked totals

| Measure | 2024 | 2025 |
| --- | ---: | ---: |
| Private households | 178,589 | 179,498 |
| Dwelling stock | 174,979 | 175,608 |
| Registered population, main and secondary residences | — | 333,778 |

The six age groups, household types, and seven room-size categories were checked against their row totals. Boundary and table codes match for all 38 neighbourhoods.
