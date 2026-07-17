# AAA Electric Vehicle Index Notes

Source: https://www.aaa.asn.au/research-data/electric-vehicle/

The AAA Electric Vehicle Index is likely the strongest public source found so far for the core reconstruction task.

The page states that the dashboard data can be used if the AAA is credited and linked. Its embedded page data exposes a public Google Sheets CSV URL, but the live dashboard data is served through the site's public GraphQL endpoint.

Observed data structure:

```text
STATE | VEHICLE TYPE | MANUFACTURER | MODEL | FUEL TYPE | Q1 2022 | ... | Q1 2024
```

The GraphQL query used by `scripts/ingest_aaa_ev_index.R` is:

```graphql
{ evIndexData { data } }
```

Each returned row is a JSON string with the same fields as the published CSV.

This is directly useful for:

- vehicle type by fuel type
- model-level BEV/PHEV/Hybrid/ICE splits
- state-level sales by fuel and model
- quarterly aggregation from 2022 onward

Important caveat resolved: the default published Google Sheets CSV inspected locally covered `Q1 2022` through `Q1 2024`, while the dashboard text references examples for `Q1 2026`. The live GraphQL endpoint exposes the newer dashboard data through `Q1 2026`.

Next extraction step:

1. Run `Rscript scripts/ingest_aaa_ev_index.R`.
2. Inspect `data/processed/aaa_ev_index_coverage.csv` to confirm the current live coverage.
3. Normalize quarters into a long table:

```text
quarter | state | vehicle_type | manufacturer | model | fuel_type | sales
```

4. Aggregate to:

```text
quarter | geography | vehicle_type | fuel_type | sales
year | geography | vehicle_type | fuel_type | sales
```

Current script outputs:

- `data/processed/aaa_ev_index_long.csv`
- `data/processed/aaa_sales_by_state_vehicle_type_fuel_quarter.csv`
- `data/processed/aaa_sales_by_vehicle_type_fuel_quarter.csv`
- `data/processed/aaa_sales_by_broad_vehicle_type_fuel_quarter.csv`
- `data/processed/aaa_sales_by_broad_vehicle_type_fuel_year.csv`
- `data/processed/aaa_ev_index_coverage.csv`
- `data/processed/aaa_reconciliation_with_seed_totals.csv`

Remaining investigation:

- Add the AAA vehicle specification CSV and registration workbook as auxiliary stock/specification layers.
- Decide whether to treat AAA quarterly sales as the authoritative 2022-Q1 2026 backbone and use VFACTS-media reconstructions mainly for 2016-2021 and annual validation.
