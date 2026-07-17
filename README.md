# Australia Open Vehicle Sales Reconstruction

This project reconstructs Australian new vehicle sales by vehicle type and fuel / EV type for roughly 2016-2026 using public online sources only.

Scope convention: `2016-2025` are full calendar years; `2026` is treated as YTD/monthly until a full-year 2026 VFACTS-style roundup exists in January 2027.

The goal is not to clone paywalled VFACTS perfectly. The goal is a reproducible public-interest dataset with source grades, uncertainty, and clear separation between observed figures, derived estimates, and gaps.

## Research Target

Primary panel:

```text
period | geography | vehicle_type | powertrain | sales | estimate_status | source
```

Preferred vehicle types:

- passenger
- SUV
- light_commercial
- light_rigid_truck
- heavy_rigid_truck
- articulated_truck
- bus
- motorcycle
- other

Preferred powertrains:

- ICE_petrol
- ICE_diesel
- HEV
- PHEV
- BEV
- FCEV
- other_or_unknown

## Method

1. Collect public observations into `data/seed/observations.csv`.
2. Track every source in `data/seed/source_register.csv`.
3. Use direct figures where public sources report them.
4. Use derived estimates only where the formula is explicit.
5. Keep monthly, quarterly, annual, sales, and registration-stock observations in one ledger, then build analysis tables from that ledger.
6. Validate annual new-sales estimates against registration-stock changes from ABS/BITRE/NEVDIS and known VFACTS media summaries.

## Source Grades

- `A`: official public statistics or primary regulator/source.
- `B`: reputable media or industry body quoting VFACTS/AAA/EVC directly.
- `C`: tertiary compilation such as Wikipedia, useful for discovery but should be backfilled with primary or media sources.
- `D`: derived estimate from public observations.

## Current State

This repository currently contains a starter ledger, not a finished dataset. It includes:

- ABS 2021 fleet stock validation points.
- Publicly reported 2024 total market and segment shares.
- Publicly reported annual plug-in EV totals for 2016-2024.
- Publicly reported 2026 monthly EV sales/share observations.
- Publicly reported or derived 2016-2020 annual market totals, broad vehicle-class totals, and selected fuel/powertrain totals.
- A 2024 top-100 model sales table from a public VFACTS media roundup.
- A starter model-to-vehicle-type and model-to-powertrain lookup.
- Full-year category and fuel breakdowns for 2021-2023, plus 2024 public broad shares and top-100 model coverage.
- A scrape-target manifest for the 2016-2026 source collection campaign.

Next best step: reconcile overlapping 2016-2021 fuel observations and add uncertainty flags before fitting projection curves.

## Build

Run:

```r
Rscript scripts/ingest_aaa_ev_index.R
Rscript scripts/build.R
```

Outputs:

- `data/raw/aaa_ev_index_YYYY-MM-DD.csv`
- `data/processed/aaa_ev_index_long.csv`
- `data/processed/aaa_sales_by_state_vehicle_type_fuel_quarter.csv`
- `data/processed/aaa_sales_by_vehicle_type_fuel_quarter.csv`
- `data/processed/aaa_sales_by_broad_vehicle_type_fuel_quarter.csv`
- `data/processed/aaa_sales_by_broad_vehicle_type_fuel_year.csv`
- `data/processed/aaa_ev_index_coverage.csv`
- `data/processed/aaa_reconciliation_with_seed_totals.csv`
- `data/processed/sales_panel_annual.csv`
- `data/processed/sales_panel_annual_preferred.csv`
- `data/processed/sales_panel_annual_quality_scored.csv`
- `data/processed/sales_panel_projection_input.csv`
- `data/processed/sales_panel_overlap_diagnostics.csv`
- `data/processed/sales_panel_coverage_summary.csv`
- `data/processed/sales_panel_internal_consistency.csv`
- `data/processed/annual_plugin_sales_seed.csv`
- `data/processed/model_sales_enriched.csv`
- `data/processed/model_sales_by_vehicle_type.csv`
- `data/processed/model_sales_by_powertrain_profile.csv`
- `data/processed/model_sales_period_summary.csv`
- `data/processed/model_powertrain_observations_seed.csv`
- `data/processed/known_model_powertrain_2024_by_type.csv`
- `data/processed/diagnostics_2024.csv`
