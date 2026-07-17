# Reconstruction Plan

## Phase 1: Public Fact Ledger

Build a ledger of every public observation. Do not collapse observations too early.

Useful observation classes:

- Annual total new vehicle sales.
- Annual broad segment shares: passenger, SUV, light commercial, heavy commercial.
- Annual or quarterly fuel shares: petrol/diesel, HEV, PHEV, BEV.
- Monthly 2026 observations.
- Model-level sales for top vehicles where public.
- ABS/BITRE/NEVDIS stock totals by vehicle type and fuel type.

Target period:

- Full calendar years: 2016-2025.
- 2026: monthly/YTD observations only until the year is complete.

The scrape/source campaign is tracked in `data/seed/scrape_targets.csv`.

## Phase 2: Model Lookup

Create a model dictionary:

```text
make | model | year_start | year_end | vehicle_type | likely_powertrain | notes
```

This unlocks model-level reconstruction from top-100 lists and manufacturer press releases.

Examples:

- Tesla Model Y: SUV, BEV.
- Tesla Model 3: passenger, BEV.
- Ford Ranger: light_commercial, mostly diesel until PHEV introduction.
- Toyota RAV4: SUV, ICE/HEV; PHEV arrives later in Australia.
- Mitsubishi Outlander: SUV, ICE/PHEV.
- BYD Atto 3: SUV, BEV.
- BYD Shark 6: light_commercial, PHEV.

Current implementation:

- `data/seed/model_sales_observations.csv` stores public top-model sales observations.
- `data/seed/model_lookup.csv` stores vehicle-type and powertrain-profile assumptions.
- `data/seed/model_powertrain_observations.csv` stores explicit model-powertrain splits where public sources report them.

The lookup deliberately uses broad powertrain profiles such as `mixed_ICE_HEV` or `mixed_ICE_PHEV` where a model has multiple drivetrain options. Those rows should not be collapsed into exact fuel cells until we have sub-model evidence or a transparent allocation rule.

## Phase 3: Annual Panel

For each year, solve a constrained allocation problem:

```text
sum(vehicle_type, powertrain) = annual_total_sales
sum(powertrain = BEV/PHEV/HEV) = reported fuel totals where available
sum(vehicle_type = SUV/LCV/passenger) = reported segment totals where available
```

Where exact cross-tabs are unavailable, use model-level sales and segment priors to allocate the residual.

## Phase 4: Confidence Bands

Every final cell should have:

- `sales_estimate`
- `lower_bound`
- `upper_bound`
- `method`
- `source_grade`

This matters because the public data are uneven. A transparent interval is better than a false point estimate.

## Likely Data Quality by Cell

Best:

- annual total new vehicle sales
- total BEV sales after 2021
- top model sales
- 2024 broad segment shares

Good enough:

- passenger/SUV/LCV annual totals from media roundups
- quarterly fuel shares from AAA/EVC

Hard:

- cross-tab of vehicle type by fuel type
- heavy trucks by fuel type
- PHEV vs HEV consistency across publishers

## Research Argument

The lack of open sales microdata is itself policy-relevant. Australia has national transport decarbonisation targets and a New Vehicle Efficiency Standard, yet core market-monitoring data are paywalled. A reproducible reconstruction with uncertainty is a defensible public-interest contribution.
