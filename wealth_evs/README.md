# Wealth and EVs by Postcode

This folder is a small public-facing exhibit that asks whether BEV registration stock is clustered in more advantaged Australian postcodes.

Main artifact: [`outputs/interactive_ev_wealth_map.html`](outputs/interactive_ev_wealth_map.html)

If GitHub Pages is enabled for the repository, `wealth_evs/index.html` will redirect to the interactive map.

## Headline

In the 2025 AAA registration stock snapshot, the most advantaged IRSAD decile has a BEV stock share of about 2.90%, compared with about 0.37% in the most disadvantaged decile. The most advantaged decile contains about 31.7% of all mapped BEVs.

That should be read as a stock pattern, not a sales pattern.

## Rebuild

From the repository root:

```r
Rscript wealth_evs/scripts/build_wealth_evs.R
```

Data products:

- `data/processed/postcode_ev_wealth_panel.csv`: annual postcode panel, 2021-2025, joined to ABS SEIFA 2021.
- `data/processed/irsad_decile_ev_summary.csv`: BEV stock/share summarized by IRSAD decile and year.
- `data/processed/postcode_ev_wealth_map.geojson`: map-ready joined GeoJSON.
- `data/processed/top_bev_share_postcodes_2025.csv`: high-BEV-share postcodes among postcodes with at least 500 registered vehicles.

Caveats:

- The AAA postcode data are registration stock snapshots as at 31 January, not new vehicle sales.
- BEV is clean; HEV and PHEV are combined in the AAA registration workbook.
- Postal Areas are ABS approximations of postcodes for statistical use.
- SEIFA is measured from the 2021 Census and held fixed across the EV stock years.

Sources:

- Australian Automobile Association EV Index registration workbook.
- ABS SEIFA 2021 Postal Area Indexes.
- AIHW-hosted simplified ABS ASGS Edition 3 Postal Area 2021 boundary service.
