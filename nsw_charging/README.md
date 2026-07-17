# NSW Charging and Wealth

This folder is a separate public-facing exhibit for NSW EV charging infrastructure. It maps charging sites directly from coordinates and aggregates plugs/sites to postcodes for SEIFA wealth analysis.

Main artifact: [`outputs/interactive_nsw_charging_wealth_map.html`](outputs/interactive_nsw_charging_wealth_map.html)

If GitHub Pages is enabled for the repository, `nsw_charging/index.html` redirects to the interactive map.

## Headline

- Charging sites: 1,958
- Charging plugs: 5,626
- DC plugs: 1,658
- Upcoming plugs: 543
- Postcodes with at least one known charger: 420

The wealth relationship should be read cautiously: dense inner-city and tourism postcodes can have lots of charging even where resident vehicle stock is small. The map therefore includes both absolute plug counts and normalised rates.

## Rebuild

From the repository root:

```r
Rscript wealth_evs/scripts/build_wealth_evs.R
Rscript nsw_charging/scripts/build_nsw_charging.R
```

## Data Products

- `data/processed/nsw_charging_sites_clean.csv`: cleaned site-level charger data.
- `data/processed/nsw_charging_postcode_panel.csv`: postcode-level charging, SEIFA, and 2025 EV stock panel.
- `data/processed/nsw_charging_irsad_decile_summary.csv`: charging access by IRSAD wealth decile.
- `data/processed/nsw_charging_postcode_map.geojson`: map-ready postcode polygons.

## Caveats

- The charging CSV is a snapshot supplied by the project owner and should be source-checked before publication.
- The map uses CSV coordinates for site points and parsed postcode/address values for postcode aggregation.
- SEIFA is fixed to ABS 2021 Postal Area indexes.
- Postcode denominators use 2025 AAA registration stock from the `wealth_evs` build.

