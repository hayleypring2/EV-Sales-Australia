# GVG, VESR, and RAV/NVES Data Hunt Notes

Accessed: 2026-07-17

## Green Vehicle Guide

The Green Vehicle Guide (GVG) is the best public route found for model/variant-level Australian vehicle specifications across ICE, hybrid, plug-in hybrid, BEV, hydrogen, and other fuel types.

The public search page exposes year, make, model, variant, transmission, GVG vehicle class, body style, fuel type, seats, and drivetrain filters. Search results include model, body, engine/fuel type, transmission, drivetrain, tailpipe CO2, annual fuel cost, fuel consumption, energy consumption, electric range, air-pollution standard, annual tailpipe CO2, fuel-lifecycle CO2, noise data, and test cycle. The UI also offers CSV export.

The scraper in `scripts/ingest_gvg_search.R` posts to the public search form and parses the result table. The page defaults to a maximum of 200 vehicles, so year/fuel queries that hit 200 rows are probably capped. The script splits capped searches by GVG vehicle class by default. For a complete ICE inventory, any class-level query still returning 200 rows should be further split by make before treating coverage as complete.

The initial 2025 pilot was run with manufacturer splitting enabled for capped class slices. It produced 2,072 current-GVG rows across petrol 91, diesel, pure electric, and common plug-in hybrid fuel IDs, with no capped query used directly in the final output.

GVG also provides a direct older-model CSV for 1986-2003 fuel consumption. This is useful for historical fleet/emissions imputation, but it is not directly comparable with the post-2004 GVG search tables.

Relevant URLs:

- https://www.greenvehicleguide.gov.au/pages/ToolsAndCalculators/HowToGuide
- https://www.greenvehicleguide.gov.au/Vehicle/Search
- https://www.greenvehicleguide.gov.au/pages/ToolsAndCalculators/SearchOlderVehicles
- https://www.greenvehicleguide.gov.au/Content/OlderModels/FuelConsumptionGuide1986-2003.csv

## VESR

The Vehicle Emissions Star Rating site is useful as a public-facing comparison tool, but it appears to draw from GVG data under licence. Its home page includes a limited end-user licence notice for GVG-derived views. Treat VESR as a discovery/reference source, not the primary source for a redistributable dataset unless permission terms are clarified.

Relevant URL:

- https://www.vesr.gov.au/

## RAV/NVES

The Register of Approved Vehicles and New Vehicle Efficiency Standard are important for future schema design. The public RAV submission template and NVES regulator guidance confirm that from 1 July 2025 covered-vehicle RAV entries need NVES vehicle type, CO2 emissions, mass in running order, and rated towing capacity. This is promising for future compliance-era data, but the hunt has not yet found a public bulk RAV/NVES extract for model-level research use.

Relevant URLs:

- https://www.infrastructure.gov.au/department/media/publications/rav-submission-template
- https://www.nvesregulator.gov.au/complying-nves/calculating-ievs-and-issuing-units
