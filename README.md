# VMT-Rates

R scripts for computing region-level Vehicle Miles Traveled (VMT) rates across
California, along with the supporting transit-eligibility and area-median-income
calculation scripts that generate necessary inputs.

## Scripts

### `VMT_Rates.R`

The main analysis script. For each California region defined in the county
crosswalk, it computes population-weighted VMT rates for defined criteria areas using residential VMT and population data from
Replica, plus tract-level spatial overlays for High Quality Transit Areas
(HQTAs) and transit-eligible areas.

The script:

- Loads 2020 Census tracts and counties, the region crosswalk, and the HQTA /
  transit-eligible layers.
- Pre-computes tract coverage ratios against each transit overlay (overlap area
  ÷ tract area) once, outside the per-region loop.
- Reads statewide all-income VMT once, then reads the region-specific
  low-income VMT inside the loop.
- Classifies each tract as ≥15% below the regional per-capita VMT rate or between that threshold
  and the regional average.
- Computes Criteria 1–3 VMT rates for both tables, plus a combined
  non-overlapping rate per table. Table 3's combined area subtracts Table 2's
  combined area so the two tables never overlap.
- Saves a static JPEG map per region to `Output/Maps/` showing Table 2 (blue)
  and Table 3 (orange) areas clipped to the region boundary.
  <img width="500" height="500" alt="SACOG_Combined_Area" src="https://github.com/user-attachments/assets/da3d0679-407c-43b7-9f76-c04d08eddcce" />


### `Transit_Eligibility_Calculations.R`

Builds the **transit-eligible area** input layer (`eligible_sf.geojson`) that
`VMT_Rates.R` consumes. Starting from a California transit stops GeoJSON,
the script:

- Explodes each stop's pipe-delimited `route_ids_served` string into one row
  per (stop, route) pair.
- Buffers every stop by a set distance and dissolves the buffers by route to produce a
  service area polygon for each route.
- Intersects every pair of route service areas and unions the results to
  identify locations reachable from **two or more distinct routes** within X
  miles — the operational definition of "transit eligible" used downstream.
- Writes the dissolved eligibility polygon to `eligible_sf.geojson`

### `Area_Median_Income.R`

Estimates **regional median household income** and the associated low-income
threshold (80% of regional median) for each region in the crosswalk. ACS
median income is published at the county level, so for multi-county regions
the script aggregates the underlying B19001 household income brackets and
interpolates a combined median:

- `interpolate_median()` performs linear interpolation within the bracket
  that contains the median household.
- `estimate_combined_median_hhi()` resolves county names to FIPS codes,
  pulls B19001 bracket counts via `tidycensus`, sums bracket counts across
  the counties in a region, and applies `interpolate_median()`.
<img width="439.5" height="338" alt="MHI_Comparison" src="https://github.com/user-attachments/assets/a5ff258f-fce8-491e-9808-2e7aaa1170c0" />
