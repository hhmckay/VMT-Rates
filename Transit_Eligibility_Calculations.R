### Transit Eligibility Calculations

# ---- Packages -----------------------------------------------------------

library(sf)
library(dplyr)
library(tidyr)

# --- 2. Configuration --------------------------------------------------------

# Path to transit stops dataset (https://gis.data.ca.gov/datasets/900992cc94ab49dbbb906d8f147c2a72_0)
STOPS_GEOJSON <- "/Users/S152973/Downloads/California_Transit_Stops.geojson"

# Buffer distance
BUFFER_MILES <- 0.5
BUFFER_METERS <- BUFFER_MILES * 1609.344

# --- 3. Load stops from local GeoJSON ----------------------------------------

stops_sf <- st_read(STOPS_GEOJSON, quiet = TRUE)

# --- 4. Project to CA Albers (metric, equal-area) ----------------------------

stops_proj <- st_transform(stops_sf, crs = 3310)

# --- 5. Expand stops to one row per route ------------------------------------

# route_ids_served is a delimited string like "routeA|routeB|routeC"
# We split it so each (stop, route) pair becomes its own row
# Stops with no route information are dropped — they cannot contribute to
# route-count eligibility.

# Inspect a sample to confirm the delimiter used in route_ids_served
sample_routes <- stops_proj$route_ids_served[!is.na(stops_proj$route_ids_served)][1:5]
message("  Sample route_ids_served values: ", paste(sample_routes, collapse = " | "))

# Adjust DELIM if your data uses a different separator (e.g. "," or ";")
DELIM <- "\\|"

stop_routes <- stops_proj %>%
  st_drop_geometry() %>%
  select(OBJECTID, route_ids_served) %>%
  mutate(route_ids_served = as.character(route_ids_served)) %>%
  filter(!is.na(route_ids_served), route_ids_served != "") %>%
  mutate(route_id = strsplit(route_ids_served, DELIM)) %>%
  unnest(route_id) %>%
  mutate(route_id = trimws(route_id)) %>%
  filter(route_id != "") %>%
  select(OBJECTID, route_id)

message(sprintf("  Stop-route pairs: %d (across %d unique routes)",
                nrow(stop_routes),
                n_distinct(stop_routes$route_id)))

# --- 6. Buffer all stops -----------------------------------------------------

stop_buffers <- stops_proj %>%
  select(OBJECTID) %>%
  st_buffer(dist = BUFFER_METERS)

# Join route assignments back to buffers.
# Result: one row per (stop, route), geometry = that stop's buffer polygon.
route_buffers <- stop_routes %>%
  left_join(stop_buffers, by = "OBJECTID") %>%
  st_as_sf()

# --- 7. Dissolve buffers by route -------------------------------------------

# For each unique route, merge all its stop-buffers into one polygon.
# This is the spatial "service area" reachable from that route within the specified distance.

route_areas <- route_buffers %>%
  group_by(route_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

n_routes_total <- nrow(route_areas)

# --- 8. Find areas covered by >= 2 distinct routes --------------------------

# Intersect every pair of route service areas.
# The union of all pairwise intersections = everywhere reachable from >= 2 routes.

message(sprintf("  Processing %d routes — %d pairs to evaluate ...",
                n_routes_total,
                choose(n_routes_total, 2)))

intersection_list <- list()
idx <- 1L

for (i in seq_len(n_routes_total - 1L)) {
  for (j in seq(i + 1L, n_routes_total)) {

    inter <- tryCatch(
      st_intersection(route_areas$geometry[[i]], route_areas$geometry[[j]]),
      error = function(e) NULL
    )

    # Keep only polygon/multipolygon results (discard point/line artifacts)
    if (!is.null(inter) && !st_is_empty(inter)) {
      inter_poly <- tryCatch(
        st_collection_extract(inter, "POLYGON"),
        error = function(e) NULL
      )
      if (!is.null(inter_poly) && !st_is_empty(inter_poly)) {
        intersection_list[[idx]] <- inter_poly
        idx <- idx + 1L
      }
    }
  }

  if (i %% 50 == 0) {
    message(sprintf("Completed intersections for route %d of %d ...",
                    i, n_routes_total - 1L))
  }
}

message(sprintf("  Non-empty pairwise intersections found: %d",
                length(intersection_list)))

# --- 9. Union all intersections into the final eligibility layer -------------

if (length(intersection_list) == 0) {
  stop("No eligible areas found. Check delimiter, route data, or buffer distance.")
}

eligible_union <- st_union(do.call(c, intersection_list))
eligible_union <- st_make_valid(eligible_union)

# Explicitly wrap as sfc with CRS before passing to st_sf
eligible_geom <- st_sfc(eligible_union, crs = 3310)

eligible_sf <- st_sf(
  description = paste0(
    "Areas within ", BUFFER_MILES, " mile(s) of stops from ",
    ">= 2 distinct transit routes"
  ),
  area_sq_mi = as.numeric(st_area(eligible_geom)) / (1609.344^2),
  geometry = eligible_geom
)

message(sprintf("  Total eligible area: %.1f sq miles", eligible_sf$area_sq_mi))

# --- 10. Write outputs -------------------------------------------------------
st_write(eligible_sf, "eligible_sf.geojson")
