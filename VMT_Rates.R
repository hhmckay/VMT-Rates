### VMT Rates v2

# ---- Packages -----------------------------------------------------------

library(dplyr)
library(tidycensus)
library(tigris)
library(sf)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(mapview)

setwd(".../VMT_Rates")

# ---- Constants ----------------------------------------------------------

CRS_CA <- 3310
HALF_MILE_M <- 804.672
VMT_CSV_PATH <- "Data/Replica/%s/replica-ca_vmt-05_13_26-vmt_layer.csv"
ALL_INCOME_DIR <- "All_Income"
STATE_FIPS_CHAR <- "6"
MAP_OUTPUT_DIR <- "Output/Maps"

# Create directory for map outputs
dir.create(MAP_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Helper Functions ----------------------------------------------------

# Read a Replica VMT CSV and rename columns to (GEOID, COUNTYFP, vmt, pop)
read_vmt <- function(folder, vmt_col, pop_col) {
  read.csv(sprintf(VMT_CSV_PATH, folder)) %>%
    mutate(
      GEOID = as.character(customGeoId),
      STATE = substr(GEOID, 1, 1),
      COUNTYFP = substr(GEOID, 2, 4)
    ) %>%
    filter(STATE == STATE_FIPS_CHAR) %>%
    select(GEOID, COUNTYFP,
           !!vmt_col := residentialVMT,
           !!pop_col := peopleByHome)
}

# Dissolve an sf object into a single multipolygon and make valid.
dissolve_sf <- function(x) {
  x %>%
    summarise(geometry = st_union(geometry)) %>%
    st_make_valid() %>%
    st_as_sf()
}

# Compute fractional tract coverage by an overlay polygon (proportional allocation)
# Returns data.frame(GEOID, <ratio_col>) where ratio = overlap_area / tract_area.
tract_coverage_ratio <- function(tracts_sf, overlay_sf, ratio_col) {
  st_intersection(tracts_sf, overlay_sf) %>%
    mutate(area2 = st_area(.),
           !!ratio_col := as.numeric(area2 / area1)) %>%
    st_drop_geometry() %>%
    select(GEOID, !!ratio_col)
}

# Calculate VMT rate
vmt_rate <- function(vmt, pop) {
  sum(vmt, na.rm = TRUE) / sum(pop, na.rm = TRUE)
}

# Apportion tract-level VMT/pop into an overlay region by area ratio
# Return the population-weighted VMT rate for that overlay
overlay_rate <- function(overlay_sf, region_vmt_sf, vmt_col, pop_col) {
  inter <- st_intersection(overlay_sf, region_vmt_sf) %>%
    mutate(area2 = st_area(.),
           ratio = as.numeric(area2 / area1))
  vmt_rate(inter[[vmt_col]] * inter$ratio,
           inter[[pop_col]] * inter$ratio)
}

# Save a static JPEG map showing Table 2 and Table 3 areas
save_region_map <- function(region_name, region_boundary,
                            table2_sf, table3_sf,
                            out_dir = MAP_OUTPUT_DIR) {
  
  # Clip overlays to the region so the map shows only this region's areas
  t2_clip <- suppressWarnings(st_intersection(table2_sf, region_boundary))
  t3_clip <- suppressWarnings(st_intersection(table3_sf, region_boundary))
  
  p <- ggplot() +
    geom_sf(data = region_boundary, fill = "grey95",
            color = "grey40", linewidth = 0.3) +
    geom_sf(data = t2_clip, aes(fill = "Table 2"),
            color = NA, alpha = 0.85) +
    geom_sf(data = t3_clip, aes(fill = "Table 3"),
            color = NA, alpha = 0.85) +
    scale_fill_manual(name   = NULL,
                      values = c("Table 2" = "#1f77b4",
                                 "Table 3" = "#ff7f0e")) +
    labs(title = region_name) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")
  
  safe_name <- gsub("[^A-Za-z0-9]+", "_", region_name)
  out_path <- file.path(out_dir, paste0(safe_name, ".jpg"))
  
  ggsave(out_path, plot = p, width = 7, height = 7,
         dpi = 150, device = "jpeg")
  invisible(out_path)
}

# ---- Static spatial inputs (computed once) ------------------------------

# Tracts (with per-tract area for downstream ratios)
tracts <- tracts(state = 06, year = 2020) %>%
  mutate(GEOID = as.character(as.numeric(GEOID))) %>%
  select(GEOID) %>%
  st_transform(CRS_CA) %>%
  mutate(area1 = st_area(.))

# County / region crosswalk
crosswalk <- read.csv("Data/Geography/county_crosswalk.csv") %>%
  select(County, Region) %>%
  rename(NAME = County)

counties <- counties(state = 06, year = 2020) %>%
  st_drop_geometry() %>%
  select(COUNTYFP, NAME) %>%
  left_join(crosswalk, by = "NAME")

regions <- unique(crosswalk$Region)

# Transit layers
hqtas <- read_sf("Data/Transit/hqta_dissolved.geojson") %>%
  st_transform(CRS_CA)

transit_eligible <- read_sf("Data/Transit/eligible_sf.geojson") %>%
  st_transform(CRS_CA)

# Table 2 overlay: HQTA + transit-eligible (un-buffered)
merged_transit_table2 <- bind_rows(hqtas, transit_eligible) %>%
  dissolve_sf()

# Table 3 overlay: HQTA + transit-eligible, each buffered by 1/2 mile
# Table 2 overlay removed to keep the two tables non-overlapping
hqtas_buff <- hqtas %>% 
  st_buffer(HALF_MILE_M) %>% 
  dissolve_sf()

transit_eligible_onemile <- read_sf("Data/Transit/eligible_sf_onemile.geojson") %>%
  st_transform(CRS_CA)

merged_transit_table3 <- bind_rows(hqtas_buff, transit_eligible_onemile) %>%
  dissolve_sf()

transit_table3_non_overlap <- st_difference(merged_transit_table3,
                                            merged_transit_table2)

# Tract coverage ratios
hqta_cov <- tract_coverage_ratio(tracts, hqtas, "hqta_cov_ratio")

transit_eligible_cov <- tract_coverage_ratio(tracts, transit_eligible, "transit_eligible_cov_ratio")

transit_table3_cov <- tract_coverage_ratio(tracts, transit_table3_non_overlap, "transit_cov_ratio")

# ---- Statewide VMT ------------------------------------------------------

vmt_total_state <- read_vmt(ALL_INCOME_DIR, "vmt_total", "pop_total")

# ---- Per-region computation ---------------------------------------------

compute_region <- function(region_name) {
  
  # Tracts in the target region
  tracts_clean <- tracts %>%
    mutate(COUNTYFP = substr(GEOID, 2, 4)) %>%
    left_join(counties, by = "COUNTYFP") %>%
    filter(Region == region_name) %>%
    select(GEOID)
  
  region_geoids <- tracts_clean$GEOID
  
  # Region-filtered VMT (all-income and low-income)
  vmt_total <- vmt_total_state %>%
    filter(GEOID %in% region_geoids) %>%
    select(GEOID, vmt_total, pop_total)
  
  vmt_low_income <- read_vmt(region_name, "vmt_li", "pop_li") %>%
    filter(GEOID %in% region_geoids) %>%
    select(GEOID, vmt_li, pop_li)
  
  # Regional per-capita VMT
  regional_rate <- vmt_rate(vmt_total$vmt_total, vmt_total$pop_total)
  regional_rate_li <- vmt_rate(vmt_low_income$vmt_li, vmt_low_income$pop_li)
  fifteen_pct_below <- regional_rate * (1 - 0.15)
  
  # Criteria 1 tract classification (shared by both tables)
  criteria1_table <- vmt_total %>%
    mutate(vmt_per_capita = vmt_total / pop_total,
           below_fifteen_pct = vmt_per_capita <= fifteen_pct_below,
           below_regional_avg = vmt_per_capita > fifteen_pct_below &
             vmt_per_capita < regional_rate) %>%
    select(GEOID, below_fifteen_pct, below_regional_avg)
  
  # Tract-level df with VMT, pop, vmt classification, and transit area coverage ratios
  vmt_df <- tracts_clean %>%
    st_drop_geometry() %>%
    left_join(vmt_total, by = "GEOID") %>%
    left_join(vmt_low_income, by = "GEOID") %>%
    left_join(criteria1_table, by = "GEOID") %>%
    left_join(hqta_cov, by = "GEOID") %>%
    left_join(transit_eligible_cov, by = "GEOID") %>%
    left_join(transit_table3_cov, by = "GEOID") %>%
    mutate(across(c(hqta_cov_ratio,
                    transit_eligible_cov_ratio,
                    transit_cov_ratio),
                  ~ tidyr::replace_na(.x, 0)))
  
  # Per-tract apportioned values
  vmt_df <- vmt_df %>%
    mutate(
      # HQTA-apportioned (Table 2 Criteria 2)
      vmt_total_hqta = hqta_cov_ratio * vmt_total,
      pop_total_hqta = hqta_cov_ratio * pop_total,
      vmt_li_hqta = hqta_cov_ratio * vmt_li,
      pop_li_hqta = hqta_cov_ratio * pop_li,
      
      # Transit-eligible-apportioned (Table 2 Criteria 3)
      vmt_total_te = transit_eligible_cov_ratio * vmt_total,
      pop_total_te = transit_eligible_cov_ratio * pop_total,
      vmt_li_te = transit_eligible_cov_ratio * vmt_li,
      pop_li_te = transit_eligible_cov_ratio * pop_li,
      
      # Buffered, Table-2-removed transit (Table 3 Criteria 2)
      vmt_total_t3 = transit_cov_ratio * vmt_total,
      pop_total_t3 = transit_cov_ratio * pop_total,
      vmt_li_t3 = transit_cov_ratio * vmt_li,
      pop_li_t3 = transit_cov_ratio * pop_li
    )
  
  # ---- Table 2 criteria ------------------------------------------------
  
  # Criteria 1: tracts >=15% below regional average
  t2_c1 <- vmt_df %>% 
    filter(below_fifteen_pct)
  
  t2_c1_all <- vmt_rate(t2_c1$vmt_total, t2_c1$pop_total)
  t2_c1_li <- vmt_rate(t2_c1$vmt_li, t2_c1$pop_li)
  
  # Criteria 2: HQTA-apportioned
  t2_c2_all <- vmt_rate(vmt_df$vmt_total_hqta, vmt_df$pop_total_hqta)
  t2_c2_li <- vmt_rate(vmt_df$vmt_li_hqta, vmt_df$pop_li_hqta)
  
  # Criteria 3: transit-eligible-apportioned
  t2_c3_all <- vmt_rate(vmt_df$vmt_total_te, vmt_df$pop_total_te)
  t2_c3_li <- vmt_rate(vmt_df$vmt_li_te, vmt_df$pop_li_te)
  
  # ---- Table 3 criteria ------------------------------------------------
  
  # Criteria 1: tracts below regional avg but above 15%-below threshold
  t3_c1 <- vmt_df %>% 
    filter(below_regional_avg)
  
  t3_c1_all <- vmt_rate(t3_c1$vmt_total, t3_c1$pop_total)
  t3_c1_li <- vmt_rate(t3_c1$vmt_li, t3_c1$pop_li)
  
  # Criteria 2: buffered transit, Table-2 area removed
  t3_c2_all <- vmt_rate(vmt_df$vmt_total_t3, vmt_df$pop_total_t3)
  t3_c2_li <- vmt_rate(vmt_df$vmt_li_t3, vmt_df$pop_li_t3)
  
  # Criteria 3: urbanized-area screen — placeholder
  t3_c3_all <- NA_real_
  t3_c3_li  <- NA_real_
  
  # ---- Combined non-overlapping table geometries -----------------------
  
  # Region VMT sf for area-weighted apportionment to combined geometries
  region_vmt_sf <- tracts_clean %>%
    left_join(vmt_df, by = "GEOID") %>%
    select(GEOID, vmt_total, pop_total, vmt_li, pop_li) %>%
    mutate(area1 = st_area(.))
  
  # Table 2 combined: Criteria-1 tracts UNION transit overlay
  criteria1_tracts_t2 <- tracts %>%
    inner_join(filter(criteria1_table, below_fifteen_pct), by = "GEOID") %>%
    select(GEOID) %>%
    summarise(geometry = st_union(geometry))
  
  table2_geos <- bind_rows(criteria1_tracts_t2, merged_transit_table2) %>%
    dissolve_sf()
  
  t2_all <- overlay_rate(table2_geos, region_vmt_sf, "vmt_total", "pop_total")
  t2_li <- overlay_rate(table2_geos, region_vmt_sf, "vmt_li", "pop_li")
  
  # Table 3 combined: (Criteria-1 below-regional tracts UNION buffered transit),
  # with Table-2 combined geometry removed for non-overlap.
  criteria1_tracts_t3 <- tracts %>%
    inner_join(filter(criteria1_table, below_regional_avg), by = "GEOID") %>%
    select(GEOID) %>%
    summarise(geometry = st_union(geometry))
  
  table3_geos <- bind_rows(criteria1_tracts_t3, transit_table3_non_overlap) %>%
    dissolve_sf()
  
  table3_non_overlap <- st_difference(table3_geos, table2_geos)
  
  t3_all <- overlay_rate(table3_non_overlap, region_vmt_sf,
                         "vmt_total", "pop_total")
  t3_li <- overlay_rate(table3_non_overlap, region_vmt_sf,
                         "vmt_li", "pop_li")
  
  # ---- Map -------------------------------------------------------------
  
  region_boundary <- tracts_clean %>%
    summarise(geometry = st_union(geometry)) %>%
    st_make_valid()
  
  save_region_map(region_name, region_boundary,
                  table2_sf = table2_geos,
                  table3_sf = table3_non_overlap)
  
  message("Finished: ", region_name)
  
  list(
    table2 = data.frame(
      region = region_name,
      regional_rate = regional_rate,
      regional_rate_li = regional_rate_li,
      vmt_criteria1_all_income = t2_c1_all,
      vmt_criteria2_all_income = t2_c2_all,
      vmt_criteria3_all_income = t2_c3_all,
      vmt_criteria1_low_income = t2_c1_li,
      vmt_criteria2_low_income = t2_c2_li,
      vmt_criteria3_low_income = t2_c3_li,
      vmt_table2_all_income = t2_all,
      vmt_table2_low_income = t2_li
    ),
    table3 = data.frame(
      region = region_name,
      regional_rate = regional_rate,
      regional_rate_li = regional_rate_li,
      vmt_criteria1_all_income = t3_c1_all,
      vmt_criteria2_all_income = t3_c2_all,
      vmt_criteria3_all_income = t3_c3_all,
      vmt_criteria1_low_income = t3_c1_li,
      vmt_criteria2_low_income = t3_c2_li,
      vmt_criteria3_low_income = t3_c3_li,
      vmt_table3_all_income = t3_all,
      vmt_table3_low_income = t3_li
    )
  )
}

# ---- Run ---------------------------------------------------------------

results <- map(regions, compute_region)

table_2_out <- map_dfr(results, "table2")
table_3_out <- map_dfr(results, "table3")

write.csv(table_2_out, "table_2_out.csv")
write.csv(table_3_out, "table_3_out.csv")
