# metro_tier_map.R
#
# [NEW 2026-08-21] A US county-level choropleth of MEMO1_WEIGHTING.md
# SS5.1's 3-tier metro classification (Top 10 / Top 11-50 / Everything
# else -- the "rank3" scheme in Code/memo1_00_metro_tier_definitions.R),
# per Nicholas's request. Not part of Memo 1's own write-up.
#
# Reuses Code/scripts/09_plots.R's established map-building pattern
# (county-level base geometry, joined to Data/raw/census_geo/
# unified_cbsa.csv's county->CBSA crosswalk, filled by a categorical
# variable, theme_void(), transparent background, centered title) --
# that script's own geometry source, `data(county_laea)` (from the
# `urbnmapr` package), isn't installed in this project's renv library, so
# `tigris::counties()` is used instead (already installed, live TIGER/Line
# cartographic-boundary pull, cb=TRUE/20m resolution keeps the file light).
# County-level fill is used directly rather than dissolving to CBSA
# polygons first (09_plots.R's st_union step) -- visually identical, since
# every county in the same CBSA gets the same tier color anyway, without
# the extra dissolve cost or geometry-validity risk.
#
# CONUS only (continental 48 + DC) -- Alaska/Hawaii/territories dropped
# for a clean single-panel map, a standard simplification for a quick
# descriptive plot (09_plots.R's own map chunks don't handle AK/HI insets
# either). Projected to EPSG:5070 (NAD83 Conus Albers), the standard
# equal-area projection for exactly this kind of CONUS choropleth.
#
# Tier assignment reuses Code/memo1_00_metro_tier_definitions.R's
# build_cbsa_tier_lookup("rank3") directly (same population ranking used
# everywhere else in this project, not re-derived) -- counties that don't
# join to any CBSA at all (true non-metro) default to "Everything else",
# matching that scheme's own non_metro_label() definition.

library(data.table)
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(dotenv)
library(here)
load_dot_env(here::here(".env"))
directory <- Sys.getenv("BRAIN_DRAIN_ROOT")
data_dir  <- file.path(directory, "Data")
options(tigris_use_cache = TRUE)
source(here::here("Code/memo1_00_metro_tier_definitions.R"))

FONT <- "Segoe UI"
NON_CONUS_FIPS <- c("02", "15", "60", "66", "69", "72", "78")  # AK, HI, AS, GU, MP, PR, VI

log_step <- function(msg) { cat(format(Sys.time(), "%H:%M:%S"), "-", msg, "\n"); flush(stdout()) }

log_step("Pulling county cartographic boundaries (tigris, cb=TRUE)")
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE)
counties_sf <- counties_sf[!counties_sf$STATEFP %in% NON_CONUS_FIPS, ]

log_step("Pulling state boundaries for the reference overlay")
states_sf <- states(cb = TRUE, resolution = "20m", year = 2022, progress_bar = FALSE)
states_sf <- states_sf[!states_sf$STATEFP %in% NON_CONUS_FIPS, ]

## ---- Census region outlines (Northeast/Midwest/South/West) -- same
## STATE_REGION_CROSSWALK sourced from memo1_00_metro_tier_definitions.R
## that every region-crossed scheme in this project already uses, not a
## new/different region definition. States dissolved into one polygon per
## region via st_union so the boundary drawn is the REGION outline, not
## every state line again. ----
log_step("Building Census region outlines")
region_for_abbr <- setNames(STATE_REGION_CROSSWALK$census_region, STATE_REGION_CROSSWALK$state_abbr)
states_sf$census_region <- unname(region_for_abbr[states_sf$STUSPS])
stopifnot(all(!is.na(states_sf$census_region)))  # every CONUS state should resolve -- catches a silent join miss immediately
regions_sf <- states_sf %>% dplyr::group_by(census_region) %>% dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop")

## ---- tier assignment: same population-rank lookup as everywhere else in
## this project, not re-derived ----
log_step("Building rank3 CBSA-tier lookup")
lookup <- build_cbsa_tier_lookup("rank3")
tier_lookup <- lookup$cbsa_pop[, .(cbsa_code, metro_tier)]

unified_cbsa <- fread(file.path(data_dir, "raw/census_geo/unified_cbsa.csv"), colClasses = c(GeoFIPS = "character", cbsa_code = "character"))

county_tier <- merge(unified_cbsa, tier_lookup, by = "cbsa_code", all.x = TRUE)
county_tier[, metro_tier := fifelse(is.na(metro_tier), "Everything else", metro_tier)]
county_tier <- unique(county_tier[, .(GeoFIPS, metro_tier)])  # a county can span multiple CBSA rows in rare cases -- keep first/only tier per county

counties_sf <- merge(counties_sf, county_tier, by.x = "GEOID", by.y = "GeoFIPS", all.x = TRUE)
counties_sf$metro_tier[is.na(counties_sf$metro_tier)] <- "Everything else"  # unmatched counties (no CBSA at all) are non-metro by definition
counties_sf$metro_tier <- factor(counties_sf$metro_tier, levels = c("Top 10", "Top 11-50", "Everything else"))

cat("\nCounty tier counts:\n")
print(table(counties_sf$metro_tier))

## ---- project to a standard CONUS equal-area projection ----
counties_sf <- st_transform(counties_sf, 5070)
states_sf <- st_transform(states_sf, 5070)
regions_sf <- st_transform(regions_sf, 5070)
region_labels <- st_centroid(regions_sf)
region_coords <- st_coordinates(region_labels)
region_labels$x <- region_coords[, 1]
region_labels$y <- region_coords[, 2]
cat("\nRegion label coordinates (EPSG:5070, for nudge-tuning if needed):\n")
print(st_drop_geometry(region_labels)[, c("census_region", "x", "y")])

## ---- tasteful sequential palette: tier is an ORDERED size classification
## (Top 10 = most concentrated, down to Everything else = the diffuse
## background category), so one hue light->dark reads more honestly than
## three arbitrary discrete colors -- a deep indigo, stepping down in
## saturation/lightness, with "Everything else" a quiet neutral so the two
## real size tiers are what draw the eye. ----
tier_colors <- c("Top 10" = "#2a1a5e", "Top 11-50" = "#8b7ab8", "Everything else" = "#e8e6ee")

p <- ggplot(counties_sf) +
  geom_sf(aes(fill = metro_tier), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.25) +
  geom_sf(data = regions_sf, fill = NA, color = "#d1476b", linewidth = 0.9) +
  geom_text(data = region_labels, aes(x = x, y = y, label = census_region),
            inherit.aes = FALSE, color = "#d1476b", family = FONT, fontface = "bold", size = 4.2) +
  scale_fill_manual(values = tier_colors, name = NULL) +
  labs(title = "Metro tiers x Census region: Top 10 vs. Top 11–50 vs. everything else") +
  theme_void() +
  theme(text = element_text(size = 10, family = FONT, color = "#1a1a1a"),
        plot.title = element_text(hjust = 0.5, size = 13, color = "#1a1a1a"),
        legend.text = element_text(color = "#1a1a1a"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")

out <- file.path(data_dir, "results/metro_tier_map.png")
ggsave(out, p, width = 9, height = 5.8, units = "in", dpi = 600, bg = "white")
cat(sprintf("Wrote %s\n", out))
