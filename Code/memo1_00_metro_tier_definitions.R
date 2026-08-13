# memo1_00_metro_tier_definitions.R
#
# [NEW 2026-08-12, deliberate exception to this project's usual "standalone
# script, no source()" convention] Centralizes the metro-tier classification
# scheme in ONE place, at Nicholas's explicit request. The tier cutoffs
# (Top 10/50/100 rank thresholds) were previously hardcoded independently
# in 6 different scripts (memo1_metro_tiers.R, memo1_puma_cbsa_crosswalk.R,
# memo1_irs_soi_desttier_margin.R, memo1_phase_a_year_calibration_test.R,
# memo1_phase_b_flow_calibration_test.R, memo1_calibration_lines.R), so
# testing an alternative classification meant hand-editing all six. This
# file is meant to be source()'d, not run standalone.
#
# A "scheme" is a named entry in METRO_TIER_SCHEMES: an `assign(cbsa_rank,
# region)` function returning a tier label (region is ignored for
# size-only schemes), a `uses_region` flag, and a `non_metro_label(region)`
# function for Census's synthetic state+"999" non-metro pseudo-CBSA-code
# (see unified_cbsa.csv / Code/00_crosswalks.Rmd) -- taking region too, so
# a region-crossed scheme's non-metro bucket still carries a region tag (a
# non-metro county in Alabama is still genuinely "South").
#
# CBSA_RANK_YEAR (2022) is NOT scheme-specific -- every scheme ranks CBSAs
# by the same fixed population snapshot, so "Top 10" always means the same
# ten metros regardless of which scheme groups them; only the GROUPING
# varies by scheme.
#
# Region resolution note: a CBSA can span multiple states (rare but real,
# e.g. the NYC metro spans NY/NJ/CT), so region can't be assigned at the
# CBSA level the way size-rank can -- it's resolved at the TRACT/PUMA level
# instead (where state is unambiguous), via state_fips_to_region() below.
# code_to_tier_for_scheme() requires a `region` argument for schemes with
# uses_region = TRUE precisely because of this.

library(data.table)

# Duplicated from Code/demographics.R's state_region_crosswalk (not
# source()'d directly -- that script has other, unrelated side effects on
# load) -- per this project's existing Decisions note in HANDOFF.md, this
# is the canonical Census-region labeling to use everywhere geography is
# compared, not base R's state.region.
STATE_REGION_CROSSWALK <- data.table(
  state_abbr = c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA",
    "IL", "IN", "MI", "OH", "WI",
    "IA", "KS", "MN", "MO", "NE", "ND", "SD",
    "DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
    "AL", "KY", "MS", "TN",
    "AR", "LA", "OK", "TX",
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
    "AK", "CA", "HI", "OR", "WA"
  ),
  census_region = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  )
)

METRO_TIER_SCHEMES <- list(

  rank5 = list(
    label = "5-tier (original): Top 10 / Top 11-50 / Top 51-100 / Other metro / Non-metro",
    uses_region = FALSE,
    assign = function(cbsa_rank, region = NULL) {
      fcase(
        cbsa_rank <= 10, "Top 10",
        cbsa_rank <= 50, "Top 11-50",
        cbsa_rank <= 100, "Top 51-100",
        default = "Other metro"
      )
    },
    non_metro_label = function(region = NULL) "Non-metro"
  ),

  # [NEW 2026-08-12, per Nicholas's request 2] Top 10 and Top 11-50 hold
  # roughly equal population shares (confirmed in the existing chart) and
  # stay separate; everything else -- Top 51-100, Other metro, AND
  # Non-metro alike -- collapses into one bucket.
  rank3 = list(
    label = "3-tier (consolidated): Top 10 / Top 11-50 / Everything else",
    uses_region = FALSE,
    assign = function(cbsa_rank, region = NULL) {
      fcase(
        cbsa_rank <= 10, "Top 10",
        cbsa_rank <= 50, "Top 11-50",
        default = "Everything else"
      )
    },
    non_metro_label = function(region = NULL) "Everything else"
  ),

  # [NEW 2026-08-12, per Nicholas's request 3] Same 3-tier size split,
  # crossed with Census region -- 12 cells total (3 x 4), meant to surface
  # heterogeneity a size-only scheme masks (e.g. "large cities in the
  # Northeast" vs. "large cities in the South" as genuinely different
  # destinations, not pooled into one "Top 10" bucket). Real sparsity risk
  # at this granularity for a JOINT origin x destination margin (12x12=144
  # cells) -- flagged, not yet built; this scheme is validated first via
  # the simpler univariate tier-share comparison (Phase A-style).
  rank3_region = list(
    label = "3-tier x Census region: (Top 10 / Top 11-50 / Everything else) x (Northeast/Midwest/South/West)",
    uses_region = TRUE,
    assign = function(cbsa_rank, region) {
      size <- fcase(
        cbsa_rank <= 10, "Top 10",
        cbsa_rank <= 50, "Top 11-50",
        default = "Everything else"
      )
      paste0(size, " (", region, ")")
    },
    non_metro_label = function(region) paste0("Everything else (", region, ")")
  )
)

#' Build a CBSA -> metro_tier lookup for a given scheme (size-only schemes
#' only -- region-crossed schemes resolve tier at the tract/PUMA level via
#' code_to_tier_for_scheme(), since a CBSA doesn't have one definite
#' region). Single live tidycensus::get_estimates() call, same
#' CBSA_RANK_YEAR every scheme shares.
build_cbsa_tier_lookup <- function(scheme_name, rank_year = 2022) {
  stopifnot(scheme_name %in% names(METRO_TIER_SCHEMES))
  scheme <- METRO_TIER_SCHEMES[[scheme_name]]
  cbsa_pop <- tidycensus::get_estimates(geography = "cbsa", product = "population", year = rank_year, vintage = rank_year)
  setDT(cbsa_pop)
  cbsa_pop <- cbsa_pop[variable == "POPESTIMATE", .(cbsa_code = as.character(GEOID), cbsa_pop = value)]
  setorder(cbsa_pop, -cbsa_pop)
  cbsa_pop[, cbsa_rank := seq_len(.N)]
  if (!scheme$uses_region) cbsa_pop[, metro_tier := scheme$assign(cbsa_rank)]
  list(cbsa_pop = cbsa_pop, scheme_name = scheme_name, scheme = scheme)
}

#' Returns a code_to_tier(codes, region = NULL) closure bound to `lookup`.
#' `region` is REQUIRED (a same-length character vector) when the scheme
#' has uses_region = TRUE, ignored otherwise.
code_to_tier_for_scheme <- function(lookup) {
  scheme <- lookup$scheme
  cbsa_pop <- lookup$cbsa_pop
  function(codes, region = NULL) {
    rank <- cbsa_pop$cbsa_rank[match(codes, cbsa_pop$cbsa_code)]
    is_non_metro <- is.na(rank) & grepl("999$", codes)
    if (scheme$uses_region) {
      stopifnot(!is.null(region), length(region) == length(codes))
      # [FIXED 2026-08-12, caught via a suspicious cell-coverage number,
      # not assumed clean] region can itself be NA (e.g. cbsa_state_t is
      # occasionally an empty string "" rather than NA -- a real, if
      # small, ~3-4%-of-rows data gap -- which doesn't match any state
      # abbreviation and resolves to NA region). Without this guard,
      # scheme$assign()'s paste0(size, " (", region, ")") would silently
      # produce a garbage "Top 10 (NA)"-style label instead of a real NA
      # -- a fake category that inflates cell counts and can never have
      # real ACS-side coverage, rather than being correctly excluded.
      tier <- rep(NA_character_, length(codes))
      has_rank <- !is.na(rank) & !is.na(region)
      tier[has_rank] <- scheme$assign(rank[has_rank], region[has_rank])
      is_non_metro <- is_non_metro & !is.na(region)
      tier[is_non_metro] <- scheme$non_metro_label(region[is_non_metro])
      tier
    } else {
      tier <- cbsa_pop$metro_tier[match(codes, cbsa_pop$cbsa_code)]
      tier[is_non_metro] <- scheme$non_metro_label()
      tier
    }
  }
}

#' State FIPS -> Census region, for resolving `region` at the tract/PUMA
#' level (where state is unambiguous, unlike CBSA).
state_fips_to_region <- function() {
  data(fips_codes, package = "tidycensus")
  setDT(fips_codes)
  fc <- unique(fips_codes[, .(state_fips = state_code, state_abbr = state)])
  merge(fc, STATE_REGION_CROSSWALK, by = "state_abbr", all.x = TRUE)
}
