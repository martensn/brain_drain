paths <- c(
  "intermediate/both_demo.rds","intermediate/both_final.rds","intermediate/both_orig.rds",
  "intermediate/cc.rds","intermediate/cc_alias.rds","intermediate/cc_rsid_crosswalk.rds",
  "intermediate/cc_strings.rds","intermediate/col_alias.rds","intermediate/col_alias_resp.rds",
  "intermediate/col_ct.rds","intermediate/col_embed.rds","intermediate/col_rsid_crosswalk.rds",
  "intermediate/col_strings.rds","intermediate/colleges.rds","intermediate/ed_wide.rds",
  "intermediate/hs_alias.rds","intermediate/hs_alias_resp.rds","intermediate/hs_embed.rds",
  "intermediate/hs_rsid_crosswalk.rds","intermediate/hs_strings.rds","intermediate/input_col.rds",
  "intermediate/input_col_m.rds","intermediate/ipeds_deg_awarded.rds","intermediate/matched_col.rds",
  "intermediate/matched_hs.rds","intermediate/raw_unmatched_strings.rds",
  "intermediate/rsid_id_dup_crosswalk.rds","intermediate/schools.rds","intermediate/state_fips.rds",
  "intermediate/unmatched_cc.rds","intermediate/unmatched_col.rds","intermediate/unmatched_hs.rds",
  "results/summary_demographics.rds"
)

root <- "P:/BRAIN_DRAIN/Data"
ok <- 0
bad <- 0
for (p in paths) {
  full <- file.path(root, p)
  result <- tryCatch({
    obj <- readRDS(full)
    dims <- if (!is.null(dim(obj))) paste(dim(obj), collapse = "x") else paste0("length ", length(obj))
    cat(sprintf("OK\t%s\t%s\t%s\n", p, class(obj)[1], dims))
    ok <- ok + 1
  }, error = function(e) {
    cat(sprintf("BAD\t%s\tERROR\t%s\n", p, conditionMessage(e)))
    bad <<- bad + 1
  })
}
cat(sprintf("\nTOTAL: %d ok, %d bad\n", ok, bad))
