# memo1_ipf.R
#
# [NEW 2026-08-23] Shared raking mechanism, extracted from memo1_04's
# original (and best-documented) copy -- previously an identical function
# body copy-pasted independently into memo1_05_reweight_column2.R,
# memo1_10_full_sample_extras.R, and memo1_07_reweight_column2_occupation.R,
# each with its own "copied verbatim, same reason as those files" header
# note. Centralized here, deliberately source()'d (the same one-time
# exception this project already makes for memo1_00_metro_tier_definitions.R,
# for the same reason: every script that rakes anything -- Stage 1
# demographic IPF, Stage 2 geography/occupation flow calibration, and any
# future margin -- needs the identical, already-debugged mechanism, not a
# fifth near-copy that can silently drift from the others).
#
# Usage: add one more list(keys=..., pop=...) entry to `margins` to rake
# against an additional margin (this is the actual mechanism a future
# Stage 2 addition -- e.g. industry -- would extend; see
# memo1_07_reweight_column2_occupation.R for a worked 2-margin example).
# `pop` must be a data.table/data.frame with a `Freq` column and the same
# key columns as `dt`.
#
# Full derivation/debugging history (why this is a hand-rolled loop and not
# survey::rake()/calibrate(), why the ratio is clamped every iteration not
# just at the end, why an explicit .rowid tracks row order through every
# merge) lives in memo1_05_reweight_column2.R's header comments -- not
# repeated here, since this file is the mechanism itself, not the record of
# how it was arrived at.
manual_ipf <- function(dt, w_col, margins, maxit = 10, epsilon = 1, cap_lo = 0.05, cap_hi = 20, verbose = FALSE) {
  dt <- copy(dt)
  dt[, .rowid := .I]
  dt[, w_iter := get(w_col)]
  old_w <- dt$w_iter
  iter <- 0; converged <- FALSE
  while (iter < maxit) {
    for (m in margins) {
      keys <- m$keys; pop <- m$pop
      cell_sum <- dt[, .(sample_sum = sum(w_iter)), by = keys]
      r <- merge(cell_sum, pop, by = keys, all.x = TRUE)
      # NA Freq here means a population cell genuinely has no sample
      # coverage -- ratio 1 (no adjustment) rather than propagating NA.
      r[, ratio := fifelse(!is.na(Freq) & sample_sum > 0, Freq / sample_sum, 1)]
      r[, ratio := pmin(pmax(ratio, cap_lo), cap_hi)]
      dt <- merge(dt, r[, c(keys, "ratio"), with = FALSE], by = keys, all.x = TRUE)
      dt[, ratio := fifelse(is.na(ratio), 1, ratio)]
      dt[, w_iter := w_iter * ratio]
      dt[, ratio := NULL]
    }
    # dt's row order drifts within the margin loop (each merge re-sorts by
    # its own join keys) -- restore canonical .rowid order BEFORE comparing
    # against old_w.
    setorder(dt, .rowid)
    delta <- max(abs(dt$w_iter - old_w))
    if (verbose) cat(sprintf("  [manual_ipf] iter=%d delta=%.4f any_nonfinite=%s\n", iter, delta, any(!is.finite(dt$w_iter))))
    if (is.finite(delta) && delta < epsilon) { converged <- TRUE; break }
    old_w <- dt$w_iter
    iter <- iter + 1
  }
  if (!converged) warning(sprintf("manual_ipf did not converge after %d iterations (delta=%.4f, epsilon=%d)", iter, delta, epsilon))
  cat(sprintf("manual_ipf: %s after %d iteration(s) (delta=%.4f), any non-finite: %s\n",
              if (converged) "converged" else "DID NOT CONVERGE", iter, delta, any(!is.finite(dt$w_iter))))
  setorder(dt, .rowid)
  dt$w_iter
}
