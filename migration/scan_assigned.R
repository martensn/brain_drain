files <- readLines("D:/Users/martensn/BRAIN_DRAIN/migration/scope_files.txt")
files <- files[nchar(trimws(files)) > 0]

tangle_rmd <- function(rmd_path, out_path) {
  lines <- readLines(rmd_path, warn = FALSE)
  in_chunk <- FALSE
  code <- character()
  for (ln in lines) {
    if (!in_chunk && grepl("^```\\{r", ln)) { in_chunk <- TRUE; next }
    if (in_chunk && grepl("^```\\s*$", ln)) { in_chunk <- FALSE; next }
    if (in_chunk) code <- c(code, ln)
  }
  writeLines(code, out_path)
}

# Recursively collect assignment-target symbols and for-loop variables from a parsed expression
collect_assigned <- function(e, acc) {
  if (is.call(e)) {
    fn <- e[[1]]
    if (is.symbol(fn) && as.character(fn) %in% c("<-", "=", "<<-") && length(e) >= 3) {
      target <- e[[2]]
      if (is.symbol(target)) acc$names <- c(acc$names, as.character(target))
      if (is.call(target) && is.symbol(target[[1]]) && length(target) >= 2) {
        # e.g. names(x) <- ... : the base object x is still the meaningfully assigned one
        inner <- target[[2]]
        if (is.symbol(inner)) acc$names <- c(acc$names, as.character(inner))
      }
    }
    if (is.symbol(fn) && as.character(fn) == "for" && length(e) >= 2) {
      loopvar <- e[[2]]
      if (is.symbol(loopvar)) acc$names <- c(acc$names, as.character(loopvar))
    }
    if (is.symbol(fn) && as.character(fn) == "function") {
      params <- names(e[[2]])
      if (!is.null(params)) acc$names <- c(acc$names, params)
    }
    if (is.symbol(fn) && as.character(fn) == "assign" && length(e) >= 2) {
      nm <- e[[2]]
      if (is.character(nm)) acc$names <- c(acc$names, nm)
    }
    for (i in seq_along(e)) {
      sub <- e[[i]]
      if (!missing(sub) && (is.call(sub) || is.pairlist(sub))) acc <- collect_assigned(sub, acc)
    }
  }
  acc
}

out <- data.frame(file = character(), variable = character(), stringsAsFactors = FALSE)

for (f in files) {
  src_path <- f
  tmp_r <- NULL
  if (grepl("\\.Rmd$", f, ignore.case = TRUE)) {
    tmp_r <- tempfile(fileext = ".R")
    tryCatch({ tangle_rmd(f, tmp_r); src_path <- tmp_r }, error = function(e) NULL)
    if (is.null(tmp_r) || !file.exists(tmp_r)) next
  }
  parsed <- tryCatch(parse(src_path), error = function(e) NULL)
  if (is.null(parsed)) next

  acc <- new.env()
  acc$names <- character()
  for (top in as.list(parsed)) {
    acc <- collect_assigned(top, acc)
  }
  nms <- unique(acc$names)
  if (length(nms) > 0) {
    out <- rbind(out, data.frame(file = f, variable = nms, stringsAsFactors = FALSE))
  }
  if (!is.null(tmp_r)) unlink(tmp_r)
}

write.csv(out, "D:/Users/martensn/BRAIN_DRAIN/migration/assigned_symbols.csv", row.names = FALSE)
cat("Done. Assigned-symbol rows:", nrow(out), "\n")
