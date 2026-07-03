files <- readLines("D:/Users/martensn/BRAIN_DRAIN/migration/scope_files.txt")
files <- files[nchar(trimws(files)) > 0]

# Hand-rolled Rmd -> R tangle (no knitr dependency): extract lines inside ```{r ...} fences
tangle_rmd <- function(rmd_path, out_path) {
  lines <- readLines(rmd_path, warn = FALSE)
  in_chunk <- FALSE
  code <- character()
  for (ln in lines) {
    if (!in_chunk && grepl("^```\\{r", ln)) {
      in_chunk <- TRUE
      next
    }
    if (in_chunk && grepl("^```\\s*$", ln)) {
      in_chunk <- FALSE
      next
    }
    if (in_chunk) code <- c(code, ln)
  }
  writeLines(code, out_path)
}

out <- data.frame(file = character(), variable = character(), stringsAsFactors = FALSE)
errors <- character()

for (f in files) {
  src_path <- f
  tmp_r <- NULL
  if (grepl("\\.Rmd$", f, ignore.case = TRUE)) {
    tmp_r <- tempfile(fileext = ".R")
    ok <- tryCatch({
      tangle_rmd(f, tmp_r)
      TRUE
    }, error = function(e) { errors <<- c(errors, paste0(f, ": tangle failed - ", conditionMessage(e))); FALSE })
    if (!ok) next
    src_path <- tmp_r
  }

  parsed <- tryCatch(parse(src_path), error = function(e) {
    errors <<- c(errors, paste0(f, ": parse failed - ", conditionMessage(e)))
    NULL
  })
  if (is.null(parsed)) next

  fn <- function() NULL
  body(fn) <- as.call(c(as.name("{"), as.list(parsed)))

  globs <- tryCatch(
    codetools::findGlobals(fn, merge = FALSE),
    error = function(e) { errors <<- c(errors, paste0(f, ": findGlobals failed - ", conditionMessage(e))); NULL }
  )
  if (is.null(globs)) next

  vars <- globs$variables
  if (length(vars) > 0) {
    out <- rbind(out, data.frame(file = f, variable = vars, stringsAsFactors = FALSE))
  }

  if (!is.null(tmp_r)) unlink(tmp_r)
}

write.csv(out, "D:/Users/martensn/BRAIN_DRAIN/migration/free_variables_raw.csv", row.names = FALSE)
writeLines(errors, "D:/Users/martensn/BRAIN_DRAIN/migration/scan_errors.log")
cat("Done. Rows:", nrow(out), " Errors:", length(errors), "\n")
