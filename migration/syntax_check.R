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

errors <- character()
ok_count <- 0
for (f in files) {
  src_path <- f
  tmp_r <- NULL
  if (grepl("\\.Rmd$", f, ignore.case = TRUE)) {
    tmp_r <- tempfile(fileext = ".R")
    tangle_rmd(f, tmp_r)
    src_path <- tmp_r
  }
  result <- tryCatch({ parse(src_path); "OK" }, error = function(e) conditionMessage(e))
  if (identical(result, "OK")) {
    ok_count <- ok_count + 1
  } else {
    errors <- c(errors, paste0(f, ": ", result))
  }
  if (!is.null(tmp_r)) unlink(tmp_r)
}

cat("Parsed OK:", ok_count, "/", length(files), "\n")
if (length(errors) > 0) {
  cat("PARSE ERRORS:\n")
  for (e in errors) cat(" ", e, "\n")
}
