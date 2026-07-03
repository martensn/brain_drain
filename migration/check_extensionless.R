for (f in c("outsample_values", "restrict_values")) {
  path <- file.path("P:/BRAIN_DRAIN/Data/_review/needs_inspection", f)
  result <- tryCatch({
    obj <- readRDS(path)
    dims <- if (!is.null(dim(obj))) paste(dim(obj), collapse = "x") else paste0("length ", length(obj))
    paste("OK, class=", class(obj)[1], "dims=", dims)
  }, error = function(e) paste("FAILED -", conditionMessage(e)))
  cat(f, ":", result, "\n")
}
