free <- read.csv("D:/Users/martensn/BRAIN_DRAIN/migration/free_variables_raw.csv", stringsAsFactors = FALSE)
assigned <- read.csv("D:/Users/martensn/BRAIN_DRAIN/migration/assigned_symbols.csv", stringsAsFactors = FALSE)

# also drop names assigned in the SAME file anywhere (findGlobals already should exclude these,
# but our tangle-based re-parse could miss a chunk boundary edge case, so double-check)
same_file_assigned <- split(assigned$variable, assigned$file)

candidates <- data.frame(file = character(), variable = character(), producer_files = character(), stringsAsFactors = FALSE)

for (i in seq_len(nrow(free))) {
  f <- free$file[i]
  v <- free$variable[i]
  if (!is.null(same_file_assigned[[f]]) && v %in% same_file_assigned[[f]]) next  # actually assigned in same file somewhere
  producers <- unique(assigned$file[assigned$variable == v & assigned$file != f])
  if (length(producers) > 0) {
    candidates <- rbind(candidates, data.frame(file = f, variable = v, producer_files = paste(producers, collapse = ";"), stringsAsFactors = FALSE))
  }
}

candidates <- unique(candidates)
write.csv(candidates, "D:/Users/martensn/BRAIN_DRAIN/migration/coupling_candidates.csv", row.names = FALSE)
cat("Candidate coupling rows:", nrow(candidates), "\n")
cat("Distinct dependent files:", length(unique(candidates$file)), "\n")
cat("Distinct variable names:", length(unique(candidates$variable)), "\n")
