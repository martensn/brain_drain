function(data, reference_col, output_col, column_prefix, max) {
  data[, (output_col) := {
    equality <- lapply(.SD, `==`, get(reference_col))  # Check if columns match reference_col
    matches <- Reduce(`|`, equality)  # Combine with OR to check if any match exists
    inequality <- lapply(.SD, `!=`, get(reference_col))  # Check if columns don't match reference_col
    no_matches <- Reduce(`|`, inequality)  # Combine with OR to check if any mismatch exists
    has_data <- rowSums(!is.na(.SD)) > 0  # Check if there's valid data in any column
    
    # Apply case_when logic
    case_when(
      is.na(matches) & is.na(no_matches) ~ NA,
      matches == TRUE & is.na(no_matches) ~ 0,
      is.na(matches) & no_matches == TRUE ~ 1,
      matches == TRUE & no_matches == FALSE ~ 0,
      matches == FALSE & no_matches == TRUE ~ 1,
      matches == TRUE & no_matches == TRUE ~ 1
    )
  }, .SDcols = paste0(column_prefix, 0:max)]
}
