# Check each SI-10 variable for out-of-range and/or non-numeric data --------
check_si_10_items <- function(
    si_10_names = lurn_si_10_names(include_bother_item = FALSE),
    input,
    warn_or_stop) {

  # Initialise empty vector for variable names with problems
  problem_vars_non_num <- vector(mode = "character")
  problem_vars_num_out_of_rng <- vector(mode = "character")

  item_ranges <- lurn_si_10_item_ranges(include_na = FALSE)

  for (i in seq_along(si_10_names)) {

    item_range <- item_ranges[[i]]
    item_name <- si_10_names[i]
    col_to_chk <- input[[si_10_names[i]]]

    if (!is.numeric(col_to_chk)) {
      problem_vars_non_num <- append(problem_vars_non_num, item_name)
    }

    col_to_chk_numeric <- suppressWarnings(as.numeric(col_to_chk))
    col_to_chk_numeric <- col_to_chk_numeric[!is.na(col_to_chk_numeric)]

    if (!all(col_to_chk_numeric %in% item_range)) {
      problem_vars_num_out_of_rng <- append(problem_vars_num_out_of_rng,
                                            item_name)
    }

  }

  # Print warnings and messages
  if (length(problem_vars_num_out_of_rng) > 0) {
    warning("The following variables contain some numeric ",
            "but out-of-range values:\n",
            paste(problem_vars_num_out_of_rng, collapse = " "),
            "\n",
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_non_num) > 0) {
    warning("The following variables contain some non-numeric values:\n",
            paste(problem_vars_non_num, collapse = " "),
            "\n",
            call. = FALSE,
            immediate. = TRUE)
  }

  any_problems <- any(length(problem_vars_num_out_of_rng) > 0 ||
                      length(problem_vars_non_num) > 0)

  # Stop execution, if desired
  if (any_problems && warn_or_stop == "stop") {
    stop("Please fix your input and try again.\n\n",
         call. = FALSE)
  }
}
