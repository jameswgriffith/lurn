check_si_10_items <- function(input,
                              si_10_names,
                              warn_or_stop) {

  check_items_for_miscoding(
    input[si_10_names],
    which_items = "1. LURN SI-10 (not including bother)")

  # Initialise empty vector for variable names with problems,
  # or other issues
  problem_vars_factors <- vector(mode = "character")
  problem_vars_non_factor_non_num <- vector(mode = "character")
  problem_vars_num_out_of_rng <- vector(mode = "character")

  item_ranges <- lurn_si_10_item_ranges(include_bother_item = FALSE,
                                        include_na = TRUE)

  for (i in seq_along(si_10_names)) {

    item_range <- item_ranges[[i]]
    item_name <- si_10_names[i]
    col_to_chk <- input[[si_10_names[i]]]

    if (is.factor(col_to_chk)) {
      problem_vars_factors <- append(problem_vars_factors, item_name)
    }

    if (!is.numeric(col_to_chk) &&
        !is.factor(col_to_chk)) {
      problem_vars_non_factor_non_num <-
        append(problem_vars_non_factor_non_num, item_name)
    }

    # Convert data to numeric type and remove NAs
    col_to_chk_numeric <-
      suppressWarnings(
        as.numeric(as.character(col_to_chk[!is.na(col_to_chk)])))

    if (any(!col_to_chk_numeric %in% item_range)) {
      problem_vars_num_out_of_rng <- append(problem_vars_num_out_of_rng,
                                            item_name)
    }
  }

  stop_or_warn_message(
    problem_vars_factors = problem_vars_factors,
    problem_vars_non_factor_non_num = problem_vars_non_factor_non_num,
    problem_vars_num_out_of_rng = problem_vars_num_out_of_rng,
    q_name = "LURN SI-10",
    warn_or_stop = warn_or_stop)

  invisible(NULL)

}
