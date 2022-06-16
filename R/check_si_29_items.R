check_si_29_items <- function(si_29_items,
                             warn_or_stop) {

  check_items_for_miscoding(
    si_29_items,
    which_items = "3. LURN SI-29")

  # Initialise empty vector for variable names with problems,
  # or other issues
  problem_vars_factors <- vector(mode = "character")
  problem_vars_non_factor_non_num <- vector(mode = "character")
  problem_vars_num_out_of_rng <- vector(mode = "character")

  # Initialise other variables
  item_names_to_check <- names(si_29_items)
  item_ranges <- lurn_si_29_item_ranges(include_na = TRUE)

  for (i in seq_along(item_names_to_check)) {

    item_name <- item_names_to_check[i]
    col_to_chk <- si_29_items[[item_name]]
    item_range <- item_ranges[[i]]

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

    if (!all(col_to_chk_numeric %in% item_range)) {
      problem_vars_num_out_of_rng <- append(problem_vars_num_out_of_rng,
                                            item_name)
    }

  }

  stop_or_warn_message(problem_vars_factors,
                       problem_vars_non_factor_non_num,
                       problem_vars_num_out_of_rng,
                       q_name = "LURN SI-29",
                       warn_or_stop)

  invisible(NULL)

}
