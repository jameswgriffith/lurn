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

  if (length(problem_vars_num_out_of_rng) > 0 &&
      length(problem_vars_non_num) > 0 &&
      warn_or_stop == "stop") {
    stop("Your input contains both non-numeric and out-of-range values.\n",
         "The following variables contain ",
         "out-of-range values:\n\n",
         paste(problem_vars_num_out_of_rng, collapse = " "),
         "\n\nThe following variables contain non-numeric data:\n\n",
         paste(problem_vars_non_num, collapse = " "),
         "\n\nPlease fix your input, check it carefully, and try again.",
         call. = FALSE)
  }

  if (length(problem_vars_num_out_of_rng) > 0  &&
      warn_or_stop == "stop") {
    stop("Your input contains out-of-range values.\n",
         "The following variables contain ",
         "out-of-range values:\n\n",
         paste(problem_vars_num_out_of_rng, collapse = " "),
         "\n\nPlease fix your input, check it carefully, and try again.",
         call. = FALSE)
  }

  if (length(problem_vars_non_num) > 0  &&
      warn_or_stop == "stop") {
    stop("Your input contains non-numeric values.\n",
         "The following variables contain non-numeric data:\n\n",
         paste(problem_vars_non_num, collapse = " "),
         "\n\nPlease fix your input, check it carefully, and try again.",
         call. = FALSE)
  }

  # Print warnings
  if (length(problem_vars_num_out_of_rng) > 0 &&
      warn_or_stop == "warn") {
    warning("The following variables contain some ",
            "out-of-range values:\n\n",
            paste(problem_vars_num_out_of_rng, collapse = " "),
            "\n\nIf this is not expected, ",
            "please check your input and your results carefully.",
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_non_num) > 0 &&
      warn_or_stop == "warn") {
    warning("The following variables contain some non-numeric values:\n\n",
            paste(problem_vars_non_num, collapse = " "),
            "\n\nIf this is not expected, ",
            "please check your input and your results carefully",
            call. = FALSE,
            immediate. = TRUE)
  }
}
