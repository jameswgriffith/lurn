check_si_29_items <- function(si_29_items,
                             warn_or_stop) {

  # Initialise empty vector for variable names with problems
  problem_vars_non_num <- vector(mode = "character")
  problem_vars_out_of_rng <- vector(mode = "character")

  # Initialise other variables
  item_names_to_check <- names(si_29_items)
  item_ranges <- lurn_si_29_item_ranges(include_na = TRUE)

  for (i in seq_along(item_names_to_check)) {

    item_name <- item_names_to_check[i]
    col_to_chk <- si_29_items[[item_name]]

    item_range <- item_ranges[[i]]

    if (any(!col_to_chk %in% item_range, na.rm = TRUE)) {
      problem_vars_out_of_rng <- append(problem_vars_out_of_rng, item_name)
    }

    if (any(!is.numeric(col_to_chk[!is.na(col_to_chk)]), na.rm = TRUE)) {
      problem_vars_non_num <- append(problem_vars_non_num, item_name)
    }
  }

  if (length(problem_vars_out_of_rng) > 0 &&
      length(problem_vars_non_num) > 0 &&
      warn_or_stop == "stop") {
    stop("Your input contains both non-numeric and out-of-range values.\n",
         "The following variables contain ",
         "out-of-range values:\n",
         paste(problem_vars_out_of_rng, collapse = " "),
         "\nThe following variables contain non-numeric data:\n\n",
         paste(problem_vars_non_num, collapse = " "),
         "\n\nPlease fix your input, check it carefully, and try again.",
         call. = FALSE)
  }

  if (length(problem_vars_out_of_rng) > 0  &&
      warn_or_stop == "stop") {
    stop("Your input contains out-of-range values.\n",
         "The following variables contain ",
         "out-of-range values:\n\n",
         paste(problem_vars_out_of_rng, collapse = " "),
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
  if (length(problem_vars_out_of_rng) > 0 &&
      warn_or_stop == "warn") {
    warning("The following variables contain some ",
            "out-of-range values:\n\n",
            paste(problem_vars_out_of_rng, collapse = " "),
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
