check_si_29_items <- function(si_29_items,
                             warn_or_stop) {

  # Initialise empty vector for variable names with problems
  problem_vars_non_num <- vector(mode = "character")
  problem_vars_out_of_rng <- vector(mode = "character")

  # Initialise other variables
  item_names_to_check <- names(si_29_items)
  item_ranges = lurn_si_29_item_ranges(include_na = TRUE)

  for(i in seq_along(item_names_to_check)) {

    item_name <- item_names_to_check[i]
    col_to_chk <- si_29_items[[item_name]]

    item_range <- item_ranges[[i]]

    if(any(!col_to_chk %in% item_range, na.rm = TRUE)) {
      problem_vars_out_of_rng <- append(problem_vars_out_of_rng, item_name)
    }

    if(any(!is.numeric(col_to_chk[!is.na(col_to_chk)]), na.rm = TRUE)) {
      problem_vars_non_num <- append(problem_vars_non_num, item_name)
    }
  }

  # Print messages
  if (length(problem_vars_out_of_rng) > 0) {
    warning("The following variables contain some out-of-range values:\n\n",
            paste(problem_vars_out_of_rng, collapse = " "),
            "\n\nPlease check your data carefully.",
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_non_num) > 0) {
    warning("The following variables contain some non-numeric values:\n\n",
            paste(problem_vars_non_num, collapse = " "),
            "\n\nPlease check your data carefully.",
            call. = FALSE,
            immediate. = TRUE)
  }

  if ((length(problem_vars_out_of_rng) > 0 ||
       length(problem_vars_non_num) > 0) &&
      warn_or_stop == "warn") {
    message("\nNote: Out-of-range and non-numeric ",
            "items will be recoded to NA.")
  }

  if ((length(problem_vars_out_of_rng) > 0 ||
       length(problem_vars_non_num) > 0) &&
      warn_or_stop == "stop") {
    stop("You have set warn_or_stop to \"stop\" ",
         "to halt execution in the case of bad input.\n\n",
         "Because your input contains out-of-range or non-numeric data,\n",
         "you will need to fix your input and try again.\n\n",
         "Please carefully your input, remove any out-or-range and numeric data, and try again.",
         call. = FALSE)
  }
}
