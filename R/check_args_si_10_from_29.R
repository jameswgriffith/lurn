check_args_si_10_from_29 <- function(input,
                                     transfer_vars,
                                     warn_or_stop) {

  if (!is.data.frame(input)) {
    stop("Your input must be a dataframe. Please try again",
         call. = FALSE)
  }

  returned_vars <-
    c("lurn_si_10_from_29_score",
      "lurn_si_10_from_29_bother",
      "lurn_si_10_from_29_count_valid",
      "lurn_si_10_from_29_note")

  if (any(returned_vars %in% names(input))) {
    stop("Variable names resulting from obtainings the LURN SI-10 score from ",
         "the SI-29 scoring are ",
         "already found in the names of your input. ",
         "Please rename or remove these variables from your input ",
         "and try again.\n\n",
         "The offending variables found in your input were:\n\n",
         paste(returned_vars[returned_vars %in% names(input)], collapse = " "),
         "\n\n",
         call. = FALSE)
  }

  lurn_si_29_names <- lurn_si_29_names()

  # Are all of the items in the LURN SI-29 found in the input
  if (!all(lurn_si_29_names %in% names(input))) {

    si_29_names_not_found <-
      lurn_si_29_names[which(!lurn_si_29_names %in% names(input))]

    stop("\nNot all of the names for the LURN SI-29 items ",
         "were found in the input.\n\n",
         "Please name the 29 scored items of the LURN SI-29 as follows:\n",
         paste(lurn_si_29_names, collapse = " "),
         "\n\nThe following LURN SI-29 items were not found in the input:\n",
         paste(si_29_names_not_found, collapse = " "),
         "\n\nPlease try again.\n",
         call. = FALSE)
  }

  if (!all(transfer_vars %in% names(input))) {
    stop("\n\nWe can only return the scores of the LURN SI-29 and variables ",
         "found in the input. \nPlease try again. For transfer_vars, ",
         "choose only variable ",
         "names found in the input.",
         call. = FALSE)
  }

  invisible(NULL)

}
