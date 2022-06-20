check_args_score_lurn_si_10 <- function(input,
                                        transfer_vars,
                                        warn_or_stop) {

  si_10_names <- lurn_si_10_names(include_bother_item = FALSE)

  returned_vars <- c("lurn_si_10_score",
                     "lurn_si_10_count_valid",
                     "lurn_si_10_note")

  if (!is.data.frame(input)) {
    stop("Your input must be a dataframe. Please try again",
         call. = FALSE)
  }

  if (ncol(input) < 10) {
    stop("\nYour input has too few columns to score the LURN SI-10.\n",
         "Please re-check your input and try again.",
         call. = FALSE)
  }

  if (!all(si_10_names %in% names(input))) {

    si_10_names_not_found <-
      si_10_names[which(!si_10_names %in% names(input))]

    stop("\nNot all of the names of the LURN SI-10 items were ",
         "found in the input.\n\n",
         "The ten scored items of the LURN SI-10 should be labelled:\n",
         paste(lurn_si_10_names(include_bother_item = FALSE), collapse = " "),
         "\n\nThe following LURN SI-10 names were not found in the input:\n",
         paste(si_10_names_not_found, collapse = " "),
         "\n\nPlease try again.",
         call. = FALSE)
  }

  if (!all(transfer_vars %in% names(input))) {
    stop("\n\nWe can only return the scoring results of ",
         "the LURN SI-10 and ",
         "variables found in the input.\n",
         "Please try again. For transfer_vars, ",
         "choose only variable names found in input.",
    call. = FALSE)
  }

  if (any(returned_vars %in% names(input))) {
    message("Variable names resulting from LURN SI-10 scoring are ",
            "already found in the names of input. ",
            "Please check your output carefully.\n\n",
            "The variables found were ",
            paste(returned_vars[returned_vars %in% names(input)], collapse = " "),
            "\n\n")
  }

  invisible(NULL)

}
