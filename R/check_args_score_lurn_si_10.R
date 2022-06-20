check_args_score_lurn_si_10 <- function(input,
                                        transfer_vars,
                                        warn_or_stop,
                                        output_LURN_class) {

  si_10_names <- lurn_si_10_names(include_bother_item = FALSE)

  returned_vars <- c("lurn_si_10_score",
                     "lurn_si_10_count_valid",
                     "lurn_si_10_note")

  if (!is.data.frame(input)) {
    stop("Your input must be a dataframe. Please try again",
         call. = FALSE)
  }

  if (any(returned_vars %in% names(input))) {
    stop("Variable names resulting from LURN SI-10 scoring are ",
         "already found in the names of your input. ",
         "Please rename or remove these variables from your input ",
         "and try again.\n\n",
         "The offending variables found in your input were:\n\n",
         paste(returned_vars[returned_vars %in% names(input)], collapse = " "),
         "\n")
  }

  if (ncol(input) < 10) {
    stop("\nYour input has too few columns to score the LURN SI-10.\n",
         "Please re-check your input and try again.",
         call. = FALSE)
  }

  if (!all(si_10_names %in% names(input))) {

    si_10_names_not_found <-
      si_10_names[which(!si_10_names %in% names(input))]

    stop("\nNot all of the names of the LURN SI-10 symptom items were ",
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
         "Please fix the variables listed in transfer_vars and try again.",
         "For transfer_vars, ",
         "choose only variable names that are found in your input.",
    call. = FALSE)
  }

  if(output_LURN_class &&
     !all(lurn_si_10_names() %in% transfer_vars)) {

    stop("output_LURN_class is set to TRUE, but ",
    "some necessary variables are missing from transfer_vars.\n\n",
    "Please fix your function call and try again.\n\n",
    "If output_LURN_class is TRUE, transfer_vars must contain these variables:\n\n",
    paste(lurn_si_10_names(), collapse = " "),
    call. = FALSE)

  }

  invisible(NULL)

}
