check_args_score_lurn_si_29 <- function(input,
                                        transfer_vars,
                                        warn_or_stop) {

  if (!is.data.frame(input)) {
    stop("Your input must be a dataframe. Please try again",
         call. = FALSE)
  }

  returned_vars <-
    c("lurn_si_29_total_score",
      "lurn_si_29_incontinence_score",
      "lurn_si_29_pain_score",
      "lurn_si_29_voiding_score",
      "lurn_si_29_urgency_score",
      "lurn_si_29_nocturia_score",
      "lurn_si_29_bother",
      "lurn_si_29_note",
      "lurn_si_29_total_count_valid",
      "lurn_si_29_incontinence_count_valid",
      "lurn_si_29_pain_count_valid",
      "lurn_si_29_voiding_count_valid",
      "lurn_si_29_urgency_count_valid",
      "lurn_si_29_nocturia_count_valid")

  if (any(returned_vars %in% names(input))) {
    stop("Variable names resulting from LURN SI-29 scoring are ",
         "already found in the names of your input. ",
         "Please rename or remove these variables from your input ",
         "and try again.\n\n",
         "The offending variables found in your input were:\n\n",
         paste(returned_vars[returned_vars %in% names(input)], collapse = " "),
         "\n\n",
         call. = FALSE)
  }

  sex_vname <- "SEX"

  # Is the name of the sex variable found in the input?
  if (!sex_vname %in% names(input)) {
    stop("The variable ", sex_vname, " is not found among ",
         "the variable names of the input.\n",
         "Note: Case matters",
         "Please check your input and try again.",
         call. = FALSE)
  }

  # Are the levels of the sex variable correct?
  sex <- input[[sex_vname]]

  sex_levels <- c(female = 1, male = 2)

  if (any(!sex %in% c(sex_levels, NA))) {
    if (warn_or_stop == "stop") {
      stop("The variable for sex contains values besides 1 and 2.\n ",
           "Please code ", sex_vname, " as follows:\n",
           "1 = female\n",
           "2 = male\n",
           "NA = missing and/or other values\n\n",
           "Any missing values for SEX will result ",
           "in missing LURN SI-29 scores.",
      call. = FALSE)
      }

    if (warn_or_stop == "warn") {
      warning("The variable for sex contains values besides 1 and 2.\n ",
           "Please ensure ", sex_vname, " is coded as follows:\n",
           "1 = female\n",
           "2 = male\n",
           "NA = missing and/or other values\n\n",
           "Any missing values for SEX will result ",
           "in missing LURN SI-29 scores.\n\n",
           "Values in SEX outside 1 or 2 will be recoded to NA.\n",
      call. = FALSE,
      immediate. = TRUE)
      }
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
