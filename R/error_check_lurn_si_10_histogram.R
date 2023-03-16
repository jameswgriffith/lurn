error_check_lurn_si_10_histogram <- function(input,
                                             title,
                                             hist_caption_stats,
                                             hist_color,
                                             digits) {

  if (!is.data.frame(input)) {
    stop("In order to use the built-in LURN SI-10 histogram, ",
         "the input must be a dataframe that contains the LURN SI-10 items, ",
         "or the SI-10 score (variable name: lurn_si_10_score).",
         call. = FALSE)
  }

  lurn_si_10_names <- lurn_si_10_names(include_bother_item = FALSE)

  lurn_si_10_output_vars <- c("lurn_si_10_score", "lurn_si_10_count_valid")

  nms_and_output <- c(lurn_si_10_names, lurn_si_10_output_vars)

  if (!any(nms_and_output %in% names(input))) {
    stop("In order to use the built-in histogram, ",
         "the variable names for the LURN SI-10 must be included ",
         "in the input, or the variable names for the scored SI-10 must ",
         "be included. None of these were found.\n\nPlease try again.\n\n",
         "Please note: For the SI-10 histogram, this does not include SI10_BOTHER.",
         call. = FALSE)
  }

  if (!all(lurn_si_10_names %in% names(input)) &&
      !all(lurn_si_10_output_vars %in% names(input))) {
    stop("In order to use the built-in histogram, ",
         "the variable names for the LURN SI-10 must be included ",
         "in the input, or the variable names for the scored SI-10 must ",
         "be included. Please check your input and try again.\n\n",
         "Please note: For SI-10 the histogram, this does not include SI10_BOTHER.",
         call. = FALSE)
  }

  if (!all(lurn_si_10_names %in% names(input)) &&
      hist_caption_stats) {
    stop("In order to use create the caption for the histogram, ",
         "the variable names for the LURN SI-10 items must ",
         "be found in the input.\n\nPlease try again.\n\n",
         call. = FALSE)
  }

  invisible(NULL)

}
