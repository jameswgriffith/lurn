error_check_lurn_si_10_plot <- function(x, plot_type, hist_caption_stats) {
  if (!is.data.frame(x)) {
    stop("In order to use the built-in LURN plots,",
         "the input must be a dataframe, ",
         call. = FALSE)
  }

  lurn_si_10_names <- lurn_si_10_names(include_bother_item = TRUE)

  if (!all(lurn_si_10_names %in% names(x)) &&
      plot_type == "item plot") {
    stop("In order to use the built-in item plot,",
         "the variable names for the LURN SI-10 must ",
         "be found in the input.\n\n",
         "Please note: This includes SI10_BOTHER.",
         call. = FALSE)
  }

  if (!all(lurn_si_10_names %in% names(x)) &&
      plot_type == "histogram" &&
      hist_caption_stats) {
    stop("In order to use create the caption for the histogram,",
         "the variable names for the LURN SI-10 must ",
         "be found in the input.\n\n",
         call. = FALSE)
  }

  if (!any(0:3 %in% x$SI10_BOTHER)) {
    warning("\nThe SI10_BOTHER item should be coded as 0, 1, 2, 3\n",
            "but these values were not found in your data.\n\n",
            "Please check your data carefully and try again, if needed.",
            call. = FALSE)
  }

  if (any(!c("lurn_si_10_score", "lurn_si_10_count_valid") %in%
          names(x)) &&
      plot_type == "histogram" &&
      hist_caption_stats) {
    stop("The scores and the valid item count for the LURN SI-10 ",
         "were not found. These are needed for the caption of the ",
         "histogram.\n\n",
         "Please try again",
         call. = FALSE)
  }

}
