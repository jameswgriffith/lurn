error_check_si_10_item_plot <- function(x, title) {

  if (!is.data.frame(x)) {
    stop("In order for the lurn_si_10_item_plot function to work properly, ",
         "the input must be a dataframe. Please fix your input and try again.",
         call. = FALSE)
  }

  if (!is.null(title) && !is.character(title)) {
    stop("title must be NULL or a character string. Please fix your intput ",
         "and try again.",
         call. = FALSE)
  }

  lurn_si_10_names <- lurn_si_10_names(include_bother_item = TRUE)

  if (!all(lurn_si_10_names %in% names(x))) {
    stop("In order to use the built-in item plot, ",
         "the variable names for the LURN SI-10 must ",
         "be found in the input.\n\n",
         "Please note: This includes SI10_BOTHER.",
         call. = FALSE)
  }

  if (!any(0:3 %in% x$SI10_BOTHER)) {
    warning("\nThe SI10_BOTHER item should be coded as 0, 1, 2, 3\n",
            "but these values were not found in your data.\n\n",
            "Please check your data carefully and try again, if needed.",
            call. = FALSE)
  }

  invisible(NULL)

}
