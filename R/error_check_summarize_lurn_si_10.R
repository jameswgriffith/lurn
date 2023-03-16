error_check_summarize_lurn_si_10 <- function(x, digits) {
  if (!is.data.frame(x)) {
    stop("In order for the lurn_si_10_item_plot function to work properly, ",
         "the input must be a dataframe. Please fix your input and try again.",
         call. = FALSE)
  }

  if (digits < 1) {
    stop("digits must be set to a whole number, 1 or greater.\n",
         "Please try again.",
         call. = FALSE)
  }
}
