#' Plots a stacked bar chart for each item of the LURN SI-10
#' (All 10 symptom items, plus the bother item). This plot method
#' can be used after the LURN SI-10 data have been scored.
#'
#' @description Plots a stacked bar chart for each item of
#' the LURN SI-10 (All 10 symptom items, plus the bother item).
#' Any responses that are out-of-range or character data will show up in red,
#' signalling a potential problem in your data.
#'
#' @details Any dataframe scored using score_lurn_si_10() will be given
#' a class of "LURN_SI_10", plot() or autoplot() to be used to create this plot.
#' (Other S3 methods will be added later).
#'
#' @param x A dataframe, with the additional class of "LURN_SI_10", which
#' contains LURN SI-10 items. Other columns may also be present.
#' The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names.
#'
#' @param ... Other arguments passed to autoplot, ggplot2, or print
#'
#' @section Item response coding: Items 1-10 must be coded with 0-4 for
#' Items 1-8 and 0-3 for Items 9, 10, and the BOTHER question.
#' This coding must be respected in order for the plot to be properly produced.
#'
#' @seealso You can use \code{score_lurn_si_10()} to score the LURN SI-10,
#' which will return a dataframe with an additional class of "LURN_SI_10".
#'
#' @return A ggplot2 object. Use \code{print()} to display the plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' autoplot(lurn_si_10_test_data)
#'
#' # Save the plot
#' si10_item_plot <- autoplot(lurn_si_10_test_data)
#' print(si10_item_plot)
#' plot(si10_item_plot)
#'
#' }
#'
#' @importFrom ggplot2 autoplot
plot.LURN_SI_10 <- function(x, ...) {
  print(ggplot2::autoplot(x, ...), ...)
  invisible()
}