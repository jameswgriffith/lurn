#' Uses ggplot2 to create helpful plots for the LURN SI-10.
#' Plots include a stacked bar chart ("item plot")
#' for each item of the LURN SI-10
#' (All 10 symptom items, plus the bother item), as well as a histogram.
#'
#' @description Selecting "item plot" (the default)
#' will create a stacked bar chart
#' using ggplot2 for each item of
#' the LURN SI-10 (All 10 symptom items, plus the bother item).
#' Any responses that are out-of-range or character data will show up in red,
#' signalling a potential problem in your data. Selecting "histogram"
#' will create a histogram with intervals of [0, 2), [2, 4) and so on.
#'
#' @details Any dataframe scored using score_lurn_si_10() will be given
#' a class of "LURN_SI_10", allowing autoplot() to be used.
#'
#' @param object A dataframe containing LURN SI-10 items. Other columns may
#' also be present. The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names.
#'
#' @param plot_type A parameter used to choose from among built-in
#' plot types. Choices are "item plot" or "histogram:.
#'
#' @param title We encourage you to use a descriptive title for your plot.
#' This is NULL by default, which will not include a title
#'
#' @param hist_caption_stats Logical parameter allowing you to choose
#' whether you would like LURN SI-10 statistics to be printed in the
#' caption of your histogram.
#'
#' @param hist_color This is set to a pale blue-green by default ("#7bccc4").
#' If you prefer a black-and-white graph, we recommend "darkgray"
#'
#' @param n_digits The number of decimals to display in the histogram caption.
#' (set to 1 by default).
#'
#' @param ... Other arguments passed to other functions.
#'
#' @section Item response coding: Items 1-8 are coded with 0-4;
#' Items 9, 10 are coded with 0-3;
#' the bother question is coded with 0-3.
#' This coding must be respected in order for the plot to be properly produced.
#'
#' @seealso You can use \code{score_lurn_si_10()} to score the LURN SI-10,
#' which will return a dataframe with an additional class of "LURN_SI_10".
#' You can use \code{plot()} to automatically print your plot.
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
#' si10_item_plot +
#' ggtitle("LURN data")
#'
#' }
#' @importFrom ggplot2 autoplot
autoplot.LURN_SI_10 <- function(object,
                                plot_type = c("item plot",
                                              "histogram"),
                                title = NULL,
                                hist_caption_stats = TRUE,
                                hist_color = "#7bccc4",
                                n_digits = 1,
                                ...) {

  error_check_lurn_si_10_plot(object,
                              plot_type,
                              hist_caption_stats)

  plot_type <- match.arg(plot_type)

  switch(plot_type,
    "item plot" = lurn_si_10_item_plot(object,
                                       title,
                                       ...),
    "histogram" = lurn_si_10_histogram(object,
                                       title,
                                       hist_caption_stats,
                                       hist_color,
                                       n_digits,
                                       ...))
}
