#' Creates plots for the LURN SI-10, after it has been scored.
#'
#' @section plot_type = "item plot":
#' An "item plot" (the default) #' will create a stacked bar chart
#' using ggplot2 for each item of the LURN SI-10
#' (All 10 symptom items, plus the bother item).
#' Any responses that are out-of-range or character data will show up in red,
#' signalling a potential problem in your data.
#'
#' @section plot_type = "histogram": The histogram will have bin intervals
#' of [0, 2), [2, 4) and so on, which line up nicely for the 0-38 scale
#' of the LURN SI-10.
#'
#' @details Any dataframe scored using score_lurn_si_10() will be given
#' a class of "LURN_SI_10", plot() or autoplot() to be used to create
#' various plots (note: there are only two options at the moment).
#'
#' @param x A dataframe, with the additional class of "LURN_SI_10", which
#' contains LURN SI-10 items and scores. Other columns may also be present.
#' The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names.
#'
#' @param plot_type A parameter used to choose from among built-in
#' plot types. Choices are "item plot" (the default), "histogram", or
#' "plot.default". The last option is to use default "graphics" plot
#' on your "LURN_SI_10" object rather than the built-in plots.
#'
#' @param title We encourage you to use a descriptive title for your plot.
#' This is NULL by default, which will not include a title.
#'
#' @param hist_caption_stats Logical parameter allowing you to choose
#' whether you would like LURN SI-10 statistics to be printed in the
#' caption of your histogram. This parameter is relevant only for
#' the histogram.
#'
#' @param hist_color This is set to a pale blue-green by default ("#7bccc4").
#' If you prefer a grayscale graph, we recommend "darkgray". This parameter
#' is only relevant for the histogram.
#'
#' @param digits The number of decimals to display in the summary statistics
#' in the caption of the histogram. This is set to 1 by default. This is
#' only relevant for the histogram.
#'
#' @param ... Other arguments.
#'
#' @section Item response coding: Items 1-8 are coded with 0-4;
#' Items 9, 10 are coded with 0-3;
#' the bother question is coded with 0-3.
#' This coding must be respected in order for the plot to be properly produced.
#'
#' @seealso You can use \code{score_lurn_si_10()} to score the LURN SI-10,
#' which will return a dataframe with an additional class of "LURN_SI_10".
#'
#' @return The function returns NULL invisibly. When this plot method is used,
#' a ggplot2 object will be created using autoplot() and printed.
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
#'
#' # Print a plot directly from the data
#' plot(lurn_si_10_test_data)
#'
#' }
plot.LURN_SI_10 <- function(x,
                            plot_type = c("item plot",
                                          "histogram",
                                          "plot.default"),
                            title = NULL,
                            hist_caption_stats = TRUE,
                            hist_color = "#7bccc4",
                            digits = 1,
                            ...) {

  plot_type <- match.arg(plot_type)

  if (plot_type == "plot.default") {

    class(x) <- class(x)[class(x) != "LURN_SI_10"]
    plot(x, ... = ...)

  } else {

    invisible(x)

    print(ggplot2::autoplot(object = x,
                            plot_type = plot_type,
                            title = title,
                            hist_caption_stats = hist_caption_stats,
                            hist_color = hist_color,
                            digits = digits,
                            ...), ...)
  }

  invisible(NULL)

}
