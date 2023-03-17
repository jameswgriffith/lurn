#' Uses ggplot2 to create a histogram for the LURN SI-10.
#'
#' @description For the binning of the histogram, intervals of [0, 2), [2, 4)
#' and so on are used.
#'
#' @param input A dataframe containing LURN SI-10 items. Other columns may
#' also be present. The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names. If
#' the SI-10 has already been scored, the function will look for the score to
#' create the plot. Otherwise, the function will attempt to score the SI-10 in
#' order to create the histogram.
#'
#' @param title We encourage you to use a descriptive title for your plot.
#' This is NULL by default, which will not include a title.
#'
#' @param hist_caption_stats Logical parameter allowing you to choose
#' whether you would like LURN SI-10 statistics to be printed in the
#' caption of your histogram.
#'
#' @param hist_color This is set to a pale blue-green by default ("#7bccc4").
#' If you prefer a black-and-white graph, we recommend "darkgray"
#'
#' @param digits The number of decimals to display in the histogram caption.
#' (set to 1 by default).
#'
#' @section For the LURN SI-10 Item response coding:
#' Items 1-8 are coded with 0-4;
#' Items 9, 10 are coded with 0-3;
#' the bother question is coded with 0-3.
#' This coding must be respected in order for the plot to be properly produced.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lurn_si_10_histogram(some_data,
#' title = "My title goes here")
#' }
lurn_si_10_histogram <- function(input,
                                 title = NULL,
                                 hist_caption_stats = TRUE,
                                 hist_color = "#7bccc4",
                                 digits = 1) {

  # Check for errors
  error_check_lurn_si_10_histogram(input,
                                   title,
                                   hist_caption_stats,
                                   hist_color,
                                   digits)

  # Determine whether input contains lurn_si_10_score and lurn_si_10_count_valid
  if (!all(c("lurn_si_10_score", "lurn_si_10_count_valid") %in%
          names(input))) {
    message("The scores and the valid item count for the LURN SI-10 ",
         "were not found. I will attempt to score the items for you.\n\n",
         "The missing variable names are lurn_si_10_score and ",
         "lurn_si_10_count_valid, which are returned by score_lurn_si_10().",
         "\n\n")
    scored_input <- score_lurn_si_10(input)
    return(lurn_si_10_histogram(scored_input))
  }

  if (hist_caption_stats) {

    lurn_si_10_mean <-
      format(round(mean(input$lurn_si_10_score, na.rm = TRUE), digits),
             nsmall = digits)
    lurn_si_10_sd <- format(round(stats::sd(input$lurn_si_10_score, na.rm = TRUE),
                                  digits), nsmall = digits)
    lurn_si_10_min <- format(round(min(input$lurn_si_10_score, na.rm = TRUE),
                                   digits), nsmall = digits)
    lurn_si_10_mdn <- format(round(stats::median(input$lurn_si_10_score,
                                                 na.rm = TRUE), digits),
                             nsmall = digits)
    lurn_si_10_max <- format(round(max(input$lurn_si_10_score, na.rm = TRUE),
                                   digits), nsmall = digits)
    lurn_si_10_n <-
      sum(!is.na(input$lurn_si_10_score), na.rm = TRUE)
    lurn_si_10_n_missing <-
      sum(is.na(input$lurn_si_10_score), na.rm = TRUE)
    lurn_si_10_mean_n_valid <- format(round(mean(input$lurn_si_10_count_valid,
                                                 na.rm = TRUE), digits),
                                      nsmall = digits)
    lurn_si_10_mean_n_valid_non_zero <-
      format(round(mean(
        input$lurn_si_10_count_valid[input$lurn_si_10_count_valid > 0],
        na.rm = TRUE), digits), nsmall = digits)

    caption_text <- paste0("LURN SI-10 STATISTICS:\n",
                           "Number of scores: ", lurn_si_10_n, "    ",
                           "Number of missing scores: ", lurn_si_10_n_missing, "    ",
                           "Mean number of valid responses: ",
                           lurn_si_10_mean_n_valid, "\n",
                           "Mean number of valid responses (cases with at least one symptom item completed): ",
                           lurn_si_10_mean_n_valid_non_zero, "\n",
                           "Mean(SD): ", lurn_si_10_mean, " (", lurn_si_10_sd, ")    ",
                           "Median: ", lurn_si_10_mdn, "    ",
                           "Minimum: ", lurn_si_10_min, "    ",
                           "Maximum: ", lurn_si_10_max, "\n")
    } else {
      caption_text <- NULL
    }

  histogram <- ggplot2::ggplot(input, ggplot2::aes(x = .data$lurn_si_10_score)) +
    ggplot2::geom_histogram(fill = hist_color,
                   colour = "black",
                   binwidth = 2,
                   boundary = 0,
                   closed = "left") +
    ggplot2::scale_x_continuous(name = "LURN SI-10 Score (0-38)",
                       limits = c(0,40),
                       breaks = seq(0, 38, by = 2),
                       expand = ggplot2::expansion(add = c(1, 1))) +
    ggplot2::scale_y_continuous(name = "Count",
      expand = ggplot2::expansion(mult = c(0, .05))) +
    ggplot2::labs(caption = caption_text) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5,
                   colour = "black",
                   linetype = 1),
                    axis.text.x = ggplot2::element_text(
                    size = ggplot2::rel(1.25)),
                    axis.text.y = ggplot2::element_text(
                    size = ggplot2:: rel(1.25)),
                    axis.title = ggplot2::element_text(
                    size = ggplot2::rel(1.5),
                    face = "plain"),
                    plot.caption = ggplot2::element_text(
                    hjust = 0.0,
                    face = "plain",
                    size = ggplot2::rel(1)),
                    plot.title = ggplot2::element_text(
                      size = ggplot2::rel(1.5)))

  return(histogram)

}
