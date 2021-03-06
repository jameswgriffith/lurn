#' @importFrom rlang .data
lurn_si_10_histogram <- function(x,
                                 title,
                                 hist_caption_stats = TRUE,
                                 hist_color = "#7bccc4",
                                 digits = 1,
                                 ...) {

  invisible(x)

  dots_args <- list(...)

  if("hist_colour" %in% names(dots_args)) {
    hist_color <- dots_args$hist_colour
  }

  lurn_si_10_mean <-
    format(round(mean(x$lurn_si_10_score, na.rm = TRUE), digits),
           nsmall = digits)
  lurn_si_10_sd <- format(round(stats::sd(x$lurn_si_10_score, na.rm = TRUE),
                                digits), nsmall = digits)
  lurn_si_10_min <- format(round(min(x$lurn_si_10_score, na.rm = TRUE),
                                 digits), nsmall = digits)
  lurn_si_10_mdn <- format(round(stats::median(x$lurn_si_10_score,
                                               na.rm = TRUE), digits),
                           nsmall = digits)
  lurn_si_10_max <- format(round(max(x$lurn_si_10_score, na.rm = TRUE),
                                 digits), nsmall = digits)
  lurn_si_10_n <-
    sum(!is.na(x$lurn_si_10_score), na.rm = TRUE)
  lurn_si_10_n_missing <-
    sum(is.na(x$lurn_si_10_score), na.rm = TRUE)
  lurn_si_10_mean_n_valid <- format(round(mean(x$lurn_si_10_count_valid,
                                               na.rm = TRUE), digits),
                                    nsmall = digits)
  lurn_si_10_mean_n_valid_non_zero <-
    format(round(mean(x$lurn_si_10_count_valid[x$lurn_si_10_count_valid > 0],
                      na.rm = TRUE), digits), nsmall = digits)

  if(hist_caption_stats) {
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

  histogram <- ggplot2::ggplot(x, ggplot2::aes(x = .data$lurn_si_10_score)) +
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
      expand = ggplot2::expansion(add = c(0, 1))) +
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
                    plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
}
