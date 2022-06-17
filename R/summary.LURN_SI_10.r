#' Summarizes the LURN SI-10, after it has been scored.
#'
#' @details Any dataframe scored using score_lurn_si_10() will be given
#' a class of "LURN_SI_10", which in turn enables summarization using
#' this function.
#'
#' @param object A dataframe, with the additional class of "LURN_SI_10", which
#' contains LURN SI-10 items and scores. Other columns may also be present.
#' The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names.
#'
#' @param n_digits How may digits to include in the summary table?
#' For the proportion of missing data, as least two decimals will be
#' shown.
#'
#' @param ... Other arguments.
#'
#' @seealso You can use \code{score_lurn_si_10()} to score the LURN SI-10,
#' which will return a dataframe with an additional class of "LURN_SI_10".
#'
#' @return NULL. This function only prints the results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary(lurn_si_10_test_data)
#' }
summary.LURN_SI_10 <- function(object,
                               n_digits = 1,
                               ...) {

  # For proportion missing, always set to at least two digits
  n_digits_prop <- if (n_digits < 2) {
    n_digits_prop <- 2
  } else {
    n_digits_prop <- n_digits
  }

  si_10_stats <- summary_stats(object)

  m_sd <- paste0(format(round(si_10_stats["Mean"], n_digits),
                        nsmall = n_digits),
  " (", format(round(si_10_stats["SD"], n_digits), nsmall = n_digits), ")")

  si_10_range <- paste0(format(round(si_10_stats["Minimum"], n_digits),
                               nsmall = n_digits),
                        "-", format(round(si_10_stats["Maximum"],
                                          n_digits), nsmall = n_digits))

  valid_range <- paste0(
    round(si_10_stats["Min. number of valid responses"], n_digits),
    "-", round(si_10_stats["Max. number of valid responses"], n_digits))

    out_vector <- c(
    "N (non-missing scores)" = paste(round(si_10_stats["N (non-missing)"], 0)),
    "Number missing scores" = paste(round(si_10_stats["Number missing"], 0)),
    "Proportion missing" = paste(
      format(round(si_10_stats["Proportion missing"], n_digits_prop),
             nsmall = n_digits_prop)),
    "M (SD)" = m_sd,
    "Median" = paste(format(round(si_10_stats["Median"], n_digits),
                            nsmall = n_digits)),
    "Score range" = si_10_range,
    "Mean num. of valid responses" = paste(
      format(round(si_10_stats["Mean number of valid responses"],
                   n_digits), nsmall = n_digits)),
    "Median num. of valid responses" = paste(
      format(round(si_10_stats["Median number of valid responses"],
                   n_digits), nsmall = n_digits)),
    "Range of num. of valid responses" = valid_range)

  output <- data.frame("Statistic" = out_vector,
    row.names = names(out_vector))

  cat("\nSummary statistics for LURN SI-10\n\n")

  print(output)

  invisible(NULL)

}

# Special thanks to stack overflow :
# https://stackoverflow.com/questions/42105336/how-to-round-a-number-and-make-it-show-zeros
