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
#' @param digits How may digits to include in the summary table?
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
                               digits = 1,
                               ...) {

  # For proportion missing, always set to at least two digits
  digits_prop <- if (digits < 2) {
    digits_prop <- 2
  } else {
    digits_prop <- digits
  }

  si_10_stats <- summary_stats(object)

  m_sd <- paste0(format(round(si_10_stats["Mean"], digits),
                        nsmall = digits),
  " (", format(round(si_10_stats["SD"], digits), nsmall = digits), ")")

  si_10_range <- paste0(format(round(si_10_stats["Minimum"], digits),
                               nsmall = digits),
                        "-", format(round(si_10_stats["Maximum"],
                                          digits), nsmall = digits))

  valid_range <- paste0(
    round(si_10_stats["Min. number of valid responses"], digits),
    "-", round(si_10_stats["Max. number of valid responses"], digits))

    out_vector <- c(
    "N (non-missing scores)" = paste(round(si_10_stats["N (non-missing)"], 0)),
    "Number missing scores" = paste(round(si_10_stats["Number missing"], 0)),
    "Proportion missing" = paste(
      format(round(si_10_stats["Proportion missing"], digits_prop),
             nsmall = digits_prop)),
    "M (SD)" = m_sd,
    "Median" = paste(format(round(si_10_stats["Median"], digits),
                            nsmall = digits)),
    "Score range" = si_10_range,
    "Mean num. of valid responses" = paste(
      format(round(si_10_stats["Mean number of valid responses"],
                   digits), nsmall = digits)),
    "Median num. of valid responses" = paste(
      format(round(si_10_stats["Median number of valid responses"],
                   digits), nsmall = digits)),
    "Range of num. of valid responses" = valid_range)

  output <- data.frame("Statistic" = out_vector,
    row.names = names(out_vector))

  cat("\nSummary statistics for LURN SI-10\n\n")

  output

}

# Special thanks to stack overflow :
# https://stackoverflow.com/questions/42105336/how-to-round-a-number-and-make-it-show-zeros
