#' Summarizes the LURN SI-10, using data supplied in a dataframe
#'
#' @details Any dataframe containing the items of the LURN SI-10.
#'
#' @param input A dataframe, which
#' contains LURN SI-10 items. Other columns may also be present.
#' The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names.
#'
#' @param digits How may digits to include in the summary table?
#' For the proportion of missing data, as least two decimals will be
#' shown.
#'
#' @seealso You can use \code{score_lurn_si_10()} to score the LURN SI-10.
#'
#' @return NULL is returned. The results are only printed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary(lurn_si_10_test_data)
#' }
summarize_lurn_si_10 <- function(input, digits = 1) {

  # Determine whether input contains lurn_si_10_score and lurn_si_10_count_valid
  if (!all(c("lurn_si_10_score", "lurn_si_10_count_valid") %in%
           names(input))) {
    message("\nThe scores and the valid item count for the LURN SI-10 ",
            "were not found in the input. I will attempt to score the items ",
            "for you.\n\n",
            "The missing variable names are lurn_si_10_score and ",
            "lurn_si_10_count_valid, which are returned by score_lurn_si_10().",
            "\n")
    scored_input <- score_lurn_si_10(input)
    return(summarize_lurn_si_10(scored_input))
  }

  # Check for errors in the input
  error_check_summarize_lurn_si_10(x = input, digits = digits)

  # For proportion missing, always set to at least two digits
  digits_prop <- if (digits < 2) {
    digits_prop <- 2
  } else {
    digits_prop <- digits
  }

  si_10_stats <- summary_stats(input)

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

  print(output)

  invisible(NULL)

}

# Special thanks to stack overflow :
# https://stackoverflow.com/questions/42105336/how-to-round-a-number-and-make-
# it-show-zeros
