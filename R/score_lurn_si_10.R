#' Returns a score for the SI-10 based on a dataframe of input
#'
#' @description This function returns takes a dataframe, extracts the
#' LURN SI-10 items, calculates the LURN SI-10 score, and returns the
#' score along with any other requested variables from the input.
#'
#' @details
#' If only a subset of variables are desired to be returned,
#' the column names can be specified in transfer_vars.
#' It should be noted that if any out-of-range or non-numeric
#' (e.g., character) values are detected, then execution of the
#' function will stop by default. You can set warn_or_stop to "warn"
#' (see below), in which case any out-of-range values or non-numeric
#' values are recoded to NA, and a warning will be displayed. We recommend
#' that you pre-process your data so that all values are in-range so that
#' execution can proceed normally and no warnings are needed.
#'
#' @param input A dataframe containing LURN SI-10 items. Other columns may also
#' be present and will be returned by the function (if desired). The items of
#' the SI-10 must use the recommended names: SI10_Q1 - SI10_Q10. The input
#' cannot contain any variable names that need to be returned as part of the
#' scoring: lurn_si_10_score, lurn_si_10_count_valid, or lurn_si_10_note. Any
#' character data will be coerced to numeric (e.g., "1" will be coerced to 1).
#'
#' @section Item response coding: Items 1-10 must be coded with 0-4 for
#' Items 1-8 and 0-3 for Items 9 and 10.
#' This coding must be respected in order for the algorithm to work properly.
#' You can check the numbering on the official versions on the questionnaires
#' found at \url{https://nih-lurn.org/Resources/Questionnaires}.

#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with the
#' LURN SI-10 scores.
#'
#' @param warn_or_stop This is set to "stop" by default, which means that any
#' problems in the input will cause execution of the function to stop
#' (i.e., you will receive an error).
#' Problems in the input include any out-of-range or non-numeric data
#' in the input.
#' If you receive an error message, you
#' will need to fix your input in order to proceed.
#' If warn_or_stop is set to "warn", then warnings will notify you that
#' non-numeric or out-of-range data are present, which are then recoded
#' to NA with a warning message.
#'
#' @seealso lurn_si_10_names()
#'
#' @return A dataframe of output containing LURN SI-10 scores,
#' a count of valid items, and a scoring note. Any variables
#' requested in transfer_vars will also be returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' score_lurn_si_10(input = lurn_data)
#' }
score_lurn_si_10 <- function(input,
                             transfer_vars = names(input),
                             warn_or_stop = c("stop", "warn")) {

  warn_or_stop <- match.arg(warn_or_stop)

  # Check for errors in the the arguments
  check_args_score_lurn_si_10(input = input,
                              transfer_vars = transfer_vars)

  si_10_names <- lurn_si_10_names(include_bother_item = FALSE)

  # Check each column for out-of-range or non-numeric data
  check_si_10_items(si_10_names = si_10_names,
                    input = input,
                    warn_or_stop = warn_or_stop)

  # Extract SI-10 items
  si_10 <- input[si_10_names]

  n <- nrow(si_10)

  si_10_recoded <- suppressWarnings(
    vapply(si_10, as.numeric, numeric(n)))

  item_ranges <- lurn_si_10_item_ranges()

  # Convert vector to one-row dataframe
  if (is.vector(si_10_recoded)) {
    si_10_recoded <- as.data.frame(t(si_10_recoded))
  }

  # Convert out-of-range values to NA
  for (i in seq_len(ncol(si_10_recoded))) {
    j <- which(!si_10_recoded[[i]] %in% item_ranges[[i]])
    si_10_recoded[[i]][j] <- NA
  }

  count_valid <- count_not_na(si_10_recoded)

  si_10_item_sum <- apply(si_10_recoded,
                          1,
                          function(x) sum(x, na.rm = TRUE))

  max_possible <- apply(si_10_recoded,
                        1,
                        max_possible_si_10)

  # Initialise empty vector
  lurn_si_10_score <- vector(mode = "numeric",
                             length = nrow(input))

  lurn_si_10_note <- vector(mode = "character",
                             length = nrow(input))

  # Score SI-10
  for (i in seq_len(nrow(input))) {
    if(count_valid[i] > 5) {
      lurn_si_10_score[i] <- si_10_item_sum[i] / max_possible[i] * 38
    } else {
      lurn_si_10_score[i] <- NA
      lurn_si_10_note[i] <-
        paste("Missing total score due to half or more items missing",
        "among all contributing questions")
    }
  }

  # Prepare output
  lurn_si_10_output <- data.frame(
    lurn_si_10_score,
    lurn_si_10_count_valid = count_valid,
    lurn_si_10_note)

  # Return output
  cbind(
    input[transfer_vars],
    lurn_si_10_output)
}