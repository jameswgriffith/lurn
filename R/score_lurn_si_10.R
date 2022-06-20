#' Returns a score for the LURN SI-10 based on a dataframe of input
#'
#' @description This function returns takes a dataframe, extracts the
#' LURN SI-10 items, calculates the LURN SI-10 score, and returns the
#' score along with any other requested variables from the input. If
#' you request all of the LURN SI-10 items in the output
#' (which will be true using the default settings),
#' then the output will be given an additional class of
#' "LURN_SI_10", enabling additional S3 methods (e.g., plot).
#'
#' @details
#' If only a subset of variables are desired to be returned,
#' the column names can be specified in transfer_vars.
#' It should be noted that if any out-of-range or non-numeric
#' (e.g., character) values are detected, then execution of the
#' function will stop by default. You can set warn_or_stop to "warn"
#' (see below), in which case any out-of-range values or non-numeric
#' values are recoded to NA, and a warning will be displayed. We strongly
#' recommend that you pre-process your data so that all values are in-range
#' so that execution can proceed normally and no warnings are needed.
#'
#' @param input A dataframe containing LURN SI-10 items. Other columns may also
#' be present and will be returned by the function (if desired). The items of
#' the SI-10 must use the recommended names: SI10_Q1 - SI10_Q10. The input
#' cannot contain any variable names that need to be returned as part of the
#' scoring: lurn_si_10_score, lurn_si_10_count_valid, or lurn_si_10_note. Any
#' character data will be coerced to numeric (e.g., "1" will be coerced to 1).
#' Although it is not scored, the bother items should be named SI10_BOTHER and
#' coded 0, 1, 2, 3. If "warn_or_stop" is set to "warn", the function
#' will attempt to automatically recode factors to numeric type as well, but
#' we recommend using numeric data for the LURN SI-10.
#'
#' @section Item response coding: Items 1-8 are coded with 0-4;
#' Items 9, 10 are coded with 0-3;
#' the bother question is coded with 0-3.
#' This coding must be respected in order for the plot to be properly produced.
#' You can check the numbering on the official versions on the questionnaires
#' found at \url{https://nih-lurn.org/Resources/Questionnaires}. A check on
#' your input will be conducted when you score your data. If your data appear
#' to be mis-coded, you will receive a friendly message encouraging you to
#' carefully check your input.
#'
#' @section Data type for LURN SI-10 items: We recommend that numeric type
#' be used for the LURN SI-10 data. If you use factors,
#' then the function will stop with an error message. If you set
#' "warn_or_stop" to "warn", the function will attempt to fix up your data
#' and present a warning.

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
#' @seealso For a list of recommended variable names for the LURN SI-10,
#' you can use this helper function: \code{lurn_si_10_names()}
#'
#' @return A dataframe of output containing LURN SI-10 scores,
#' a count of valid items, and a scoring note. Any variables
#' requested in transfer_vars will also be returned. If all of the LURN SI-10
#' items are returned with the output, then the output will be given an
#' additional class of "LURN_SI_10" for additional methods
#' (e.g., plot.LURN_SI_10).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' score_lurn_si_10(input = lurn_si_10_test_data)
#' }
score_lurn_si_10 <- function(input,
                             transfer_vars = names(input),
                             warn_or_stop = c("stop", "warn")) {

  warn_or_stop <- match.arg(warn_or_stop)

  # Check for errors in the the arguments
  check_args_score_lurn_si_10(input = input,
                              transfer_vars = transfer_vars,
                              warn_or_stop = warn_or_stop)

  si_10_names <- lurn_si_10_names(include_bother_item = FALSE)

  # Check each column for out-of-range or non-numeric data
  check_si_10_items(input = input,
                    si_10_names = si_10_names,
                    warn_or_stop = warn_or_stop)

  # Extract SI-10 items
  si_10 <- input[si_10_names]

  n <- nrow(si_10)

  # Recode factors to character type
  si10_recoded <- factor_cols_to_char(si_10)

  si_10_recoded <- suppressWarnings(
    vapply(si10_recoded, as.numeric, numeric(n)))

  si_10_recoded <- as.data.frame(si_10_recoded)

  item_ranges <- lurn_si_10_item_ranges(include_bother_item = FALSE)

  # Convert out-of-range values to NA
  for (i in seq_len(ncol(si_10_recoded))) {

    item <- si_10_recoded[[i]]

    item[!item %in% item_ranges[[i]]] <- NA

    si_10_recoded[[i]] <- item
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
    if (count_valid[i] > 5) {
      lurn_si_10_score[i] <- si_10_item_sum[i] / max_possible[i] * 38
      lurn_si_10_note[i] <- NA_character_
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

  # Prepare output
  output <- cbind(
    input[transfer_vars],
    lurn_si_10_output)

  # Return output
  if (all(lurn_si_10_names(include_bother_item = TRUE) %in% names(output))) {
    add_LURN_SI_10_class(output)
  } else {
    output
  }

}
