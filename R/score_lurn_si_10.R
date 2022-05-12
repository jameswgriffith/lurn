#' Returns a score for the SI-10 based on a dataframe of input
#'
#' @description This function returns takes a dataframe, extracts the SI-10
#' items, calculates the SI-10 score, and returns the score along with any
#' other variables from input requested by the user
#'
#' @details
#' If only a subset of variables are desired to be returned,
#' the column names can be specified in transfer_vars.
#' It should be noted that out-of-range
#' values in the items are recoded to NA. Moreover, any character data
#' will be coerced to numeric or NA as part of the scoring.
#'
#' @param input A dataframe containing LURN SI-10 items. Other columns may also
#' be present and will be returned by the function (if desired). The items of
#' the SI-10 must use the recommended names: SI10_Q1 - SI10_Q10. The input
#' cannot contain variable names to be returned: lurn_si_10_score,
#' lurn_si_10_count_valid, or lurn_si_10_note.
#'
#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with SI-10 scores
#'
#' @param warn_or_stop If set to "warn", warnings will notify the user that
#' non-numeric or out-of-range data are present. If set to "stop", the
#' warnings will be printed, but execution will stop. In this case, the user
#' will need to fix the input data in order to proceed. The default is "warn",
#' but set to "stop" if you wish to prevent the function from executing when
#' the input contains any out-of-range or non-numeric data.
#'
#' @seealso lurn_si_10_names()
#'
#' @return A dataframe of output containing SI-10 scores,
#' a count of valid items, and a scoring note. Any variables
#' requested in transfer_vars will also be returned.
#' @export
#'
#' @examples
#' \dontrun{
#' score_lurn_si_10(input = lurn_data)
#' }
score_lurn_si_10 <- function(input,
                             transfer_vars = names(input),
                             warn_or_stop = c("warn", "stop")) {

  warn_or_stop <- match.arg(warn_or_stop)

  # Convert vector to one-row dataframe, if needed
  if (is.vector(input)) {
    input <- as.data.frame(t(input))
  }

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
  if(is.vector(si_10_recoded)) {
    si_10_recoded <- as.data.frame(t(si_10_recoded))
  }

  # Convert out-of-range values to NA
  for(i in seq_len(ncol(si_10_recoded))) {
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
  for(i in seq_len(nrow(input))) {
    if(count_valid[i] > 5) {
      lurn_si_10_score[i] <- si_10_item_sum[i] / max_possible[i] * 38
    } else {
      lurn_si_10_score[i] <- NA
      lurn_si_10_note[i] <- "Missing total score due to half or more items missing among all contributing questions"
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
