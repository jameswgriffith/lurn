#' Returns a score for the SI-10 based on a dataframe of input
#'
#' @description This function returns takes a dataframe, extracts the SI-10
#' items, calculates the SI-10 score, and returns the score along with any
#' other variables from input requested by the user
#'
#' @details
#' If only a subset of variables are desired, the column names can be
#' specified in transfer_vars. It should be noted that out-of-range
#' values in the items are recoded to NA. Moreover, any character data
#' will be coerced to numeric as part of the scoring.
#'
#' @param input A dataframe containing LURN SI-10 items. Other columns may also
#' be present and will be returned by the function (if desired).
#'
#' @param si_10_names A vector that identifies the names of the LURN SI-10
#' items in the "input" data frame. We strongly recommend that you use the
#' recommended names SI10_Q1 - SI10_Q10.
#'
#' @param returned_vars A vector containing a list of additional scoring
#' variables that may be desired by the user. The user can request all
#' possible variables by specifying "all". Individual options are:
#' \itemize{
#'  \item{lurn_si_10_score - }{LURN SI-10 Score}
#'  \item{lurn_si_count_valid - }{A count of the number of valid items for scoring}
#'  \item{lurn_si_10_floor - }{Is the score at the minumum possible value, 0 (i.e., the floor)?}
#'  \item{lurn_si_10_ceiling - }{Is the score at the minumum possible values, 38 (i.e., the floor)?}
#'  \item{lurn_si_10_note - }{Notes on how the case was scored}
#' }
#'
#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with SI-10 scores
#'
#' @param warn_or_stop If set to "warn", warnings will notify the user that
#' non-numeric or out-of-range data are present. If set to "stop", the
#' warnings will be printed, but execution will stop. In this case, the user
#' will need to fix the input data in order to proceed.
#'
#' @return A dataframe of output containing SI-10 scores.
#' @export
#'
#' @examples
#' \dontrun{
#' score_lurn_si_10(input = lurn_data)
#' }
score_lurn_si_10 <- function(input,
                             si_10_names = paste0("SI10_Q", 1:10),
                             returned_vars = c("lurn_si_10_score",
                                               "lurn_si_10_count_valid"),
                             transfer_vars = names(input),
                             warn_or_stop = c("warn", "stop")) {

  warn_or_stop <- match.arg(warn_or_stop)

  # Convert vector to one-row dataframe
  if(is.vector(input)) {
    input <- as.data.frame(t(input))
  }

  # Check for errors in the the arguments
  check_args_score_lurn_si_10(input = input,
                              si_10_names = si_10_names,
                              returned_vars = returned_vars,
                              transfer_vars = transfer_vars,
                              warn_or_stop = warn_or_stop)

  if("all" %in% returned_vars) {
    returned_vars <- c(
      "lurn_si_10_score",
      "lurn_si_10_count_valid",
      "lurn_si_10_floor",
      "lurn_si_10_ceiling",
      "lurn_si_10_note")
  }

  # Check each column for out-of-range or non-numeric data
  check_si_10_items(si_10_names = si_10_names,
                    input = input,
                    warn_or_stop = warn_or_stop)

  # Extract SI-10 items
  si_10 <- input[si_10_names]

  # Recode input to numeric
  si_10_recoded <- suppressWarnings(
    apply(si_10,
          2,
          as.numeric))

  si_10_recoded[1:8] <- out_of_rng_to_na(as.matrix(si_10_recoded[1:8]), 0:4)
  si_10_recoded[9:10] <- out_of_rng_to_na(as.matrix(si_10_recoded[9:10]), 0:3)

  # Convert vector to one-row dataframe
  if(is.vector(si_10_recoded)) {
    si_10_recoded <- as.data.frame(t(si_10_recoded))
  }

  # Count valid responses
  # count_valid <- rowSums(!is.na(si_10_recoded))

  count_valid <- apply(si_10_recoded,
                       1,
                       function(x) {sum(!is.na(x))})

  # si_10_item_sum <- rowSums(si_10_recoded, na.rm = TRUE)

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

  for(i in seq_len(nrow(input))) {
    if(count_valid[i] > 5) {
      lurn_si_10_score[i] <- si_10_item_sum[i] / max_possible[i] * 38
    } else {
      lurn_si_10_score[i] <- NA
      lurn_si_10_note[i] <- "Missing total score due to half or more items missing among all contributing questions"
    }
  }

  lurn_si_10_floor <- lurn_si_10_score == 0
  lurn_si_10_ceiling <- lurn_si_10_score == 38

  lurn_si_10_all_output <- data.frame(
    lurn_si_10_score,
    lurn_si_10_count_valid = count_valid,
    lurn_si_10_floor,
    lurn_si_10_ceiling,
    lurn_si_10_note)

  output <- cbind(
    input[transfer_vars],
    lurn_si_10_all_output[returned_vars])

  return(output)

}
