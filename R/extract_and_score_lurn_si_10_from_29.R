#' Extracts the scores for the LURN SI-10 from the LURN SI-29.
#'
#' @description All of the items in the LURN SI-10 are contained in the
#' LURN SI-29. This function takes a dataframe, extracts the
#' LURN SI-10 items from the LURN SI-29, and calculates the LURN SI-10 score.
#' The LURN SI-10 score, bother item are returned, along with any
#' with any other variables from "input" that you request.
#'
#' @details
#' If only a subset of variables are desired to be returned,
#' the column names can be specified using  the transfer_vars argument.
#' Any out-of-range or non-numeric data in the input will cause an error,
#' unless you set warn_or_stop to "warn".
#' If warn_or_stop is set to "warn", then out-of-range or non-numeric
#' values will be recoded to NA. Moreover, any character or factor data
#' will be coerced to numeric as part of the scoring.
#'
#' @param input A dataframe containing LURN SI-29 items. Other columns may also
#' be present and will be returned by the function (if desired). The items
#' should be numeric type (e.g., not factors, not character)
#'
#' @section Setting up your input:
#' You must use the recommended variables names for the LURN SI-29, which
#' have the form SI29_Q1, SI29_Q2, etc. For item #27, there is SI29_Q27a
#' for women, and SI29_Q27b for men.
#' You can use \code{lurn_si_29_names()} to obtain a list of the
#' recommended variable names.
#'
#' @section Mapping the LURN SI-10 to the SI-29:
#' #' \itemize{
#'   \item SI10_Q1 = SI29_Q16
#'   \item SI10_Q2 = SI29_Q2
#'   \item SI10_Q3 = SI29_Q3
#'   \item SI10_Q4 = SI29_Q4
#'   \item SI10_Q5 = SI29_Q7
#'   \item SI10_Q6 = SI29_Q12
#'   \item SI10_Q7 = SI29_Q14
#'   \item SI10_Q8 = SI29_Q26
#'   \item SI10_Q9 = SI29_Q21
#'   \item SI10_Q10 = SI29_Q19
#'   \item SI10_BOTHER = SI29_Q29
#' }
#'
#' @section Item response coding: Responses should be represented in the data
#' starting at 0 (e.g., Item 1 should be 0, 1, 2, 3, ...
#' and so on for all items.) This must be respected in order for this R package
#' to score the data properly. Check the official version of the questionnaires
#' at \url{https://nih-lurn.org/Resources/Questionnaires}.
#' A check on your input will be conducted when you score your data.
#' If your data appear to be mis-coded,
#' you will receive a friendly message encouraging you to check your input.
#'
#' @section Data type for LURN SI-29 items: We recommend that numeric type
#' be used for the LURN SI-29 data. If your data are factors,
#' then the function will stop and issue an error message. If you set
#' "warn_or_stop" to "warn", the function will attempt to fix up your data
#' and present a warning.
#'
#' @section Setting up SEX in your input:
#' Your input needs to contain a variable "SEX" with numeric
#' values of "1" for male and "2" for female.
#'
#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with
#' the LURN SI-10 score, bother item, and scoring note.
#'
#' @param warn_or_stop This is set to "stop" by default, which means that any
#' problems in the input will cause execution of the function to stop
#' (i.e., you will receive an error).
#' Problems in the input include any out-of-range or non-numeric data
#' in the input.
#' If you receive an error message, you
#' will need to fix your input in order to proceed.
#' If warn_or_stop is set to "warn", then warnings will notify you that
#' non-numeric or out-of-range data are present, which are then re-coded
#' to NA with a warning message.
#'
#' @return A dataframe of output containing the SI-10 score, bother item,
#'  a scoring note, and any variables requested in transfer_vars.
#'
#' @seealso For a list of recommended variable names for the LURN SI-29,
#' you can use this helper function: \code{\link{lurn_si_29_names}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' extract_and_score_lurn_si_10_from_29(some_si_29_data)
#'
#' extract_and_score_lurn_si_10_from_29(input = lurn_si_29_data)
#' }
extract_and_score_lurn_si_10_from_29 <- function(input,
                                        transfer_vars = names(input),
                                        warn_or_stop = c("stop", "warn")) {

  warn_or_stop <- match.arg(warn_or_stop)

  si_29_names <- lurn_si_29_names()

  # Check the input for errors
  check_args_si_10_from_29(input = input,
                           transfer_vars = transfer_vars,
                           warn_or_stop = warn_or_stop)

  n <- nrow(input[si_29_names])

  # Recode factors to character type
  si_29_items <- factor_cols_to_char(input[si_29_names])

  # Recode non-numeric to NA
  si_29_items <- suppressWarnings(
    vapply(si_29_items, as.numeric, numeric(n)))

  si_29_items <- as.data.frame(si_29_items)

  recode_oor_in_col_to_na(si_29_items,
                          lurn_si_29_item_ranges(include_na = FALSE))

  # Select only the items overlapping with the LURN SI-10
  si_10_item_names <- lurn_si_10_names_from_29(include_bother = FALSE)

  # Save dataframe for LURN SI-10 from LURN SI-29
  si_10_from_29 <- si_29_items[si_10_item_names]

  count_valid <- count_not_na(si_10_from_29)

  si_10_item_sum <- apply(si_10_from_29,
                          1,
                          function(x) sum(x, na.rm = TRUE))

  max_possible <- apply(si_10_from_29,
                        1,
                        max_possible_si_10)

  # Initialise empty vector
  lurn_si_10_score <- vector(mode = "numeric",
                             length = nrow(input))

  lurn_si_10_note <- vector(mode = "character",
                            length = nrow(input))

  # Score SI-10
  for (i in seq_len(n)) {
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

  lurn_si_29_bother <- unname(si_29_items[[si_29_names[29]]])

  # output
  data.frame(
    input[transfer_vars],
    lurn_si_10_from_29_score = lurn_si_10_score,
    lurn_si_10_from_29_bother = lurn_si_29_bother,
    lurn_si_10_from_29_count_valid = count_valid,
    lurn_si_10_from_29_note = lurn_si_10_note,
    check.names = TRUE)

}
