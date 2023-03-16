#' Returns scores for the LURN SI-29 based on a dataframe of input
#'
#' @description This function returns takes a dataframe, extracts the
#' LURN SI-29 items, calculates the subscale scores, and returns them
#' along with any other variables from "input" that you request.
#'
#' @details
#' If only a subset of variables are desired to be returned,
#' the column names can be specified using  the transfer_vars argument.
#' Any out-of-range or non-numeric data in the input will cause an error,
#' unless you set warn_or_stop to "warn".
#' If warn_or_stop is set to "warn", then out-of-range or non-numeric
#' values will be recoded to NA. Moreover, any character data
#' will be coerced to numeric as part of the scoring.
#'
#' @param input A dataframe containing LURN SI-29 items. Other columns may also
#' be present and will be returned by the function (if desired). The items
#' should be numeric type (e.g., not factors, not character)
#'
#' @section Setting up your input:
#' You must use the recommended variables names for the LURN SI-29, which
#' have the form SI29_Q1, SI29_Q2, etc. For item #27, there is SI29_Q27a
#' for women (SEX = 2), and SI29_Q27b for men (SEX = 1).
#' You can use \code{lurn_si_29_names()} to obtain a list of the
#' recommended variable names.
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
#' @section Setting up Sex in your input:
#' Your input needs to contain a variable "SEX" with numeric
#' values of "1" for male and "2" for female.
#'
#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with
#' the LURN SI-29 scores.
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
#' @return A dataframe of output containing SI-29 scores, and any variables
#' requested in transfer_vars.
#'
#' @seealso For a list of recommended variable names for the LURN SI-29,
#' you can use this helper function: \code{\link{lurn_si_29_names}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' score_lurn_si_29(some_si_29_data)
#'
#' score_lurn_si_29(input = lurn_si_29_data)
#' }
score_lurn_si_29 <- function(input,
                             transfer_vars = names(input),
                             warn_or_stop = c("stop", "warn")) {

  warn_or_stop <- match.arg(warn_or_stop)

  si_29_names <- lurn_si_29_names()

  # Check the input for errors
  check_args_score_lurn_si_29(input = input,
                              transfer_vars = transfer_vars,
                              warn_or_stop = warn_or_stop)

  # Check correspondence of SEX and sex-specific questions
  # Save vectors of 27a, 27b, and SEX variable
  lurn_si_27a_women <- input[[si_29_names[27]]]
  lurn_si_27b_men <- input[[si_29_names[28]]]
  sex <- input[["SEX"]]

  sex_levels <- c(male = 1, female = 2)

  check_si_29_sex_questions(lurn_si_27a_women,
                            lurn_si_27b_men,
                            sex,
                            sex_levels)

  # Check for numeric and out-of-range questions across all 29 items
  check_si_29_items(si_29_items = input[si_29_names],
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

  # Organise variables by subscale
  incontinence_item_names <- si_29_names[1:6]
  pain_item_names <- si_29_names[7:10]
  voiding_item_names <- si_29_names[11:15]
  urgency_item_names <- si_29_names[16:18]
  nocturia_item_names <- si_29_names[19:20]

  # Save dataframes for LURN SI-29 subscales
  incontinence_items <- si_29_items[incontinence_item_names]
  pain_items <- si_29_items[pain_item_names]
  voiding_items <- si_29_items[voiding_item_names]
  urgency_items <- si_29_items[urgency_item_names]
  nocturia_items <- si_29_items[nocturia_item_names]

  # Create vectors with maximum item scores for each subscale
  incontinence_max_items <- c(4, 4, 4, 4, 4, 4)
  pain_max_items <- c(4, 4, 4, 4)
  voiding_max_items <- c(4, 4, 4, 4, 4)
  urgency_max_items <- c(4, 4, 4)
  nocturia_max_items <- c(3, 4)

  # Score five subscales
  lurn_si_29_incontinence_score <- score_lurn_si_29_subscale(incontinence_items,
                                                  incontinence_max_items)
  lurn_si_29_pain_score <- score_lurn_si_29_subscale(pain_items,
                                                     pain_max_items)
  lurn_si_29_voiding_score <- score_lurn_si_29_subscale(voiding_items,
                                                        voiding_max_items)
  lurn_si_29_urgency_score <- score_lurn_si_29_subscale(urgency_items,
                                                        urgency_max_items)
  lurn_si_29_nocturia_score <- score_lurn_si_29_subscale(nocturia_items,
                                                         nocturia_max_items)

  # Count valid responses for output
  lurn_si_29_incontinence_count_valid <- count_not_na(incontinence_items)
  lurn_si_29_pain_count_valid <- count_not_na(pain_items)
  lurn_si_29_voiding_count_valid <- count_not_na(voiding_items)
  lurn_si_29_urgency_count_valid <- count_not_na(urgency_items)
  lurn_si_29_nocturia_count_valid <- count_not_na(nocturia_items)
  lurn_si_29_total_count_valid <- count_not_na(si_29_items)

  # Create a vector for SI29_Q27
  SI29_Q27 <- vector("numeric", n)
  SI29_Q27[] <- NA

  # Recode to NA if sex (or "SEX" in the input) is not 1 or 2
  sex[!sex %in% sex_levels] <- NA

  for (i in seq_len(n)) {
    if (!is.na(sex[i]) && sex[i] == sex_levels[2]) {
      SI29_Q27[i] <- lurn_si_27a_women[i]
    } else if (!is.na(sex[i]) && sex[i] == sex_levels[1]) {
      SI29_Q27[i] <- lurn_si_27b_men[i]
    } else {
      SI29_Q27[i] <- NA
    }
  }

  si_29_items <- cbind(si_29_items[1:26],
                       SI29_Q27,
                       si_29_items[29])

  total_max_items <-
    c(4, 4, 4, 4, 4, 4,        # Section A
      4, 4, 4, 4,              # Section B
      4, 4, 4, 4, 4,           # Section C
      4, 4, 4,                 # Section D
      3, 4,                    # Section E
      3, 4, 3, 1, 4, 4, 4, 3)  # Section F

  lurn_si_29_total_score <- score_lurn_si_29_subscale(si_29_items,
                                                      total_max_items)

  lurn_si_29_note <-
    ifelse(lurn_si_29_total_count_valid < 15 |
           lurn_si_29_incontinence_count_valid < 4 |
           lurn_si_29_pain_count_valid < 3 |
           lurn_si_29_voiding_count_valid < 3 |
           lurn_si_29_urgency_count_valid < 2 |
           lurn_si_29_nocturia_count_valid < 2,
           paste("Missing total or subscale scores due to half or more items",
           "missing among all questions or among questions in that subscale"),
           NA_character_)

  lurn_si_29_bother <- unname(si_29_items[[si_29_names[29]]])

  lurn_si_29_all_output <- data.frame(
    lurn_si_29_total_score,
    lurn_si_29_incontinence_score,
    lurn_si_29_pain_score,
    lurn_si_29_voiding_score,
    lurn_si_29_urgency_score,
    lurn_si_29_nocturia_score,
    lurn_si_29_bother,
    lurn_si_29_note,
    lurn_si_29_total_count_valid,
    lurn_si_29_incontinence_count_valid,
    lurn_si_29_pain_count_valid,
    lurn_si_29_voiding_count_valid,
    lurn_si_29_urgency_count_valid,
    lurn_si_29_nocturia_count_valid)

  returned_vars <-
    c("lurn_si_29_total_score",
      "lurn_si_29_incontinence_score",
      "lurn_si_29_pain_score",
      "lurn_si_29_voiding_score",
      "lurn_si_29_urgency_score",
      "lurn_si_29_nocturia_score",
      "lurn_si_29_bother",
      "lurn_si_29_note",
      "lurn_si_29_total_count_valid",
      "lurn_si_29_incontinence_count_valid",
      "lurn_si_29_pain_count_valid",
      "lurn_si_29_voiding_count_valid",
      "lurn_si_29_urgency_count_valid",
      "lurn_si_29_nocturia_count_valid")

  data.frame(
    input[transfer_vars],
    lurn_si_29_all_output[returned_vars],
    check.names = TRUE)

}
