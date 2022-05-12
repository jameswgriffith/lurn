#' Returns scores for the LURN SI-29 based on a dataframe of input
#'
#' @description This function returns takes a dataframe, extracts the
#' LURN SI-29 items, calculates the subscale scores, and returns them
#' along with any other variables from "input" requested by the user.
#'
#' @details
#' If only a subset of variables are desired, the column names can be
#' specified in transfer_vars. It should be noted that out-of-range
#' values in the items are recoded to NA. Moreover, any character data
#' will be coerced to numeric as part of the scoring.
#'
#' @param input A dataframe containing LURN SI-29 items. Other columns may also
#' be present and will be returned by the function (if desired).
#'
#' @section Setting up your input: Gender
#' Your input needs to contain a variable \code{Gender} with numeric
#' values of "1" for female and "2" for male.
#'
#' @section Setting up your input: Variable names for the LURN SI-29
#' You must use the recommended variables names for the LURN SI-29, which
#' have the form SI29_Q1, SI29_Q2, etc. For item #27, there is SI29_Q27a
#' and SI29_Q27b. You can use \code{lurn_si_29_names()} to obtain a list of the
#' recommended variable names.
#'
#' @param transfer_vars A vector of variable names to be found in input.
#' These variables will be returned in the output along with
#' the LURN SI-29 scores.
#'
#' @param warn_or_stop If set to "warn" (the default),
#' warnings will notify the user that
#' non-numeric or out-of-range data are present. If set to "stop", the
#' warnings will be printed, but execution will stop. In this case, the user
#' will need to fix the input data in order to proceed.
#'
#' @return A dataframe of output containing SI-29 scores, and any variables
#' requested in transfer_vars.
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
                             warn_or_stop = c("warn", "stop")) {

  warn_or_stop <- match.arg(warn_or_stop)

  si_29_names <- lurn_si_29_names()

  # Check the input for errors
  check_args_score_lurn_si_29(input = input,
                              transfer_vars = transfer_vars,
                              warn_or_stop = warn_or_stop)

  # Check correspondence of gender and gender-specific questions
  # Save vectors of 27a, 27b, and gender variable
  lurn_si_27a_women <- input[[si_29_names[27]]]
  lurn_si_27b_men <- input[[si_29_names[28]]]
  gender <- input[["Gender"]]

  gender_levels <- c(female = 1,
                     male = 2)

  check_si_29_gender_questions(lurn_si_27a_women,
                               lurn_si_27b_men,
                               gender,
                               gender_levels)

  # Check for numeric and out-of-range questions across all 29 items
  check_si_29_vars(si_29_items = input[si_29_names],
                   item_ranges = lurn_si_29_item_ranges(include_na = TRUE),
                   warn_or_stop = warn_or_stop)

    # Recode non-numeric to NA
  si_29_items <- suppressWarnings(
    sapply(input[si_29_names], as.numeric))

  si_29_items <- as.data.frame(si_29_items)

  lurn_si_29_item_ranges <- lurn_si_29_item_ranges()

  for(i in seq_along(colnames(si_29_items))) {
    item <- si_29_items[[i]]
    item[!item %in% lurn_si_29_item_ranges[[i]]] <- NA
    si_29_items[[i]] <- item
  }

  # Is this needed?
  # si_29_items <- as.data.frame(si_29_items)

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
  # Programming note - These could be made conditional on whether they are
  # present in returned_vars above, but let's assume this function is only
  # for returning all of the SI-29 subscales along with the total score

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
  SI29_Q27 <- vector("numeric", nrow(si_29_items))
  SI29_Q27[] <- NA

  # Recode to NA if gender (or "Gender" in the input) is not 1 or 2
  gender[!gender %in% gender_levels] <- NA

  for(i in seq_len(nrow(si_29_items))){
    if(gender[i] == gender_levels[1]) {
      SI29_Q27[i] <- lurn_si_27a_women[i]
    } else if(gender[i] == gender_levels[2]) {
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
           "Missing total or subscale scores due to half or more items missing among all questions or among questions in that subscale",
           NA)

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

  output <- cbind(
    input[transfer_vars],
    lurn_si_29_all_output[returned_vars])

}
