# Check the arguments of the score_lurn_si_10 -----------------------------

check_args_score_lurn_si_10 <- function(
    input,
    si_10_names,
    returned_vars,
    rename_returned_vars_to = NULL,
    transfer_vars,
    warn_or_stop) {

  if(length(si_10_names) != 10) {
    stop("There should 10 variable names in si_10_names.\nPlease try again.")
  }

  if(!all(si_10_names %in% names(input))) {
    stop(c("\n\nThe names in si_10_names must be found in the input.\n",
           "We recommend that the ten scored items of the LURN SI-10 are labelled\n\n",
           paste0("SI10_Q", 1:10, " "),
           "\n\nPlease try again."))
  }

  returned_vars_choices <- c(
    "lurn_si_10_score",
    "lurn_si_10_count_valid",
    "lurn_si_10_floor",
    "lurn_si_10_ceiling",
    "lurn_si_10_note",
    "all")

  if(!any(returned_vars %in% returned_vars_choices)) {
    stop("return_vars must contain one or more of the following options:\n",
         paste(returned_vars_choices, collapse = "\n"))
  }

  if(!all(transfer_vars %in% names(input))) {
    stop(c("\n\nWe can only return the score of the SI-10 and variables found in the input.\n",
           "Please try again, choosing only variable names found in input."))
  }

  if(!is.null(rename_returned_vars_to) &&
     length(returned_vars) != length(rename_returned_vars_to)) {
    stop("If you wish to rename returned_vars, please supply a vector containing ",
         length(returned_vars),
         " variable names and try again.")
  }

  if(any(returned_vars %in% names(input)) && is.null(rename_returned_vars_to)) {
    stop(c("Varible names in returned_vars are already in the names of input.\n",
           "If desired, please use rename_returned_vars_to to change the names",
           " of the output variables."))
  }

  if(!is.null(rename_returned_vars_to) &&
     any(rename_returned_vars_to %in% names(input))) {
    stop(c("Varible names in rename_returned_vars_to are already in the names of input.\n",
           "Please check your variable names and try again."))
  }

}


# Check each SI-10 variable for out-of-range and/or non-numeric data --------
check_si_10_items <- function(si_10_names,
                              input,
                              warn_or_stop) {

  # Initialise empty vector for variable names with problems
  problem_vars_non_num <- vector(mode = "character")
  problem_vars_out_of_rng <- vector(mode = "character")

  for(i in seq_along(si_10_names)) {

    if(i < 9) {
      item_max <- 4
    } else if (i >= 9) {
      item_max <- 3
    }

    item_range <- c(seq(0, item_max), NA)

    item_name <- si_10_names[i]
    col_to_chk <- input[si_10_names[i]]

    if(!all(col_to_chk %in% item_range)) {
      problem_vars_out_of_rng <- append(problem_vars_out_of_rng, item_name)
    }

    if(!is.numeric(col_to_chk)) {
      problem_vars_non_num <- append(problem_vars_non_num, item_name)
    }
  }

  # Print messages
  if(length(problem_vars_out_of_rng) > 0) {
    warning("The following variables contain some out-of-range values:\n",
            paste(problem_vars_out_of_rng, collapse = "\n"))
  }

  if(length(problem_vars_non_num) > 0) {
    warning("The following variables contain some non-numeric values:\n",
            paste(problem_vars_non_num, collapse = "\n"))
  }

  if((length(problem_vars_out_of_rng) > 0 ||
      length(problem_vars_non_num) > 0) &&
     warn_or_stop == "stop") {
    stop("Please fix your input and try again.")
  }
}


# Convert out of range to NA ----------------------------------------------

### NEED TO FIX

out_of_rng_to_na <- function(items, valid_range) {

  items <- as.matrix(items)

  # Check for errors
  # if(!is.matrix(items)) {
  #     stop("The parameter \'items\' must be a matrix.")
  # }

  apply(items,
        c(1, 2),
        function(response) {
          if(response %in% valid_range) {
            recoded <- response
          } else {
            recoded <- NA
          }
          return(recoded)
        })
}


# Find maximum possible score based on items completed --------------------

max_possible_si_10 <- function(si_10_items) {

  # Check for errors in the input
  if(!is.vector(si_10_items)) {
    stop("si_10_items must be a vector. Please try again")
  }

  max_item_responses <-
    c(SI10_Q1 = 4,
      SI10_Q2 = 4,
      SI10_Q3 = 4,
      SI10_Q4 = 4,
      SI10_Q5 = 4,
      SI10_Q6 = 4,
      SI10_Q7 = 4,
      SI10_Q8 = 4,
      SI10_Q9 = 3,
      SI10_Q10 = 3)

  (!is.na(si_10_items)) %*% max_item_responses

}


# Count non-NA values row-by-row ------------------------------------------

count_not_na <- function(items) {
  apply(items,
        1,
        function(x) {sum(!is.na(x))})
}

# Count NA values row-by-row ------------------------------------------

count_na <- function(items) {
  apply(items,
        1,
        function(x) {sum(is.na(x))})
}

# data --------------------------------------------------------------------

#' Synthetic data for testing, based on format from LURN I
#'
#' @format A dataframe with 13 variables and 7 observations
"lurn_si_10_test_data"


# Check arguments ---------------------------------------------------------

check_args_score_lurn_si_29 <- function(input,
                                        si_29_names,
                                        gender,
                                        gender_levels,
                                        returned_vars,
                                        rename_returned_vars_to,
                                        transfer_vars,
                                        warn_or_stop) {

  # Are there 29 SI-29 items in the input
  if(length(si_29_names) != 29) {
    stop("There should 29 variable names in si_29_names.\nPlease try again.")
  }

  # Is the name of the gender variable found in the input?
  if(!gender %in% names(input)) {
    stop("The variable for gender is not found among ",
         "the variable names of the input.\n",
         "Please check your input and try again.")
  }

  # Are the levels of the gender variable correct?
  gender <- input[[gender]]

  if(any(!gender %in% gender_levels)) {
    warning("The variable for gender contains values ",
            "not found in gender_levels.\n",
            "We recommend coding gender as follows:\n",
            "1 = female\n",
            "2 = male\n",
            "NA = missing and/or other values\n\n",
            "Any missing values for gender will result in missing LURN SI-29 scores.")
  }

  # Are all of the items in the LURN SI-29 found in the input
  if(!all(si_29_names %in% names(input))) {
    stop(c("\n\nThe names in si_29_names must be found in the input.\n",
           "We recommend that the 29 scored items of the LURN SI-29 are labelled\n\n",
           lurn_si_29_names(),
           "\n\nPlease try again."))
  }

  ### Check returned_vars_choices
  returned_vars_choices <-
    c("lurn_si_29_total",
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

  if(!any(returned_vars %in% returned_vars_choices)) {
    stop("return_vars must contain one or more of the following options:\n",
         paste(returned_vars_choices, collapse = "\n"))
  }

  if(!all(transfer_vars %in% names(input))) {
    stop("\n\nWe can only return the scores of the LURN SI-29 and variables",
         "found in the input. \nPlease try again, choosing only variable names ",
         "found in the input.")
  }

  if(!is.null(rename_returned_vars_to) &&
     length(returned_vars) != length(rename_returned_vars_to)) {
    stop("If you wish to rename returned_vars, please supply a vector containing ",
         length(returned_vars),
         " variable names and try again.")
  }

  if(any(returned_vars %in% names(input)) && is.null(rename_returned_vars_to)) {
    stop("Varible names in returned_vars are already in the names of input.\n",
         "If desired, please use rename_returned_vars_to to change the names",
         " of the output variables. \nPlease try again.")
  }

  if(!is.null(rename_returned_vars_to) &&
     any(rename_returned_vars_to %in% names(input))) {
    stop("Varible names in rename_returned_vars_to are already in the names of input.\n",
         "Please check your variable names and try again.")
  }

}


# Check LURN SI-29 gender questions ---------------------------------------

check_si_29_gender_questions <- function(lurn_si_27a_women,
                                         lurn_si_27b_men,
                                         gender,
                                         gender_levels) {

  for(i in seq_along(gender)) {

    # Check female participants
    if(gender[i] == gender_levels[1] && lurn_si_27b_men[i] %in% 0:4) {
      stop("Item #27b of the LURN SI-29 is for men only, but female ",
           "participants have data (0-4) for this item. \nPlease check your ",
           "input and try again.")
    }

    # Check male participants
    if(gender[i] == gender_levels[2] && lurn_si_27a_women[i] %in% 0:4) {
      stop("Item #27a of the LURN SI-29 is for women only, but male ",
           "participants have data (0-4) for this item. Please check your ",
           "input and try again.")
    }

    # Check if data exist in both 27a and 27b
    if(lurn_si_27b_men[i] %in% 0:4 && lurn_si_27a_women[i] %in% 0:4) {
      stop("There are data (0-4) in both Item #27a and #27b of the ",
           "LURN SI-29.\nThis should never be the case. \nPlease check ",
           "your input and try again.")
    }
  }
}


# List of item ranges -----------------------------------------------------

lurn_si_29_item_ranges <-function(item_names = NULL,
                                  include_na = TRUE) {
  item_ranges <- list(
  SI29_Q1 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q2 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q3 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q4 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q5 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q6 = c(Never = 0,
              `A few nights` = 1,
              `About half the nights` = 2,
              `Most nights` = 3,
              `Every night` = 4),
  SI29_Q7 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q8 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q9 = c(Never = 0,
              `A few times` = 1,
              `About half the time` = 2,
              `Most of the time` = 3,
              `Every time` = 4),
  SI29_Q10 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q11 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q12 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q13 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q14 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q15 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q16 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q17 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q18 = c(`Not difficult` = 0,
               `A little difficult` = 1,
               `Somewhat difficult` = 2,
               `Very difficult` = 3,
               `Unable to wait` = 4),
  SI29_Q19 = c(None = 0,
               `1 time` = 1,
               `2-3 times` = 2,
               `More than 3 times` = 3),
  SI29_Q20 = c(Never = 0,
               `A few nights` = 1,
               `About half the nights` = 2,
               `Most nights` = 3,
               `Every night` = 4),
  SI29_Q21 = c(`3 or fewer times a day` = 0,
               `4-7 times a day` = 1,
               `8-10 times a day` = 2,
               `11 or more times a day` = 3),
  SI29_Q22 = c(`More than 6 hours` = 0,
               `5-6 hours` = 1,
               `3-4 hours` = 2,
               `1-2 hours` = 3,
               `Less than 1 hour` = 4),
  SI29_Q23 = c(`No urge` = 0,
               `Mild urge` = 1,
               `Moderate urge` = 2,
               `Strong urge` = 3),
  SI29_Q24 = c(Yes = 1,
               No = 0),
  SI29_Q25 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q26 = c(Never = 0,
               `A few times` = 1,
               `About half the time` = 2,
               `Most of the time` = 3,
               `Every time` = 4),
  SI29_Q27a = c(Never = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
  SI29_Q27b = c(Never = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
  SI29_Q28 = c(`Not at all bothered` = 0,
               `Somewhat bothered` = 1,
               `Very bothered` = 2,
               `Extremely bothered` = 3))

  if(!is.null(item_names) && length(item_names) != length(item_ranges)) {
    stop("The length of item_names must match the number of item ranges to be returned.")
  }

  if(!is.null(item_names)) {
    names(item_ranges) <- item_names
  }

  if(include_na) {
    item_ranges <- lapply(item_ranges,
                          append,
                          values = NA)
  }

  return(item_ranges)

}

# Check the items within SI-29 subscales ----------------------------------

check_si_29_vars <- function(si_29_items,
                             item_ranges = lurn_si_29_item_ranges(),
                             warn_or_stop = c("warn", "stop")) {

  warn_or_stop <- match.arg(warn_or_stop, c("warn", "stop"))

  # Initialise empty vector for variable names with problems
  problem_vars_non_num <- vector(mode = "character")
  problem_vars_out_of_rng <- vector(mode = "character")

  item_names_to_check <- names(si_29_items)

  for(i in seq_along(item_names_to_check)) {

    item_name <- item_names_to_check[i]
    col_to_chk <- item_names_to_check[item_name]

    item_range <- item_ranges[i]

    if(!all(col_to_chk %in% item_range)) {
      problem_vars_out_of_rng <- append(problem_vars_out_of_rng, item_name)
    }

    if(!is.numeric(col_to_chk)) {
      problem_vars_non_num <- append(problem_vars_non_num, item_name)
    }
  }

  # Print messages
  if(length(problem_vars_out_of_rng) > 0) {
    warning("The following variables contain some out-of-range values:\n",
            paste(problem_vars_out_of_rng, collapse = "\n"))
  }

  if(length(problem_vars_non_num) > 0) {
    warning("The following variables contain some non-numeric values:\n",
            paste(problem_vars_non_num, collapse = "\n"))
  }

  if((length(problem_vars_out_of_rng) > 0 ||
      length(problem_vars_non_num) > 0) &&
     warn_or_stop == "stop") {
    stop("Please fix your input and try again.")
  }
}

# Find maximum possible score based on items completed --------------------

max_possible <- function(items, max_possible_responses) {

  (!is.na(items)) %*% max_possible_responses

}
