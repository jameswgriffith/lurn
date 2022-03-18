# Check the arguments of the score_lurn_si_10 -----------------------------

check_args_score_lurn_si_10 <- function(
    input,
    si_10_names,
    returned_vars,
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
}


# Check each variable for out-of-range and/or non-numeric data ------------
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

    item_range = c(seq(0, item_max), NA)

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
  if(length(problem_vars_out_of_rng) > 1) {
    warning("The following variables contain some out-of-range values:\n",
            paste(problem_vars_out_of_rng, collapse = "\n"))
  }

  if(length(problem_vars_non_num) > 1) {
    warning("The following variables contain some non-numeric values:\n",
            paste(problem_vars_non_num, collapse = "\n"))
  }

  if((length(problem_vars_out_of_rng) > 1 ||
      length(problem_vars_non_num) > 1) &&
     warn_or_stop == "stop") {
    stop("Please fix your input and try again.")
  }
}


# Convert out of range to NA ----------------------------------------------

out_of_rng_to_na <- function(items, valid_range) {
  # Check for errors
  if(!is.matrix(items)) {
    stop("The parameter \'items\' must be a matrix.")
  }

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


# data --------------------------------------------------------------------

#' Synthetic data for testing, based on format from LURN I
#'
#' @format A dataframe with 13 variables and 7 observations
"lurn_si_10_test_data"

