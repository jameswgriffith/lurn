stop_or_warn_message <- function(problem_vars_factors,
                                 problem_vars_non_factor_non_num,
                                 problem_vars_num_out_of_rng,
                                 q_name = "questionnaire",
                                 warn_or_stop) {

  # Three problem types
  if (length(problem_vars_factors) > 0 &&
      length(problem_vars_non_factor_non_num) > 0 &&
      length(problem_vars_num_out_of_rng) > 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items have ",
         "factors, out-of-range values, and non-numeric data. ",
         "Please use numeric input and try again.\n\n",
         "The following variables are factors:\n\n",
         paste(problem_vars_factors, collapse = " "),
         "\n\nThe following variables are not numeric:\n\n",
         paste(problem_vars_non_factor_non_num, collapse = " "),
         "\n\nThe following variables contain out-of-range values:\n\n",
         paste(problem_vars_num_out_of_rng, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) > 0 &&
             length(problem_vars_non_factor_non_num) > 0 &&
             length(problem_vars_num_out_of_rng) > 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items have ",
            "factors, out-of-range values, and non-numeric data.",
            "We will coerce data to numeric as appropriate, but ",
            "we recommend that you check your scores and results ",
            "very carefully.",
            "You may wish to check your data and try again.\n\n",
            "The following variables are factors:\n\n",
            paste(problem_vars_factors, collapse = " "),
            "\n\nThe following variables are not numeric:\n\n",
            paste(problem_vars_non_factor_non_num, collapse = " "),
            "\n\nThe following variables contain out-of-range values:\n\n",
            paste(problem_vars_num_out_of_rng, collapse = " "),
            call. = FALSE,
            immediate. = TRUE)
  }

  # Two problem types
  if (length(problem_vars_factors) > 0 &&
      length(problem_vars_non_factor_non_num) == 0 &&
      length(problem_vars_num_out_of_rng) > 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items have ",
         "both factors and out-of-range values.",
         "Please use numeric input and try again.\n\n",
         "The following variables are factors:\n\n",
         paste(problem_vars_factors, collapse = " "),
         "\n\nThe following variables contain out-of-range values:\n\n",
         paste(problem_vars_num_out_of_rng, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) > 0 &&
             length(problem_vars_non_factor_non_num) == 0 &&
             length(problem_vars_num_out_of_rng) > 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items have ",
            "both factors and out-of-range values. ",
            "We have coerced these data to numeric type.",
            "If this was unexpected, we suggest you re-check your input ",
            "very carefully and try again.",
            "We recommend using only numeric input, so please check your ",
            "input, scores, and other results very carefully.\n\n",
            "The following variables are factors:\n\n",
            paste(problem_vars_factors, collapse = " "),
            "\n\nThe following variables contain out-of-range values:\n\n",
            paste(problem_vars_num_out_of_rng, collapse = " "),
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_factors) > 0 &&
      length(problem_vars_non_factor_non_num) > 0 &&
      length(problem_vars_num_out_of_rng) == 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items have ",
         "both factors and other non-numeric values.",
         "Please use numeric input and try again.\n\n",
         "The following variables are factors:\n\n",
         paste(problem_vars_factors, collapse = " "),
         "\n\nThe following variables contain non-numeric values:\n\n",
         paste(problem_vars_non_factor_non_num, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) > 0 &&
             length(problem_vars_non_factor_non_num) > 0 &&
             length(problem_vars_num_out_of_rng) == 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items have ",
            "both factors and other non-numeric values.",
            "Please check your input, scores, and results carefully. ",
            "\n\n",
            "The following variables are factors:\n\n",
            paste(problem_vars_factors, collapse = " "),
            "\n\nThe following variables contain non-numeric values:\n\n",
            paste(problem_vars_non_factor_non_num, collapse = " "),
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_factors) == 0 &&
      length(problem_vars_non_factor_non_num) > 0 &&
      length(problem_vars_num_out_of_rng) > 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items have ",
         "both non-numeric and out-of-range values. ",
         "Please use numeric input and try again.\n\n",
         "The following variables contain non-numeric values:\n\n",
         paste(problem_vars_non_factor_non_num, collapse = " "),
         "\n\nThe following variables contain out-of-range values:\n\n",
         paste(problem_vars_num_out_of_rng, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) == 0 &&
             length(problem_vars_non_factor_non_num) > 0 &&
             length(problem_vars_num_out_of_rng) > 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items have ",
            "both non-numeric and out-of-range values. ",
            "We recommend using only numeric input. ",
            "You may wish to check your input and try again. ",
            "For the moment, any non-numeric and out-of-range values ",
            "will be recoded to NA.\n\n",
            "The following variables contain non-numeric values:\n\n",
            paste(problem_vars_non_factor_non_num, collapse = " "),
            "\n\nThe following variables contain out-of-range values:\n\n",
            paste(problem_vars_num_out_of_rng, collapse = " "),
            call. = FALSE,
            immediate. = TRUE)
  }

  # One problem
  if (length(problem_vars_factors) > 0 &&
      length(problem_vars_non_factor_non_num) == 0 &&
      length(problem_vars_num_out_of_rng) == 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items contain ",
         "factor variables. The power and utility of factors ",
         "notwithstanding, you must use numeric data with this package.\n\n",
         "Please use numeric input and try again.\n\n",
         "The following variables are factors:\n\n",
         paste(problem_vars_factors, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) > 0 &&
             length(problem_vars_non_factor_non_num) == 0 &&
             length(problem_vars_num_out_of_rng) == 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items contain ",
            "factor variables. The power and utility of factors ",
            "notwithstanding, ",
            "we reccomend using numeric data with this package.\n\n",
            "Please check your input, scores, and results very carefully.",
            "\n\n",
            "The following variables are factors:\n\n",
            paste(problem_vars_factors, collapse = " "),
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_factors) == 0 &&
      length(problem_vars_non_factor_non_num) == 0 &&
      length(problem_vars_num_out_of_rng) > 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items contain ",
         "out-of-range values. Please check your input and try again.\n\n",
         "The following variables contain out-of-range values:\n\n",
         paste(problem_vars_num_out_of_rng, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) == 0 &&
             length(problem_vars_non_factor_non_num) == 0 &&
             length(problem_vars_num_out_of_rng) > 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items contain ",
            "out-of-range values, which have been coerced to NA. ",
            "If this was not expected, ",
            "Please check your input very carefully and try again. ",
            "Otherwise, please check your input, scores, and results ",
            "very carefully.\n\n",
            "The following variables contain out-of-range values:\n\n",
            paste(problem_vars_num_out_of_rng, collapse = " "),
            call. = FALSE,
            immediate. = TRUE)
  }

  if (length(problem_vars_factors) == 0 &&
      length(problem_vars_non_factor_non_num) > 0 &&
      length(problem_vars_num_out_of_rng) == 0 &&
      warn_or_stop == "stop") {
    stop("Your variables for the ", q_name, " items contain ",
         "some non-numeric values. Please check your input",
         "use only numeric input, and try again.\n\n",
         "The following variables contain non-numeric values:\n\n",
         paste(problem_vars_non_factor_non_num, collapse = " "),
         call. = FALSE)
  } else if (length(problem_vars_factors) == 0 &&
             length(problem_vars_non_factor_non_num) > 0 &&
             length(problem_vars_num_out_of_rng) == 0 &&
             warn_or_stop == "warn") {
    warning("Your variables for the ", q_name, " items contain ",
            "some non-numeric values. If this was not expected, ",
            "please check your input, scores, and results very carefully. ",
            "We recommend using only numeric input with this package.\n\n",
            "The following variables contain non-numeric values:\n\n",
            paste(problem_vars_non_factor_non_num, collapse = " "),
            call. = FALSE)
  }

  invisible(NULL)

}
