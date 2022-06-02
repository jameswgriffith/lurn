max_possible_si_10 <- function(si_10_items) {

  # Check for errors in the input
  if (!is.vector(si_10_items)) {
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
