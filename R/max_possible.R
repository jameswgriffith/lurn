# Find maximum possible score based on items completed --------------------

max_possible <- function(items, max_possible_responses) {

  (!is.na(items)) %*% max_possible_responses

}
