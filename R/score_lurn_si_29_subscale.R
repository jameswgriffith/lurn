score_lurn_si_29_subscale <- function(items,
                                      max_possible_responses) {

  n_items <- length(max_possible_responses)
  missing_50_percent <- length(max_possible_responses) / 2

  # Check for errors in the input
  if(length(items) != n_items) {
    stop("There should be ",
         n_items,
         " items in the subscale.",
         "\nPlease check your input and try again.")
  }
  # Done checking errors

  # Convert vector to one-row dataframe, if needed
  if(is.vector(items)) {
    items <- as.data.frame(t(items))
  }

  count_na_items <- count_na(items)

  item_sum <- apply(items,
                    1,
                    function(x) sum(x, na.rm = TRUE))

  max_possible_scores <- max_possible(items,
                                      max_possible_responses)

  # Return output
  ifelse(count_na_items < missing_50_percent,
         item_sum / max_possible_scores * 100,
         NA)
}
