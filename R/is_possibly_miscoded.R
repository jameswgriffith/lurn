is_item_possibly_miscoded <- function(item, correct_range) {

  min_correct_resp <- correct_range[1]
  max_plus_one <- max(correct_range) + 1


  if (all(is.na(item))) {
    FALSE
  } else if (!min_correct_resp %in% item &&
             max_plus_one %in% item) {
    TRUE
  } else {
    FALSE
  }
}
