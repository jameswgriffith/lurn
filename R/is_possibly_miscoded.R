# Check for mis-coded items

# 1. Read in data for 10/29 and item ranges
# 2. Check to see if minimum is 1 and maximum is top-of-range+1
# 3. If #2 is TRUE, return a message to the user

is_item_possibly_miscoded <- function(item, correct_range) {

  min_correct_resp <- correct_range[1]
  max_plus_one <- max(correct_range) + 1


  if(all(is.na(item))) {
    FALSE
  } else if (!min_correct_resp %in% item &&
             max_plus_one %in% item) {
    TRUE
  } else {
    FALSE
  }
}
