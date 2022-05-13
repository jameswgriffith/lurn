# Check LURN SI-29 gender questions ---------------------------------------

check_si_29_gender_questions <- function(lurn_si_27a_women,
                                         lurn_si_27b_men,
                                         gender,
                                         gender_levels = c(female = 1,
                                                           male = 2)) {

  for(i in seq_along(gender)) {

    # Check female participants
    if(gender[i] == gender_levels[1] && lurn_si_27b_men[i] %in% 0:4) {
      stop("Item #27b of the LURN SI-29 is for men only, but female ",
           "participants have data (0-4) for this item. \nPlease check your ",
           "input and try again.",
           call. = FALSE)
    }

    # Check male participants
    if(gender[i] == gender_levels[2] && lurn_si_27a_women[i] %in% 0:4) {
      stop("Item #27a of the LURN SI-29 is for women only, but male ",
           "participants have data (0-4) for this item. Please check your ",
           "input and try again.",
           call. = FALSE)
    }

    # Check if data exist in both 27a and 27b
    if(lurn_si_27b_men[i] %in% 0:4 && lurn_si_27a_women[i] %in% 0:4) {
      stop("There are data (0-4) in both Item #27a and #27b of the ",
           "LURN SI-29.\nThis should never be the case. \nPlease check ",
           "your input and try again.",
           call. = FALSE)
    }
  }
}
