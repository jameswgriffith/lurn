check_si_29_sex_questions <- function(lurn_si_27a_women,
                                      lurn_si_27b_men,
                                      sex,
                                      sex_levels = c(male = 1,
                                                     female = 2)) {

  for (i in seq_along(sex)) {

    # Check if data exist in both 27a and 27b
    if (lurn_si_27b_men[i] && lurn_si_27b_men[i] %in% 0:4 &&
        lurn_si_27a_women[i] %in% 0:4) {
      stop("There are data (0-4) in both Item #27a and #27b of the ",
           "LURN SI-29.\nThis should never be the case. \nPlease check ",
           "your input and try again.",
           call. = FALSE)
    }

    # Check female participants
    if (!is.na(sex[i]) && sex[i] == sex_levels[2] &&
        lurn_si_27b_men[i] %in% 0:4) {
      stop("Item #27b of the LURN SI-29 is for men only, but your female ",
           "participants have data (0-4) for this item. \nPlease check your ",
           "input and try again.",
           call. = FALSE)
    }

    # Check male participants
    if (!is.na(sex[i]) && sex[i] == sex_levels[1] &&
        lurn_si_27a_women[i] %in% 0:4) {
      stop("Item #27a of the LURN SI-29 is for women only, but your male ",
           "participants have data (0-4) for this item. Please check your ",
           "input and try again.",
           call. = FALSE)
    }

  }

  invisible(NULL)

}
