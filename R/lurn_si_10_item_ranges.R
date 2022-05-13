# Returns a list of item ranges for the LURN SI-10
lurn_si_10_item_ranges <-function(item_names = NULL,
                                  include_na = FALSE) {
  item_ranges <- list(
    SI10_Q1 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q2 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q3 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q4 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q5 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q6 = c(`Never` = 0,
                `A few nights` = 1,
                `About half the nights` = 2,
                `Most nights` = 3,
                `Every night` = 4),
    SI10_Q7 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q8 = c(`Never` = 0,
                `A few times` = 1,
                `About half the time` = 2,
                `Most of the time` = 3,
                `Every time` = 4),
    SI10_Q9 = c(`3 or fewer times a day` = 0,
                `4-7 times a day` = 1,
                `8-10 times a day` = 2,
                `11 or more times a day` = 3),
    SI10_Q10 = c(`none` = 0,
                `1 time` = 1,
                `2-3 times` = 2,
                `More than 3 times` = 3),
    SI10_BOTHER = c(`Not at all bothered` = 0,
                 `Somewhat bothered` = 1,
                 `Very bothered` = 2,
                 `Extremely bothered` = 3))

  if(!is.null(item_names) && length(item_names) != length(item_ranges)) {
    stop(c("The length of item_names must match the number of item ",
    "ranges to be returned.\n",
    "Please try again."))
  }

  if(!is.null(item_names)) {
    names(item_ranges) <- item_names
  }

  if(include_na) {
    item_ranges <- lapply(item_ranges,
                          append,
                          values = NA)
  } else {
    item_ranges
    }
}
