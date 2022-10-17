lurn_si_10_names_from_29 <- function(include_bother = TRUE) {

  from_29 <- c(
    SI10_Q1 =     "SI29_Q16",
    SI10_Q2 =     "SI29_Q2",
    SI10_Q3 =     "SI29_Q3",
    SI10_Q4 =     "SI29_Q4",
    SI10_Q5 =     "SI29_Q7",
    SI10_Q6 =     "SI29_Q12",
    SI10_Q7 =     "SI29_Q14",
    SI10_Q8 =     "SI29_Q26",
    SI10_Q9 =     "SI29_Q21",
    SI10_Q10 =    "SI29_Q19",
    SI10_Bother = "SI29_Q28")

  if(include_bother) {
    from_29
  } else {
    from_29[1:10]
  }

}
