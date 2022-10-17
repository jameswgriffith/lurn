#' A helper function that returns the recommended variable names of the
#' LURN SI-29
#'
#' @description This function returns a character vector of recommended
#' variable names for the LURN SI-29. The user can `include` the complete
#' list of `all` variable names, or only those variable names specific to
#' `male` or `female` participants.
#'
#' @param include By default, `all` variable names of the LURN SI-29 are
#' returned. If desired, the user can request only variable names specific
#' to `female` respondents or `male` respondents. If needed, the argument
#' names will be converted to lowercase (i.e., `ALL`, `All`, etc.,
#' will be silently converted to `all`).
#'
#' @return A character vector of recommended variable names for the LURN SI-29.
#' @export
#'
#' @examples
#' \dontrun{
#' lurn_si_29_names()
#'
#' # Only female variable names
#' lurn_si_29_names(include = "female")
#'
#' #' Only male variable names
#' lurn_si_29_names(include = "male")
#' }
lurn_si_29_names <- function(
    include = c(
      "all",
      "female",
      "male")) {

  include <- match.arg(tolower(include),
                       c("all",
                         "female",
                         "male"))

  # All variable names
  si_29_all <- c(paste0("SI29_Q", 1:26),
                 "SI29_Q27a",
                 "SI29_Q27b",
                 "SI29_Q28")

  # Female only
  si_29_female <- c(paste0("SI29_Q", 1:26),
                    "SI29_Q27a",
                    "SI29_Q28")

  # Male only
  si_29_male <- c(paste0("SI29_Q", 1:26),
                  "SI29_Q27b",
                  "SI29_Q28")

  # Return output
  switch(include,
         all = si_29_all,
         female = si_29_female,
         male = si_29_male)
}
