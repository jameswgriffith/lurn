#' A helper function that returns the recommended variable names of the
#' LURN SI-10
#'
#' @description This function returns a character vector of recommended
#' variable names for the LURN SI-10. Case matters.
#'
#' @param include_bother_item This is TRUE by default. For the SI-10,
#' only the 10 symptom questions are scored, so setting this parameter to
#' FALSE will return only the first 10 item names for symptoms,
#' and not the item about bother.
#'
#' @return A character vector of recommended variable names for the LURN SI-10.
#' @export
#'
#' @examples
#' \dontrun{
#' lurn_si_10_names()
#' lurn_si_10_names(include_bother_item = FALSE)
#' }
lurn_si_10_names <- function(include_bother_item = TRUE) {

  # All variable names
  si_10_all <- c(paste0("SI10_Q", 1:10), "SI10_BOTHER")

  if (include_bother_item) {
    si_10_all
  } else {
    si_10_all[1:10]
  }
}
